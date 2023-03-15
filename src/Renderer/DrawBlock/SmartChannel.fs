module SmartChannel
open BusWireUpdate
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

// Authored by Harshil Shah

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart channel route" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires in the BusWire model so could use the SmartHelper function for
    this purpose.
*)


/// HLP23: suggested initial smartChannel top-level function
/// to be tested, it must be given a channel in through which to route wires nicely
/// Normally the channel will come from symbol edges.
/// The function must identify all wires with segments going through the channel and space them
/// This function routes the middle segment of all 7 segment wires by moving them perpendicular to its direction.
/// It is expected that all wires provided are 7 segment with middle segment in the same direction
/// wires not so will be ignored.
/// The messiness of when to call it, what to do with different types of wire, which wires to call it with
/// could be left till later.
/// For this simple routing only one dimension of the channel is needed (determined by orientation).
/// The Wires going through the channel must be returned as an updated Wires map in model.
// HLP23: need to update XML doc comments when this function is fully worked out.

let wireInChannelPredicate channel (connId,wire) :bool = 
    //7 seg wires allowed ONLY
    //9 seg wires allowed in vertical channel ONLY
    if not (List.length wire.Segments = 7)// && channelOrientation = Vertical 
            //|| List.length wire.Segments = 9 && channelOrientation = Horizontal ) then 
        then false
    else
        match wireIntersectsBoundingBox wire channel with
        | None -> false
        | Some x -> true

let wireOrientationPredicate (cid,wire) = 
    let thirdSegOrientation = 
        getAbsoluteSegmentPos wire 3
        |> (fun (st,en) -> getSegmentOrientation st en)
    match thirdSegOrientation with
    | Vertical -> true
    | Horizontal -> false

///Top level function for auto-spacing wires in a bounding box
let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    let tl = channel.TopLeft
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"

    let allWiresList = 
        model.Wires
        |> Map.toList
        |> List.partition (wireInChannelPredicate channel)

    //List of wires in channel sorted by X coord of starting pos
    let channelWiresList = 
        fst allWiresList
        |> List.sortBy (fun (x,y) -> y.StartPos.Y)
    List.map (fun (x,y) -> printf $"{y.Segments.Length}, {getAbsoluteSegmentPos y 3}") channelWiresList |> ignore

    //Further segment channelWiresList into vertical and horizontal wires
    let orientedWiresList = 
        channelWiresList
        |> List.partition wireOrientationPredicate
    //let shiftWireList orientation
    
    //Calculates current deviation of each wire's mid segment from desired position, then calls moveSegment to correct it
    let xShiftedWiresList =

        //Setup desired wire positions
        let wireSpacing = 0.7*channel.W/(float channelWiresList.Length)
        let spacedWirePos = 
            [1..channelWiresList.Length]
            |> List.map (fun i -> tl.X  + float(i) * wireSpacing)


        fst orientedWiresList
        |> List.map (fun (cid,wire) -> getAbsoluteSegmentPos wire 3)
        |> List.map (fun x -> (fst x).X)
        |> List.map2 (fun autoPosX absPos  -> autoPosX - absPos) spacedWirePos //list of adjustments
        |> List.map2 (fun (cid,wire) adj -> (cid, moveSegment model wire.Segments[3] adj)) channelWiresList

    //let yShiftedWiresList = 
    //if a wire has a start point at at a certain y, then you want it to dip b4 a wire has an end point there
    //or if a wire has a seg2 at a certain y+- dy, then seg4 at that point must start after
    //switch positions of wires , keep making passes
    let correctedShiftedWiresList = 
        xShiftedWiresList
        //test if segments 2 or 4 overlap
        let overlapRange = 0.08 * channel.H
        let switchMidSeg (a,b) wires =
            wires
        //check if a wire overlaps with a list of other wires
        let compareOverlap w1 wireList = 
            //check if they overlap

            let w2 = snd wireList
            let w12 = fst (getAbsoluteSegmentPos w1 2)
            let w14 = fst (getAbsoluteSegmentPos w1 4)
            let w22 = fst (getAbsoluteSegmentPos w2 2)
            let w24 = fst (getAbsoluteSegmentPos w2 4)
            //If Y overlap
            //(None, wireList) |> List.fold folder
            if ((w24.Y < w12.Y + overlapRange  && w12.Y - overlapRange < w24.Y)
                || (w14.Y < w22.Y + overlapRange && w14.Y > w12.Y - overlapRange)) then
                //if X overlap
                if(w12.X) then
                    Some switchMidSeg
                else
                    None
            else
                None

        //ifSwitch would return an option of shifted wires
        let rec ifSwitch (wires: (ConnectionId*Wire) list option) = 
            //one pass of checking
            //stop checking after one switch
            match wires with
            | Some (hd::[]) -> Some (hd::[])
            | Some (hd::tl) -> 
                //if there is an overlap with selected elem and tail, return an option of it, else return None
                let pass = compareOverlap hd tl
                match pass with 
                | None -> ifSwitch tl //no switch? -> go deeper
                | Some x-> pass// we did switch? -> return

        //Low level checks if switch
        //if switch, then go deeper, and return that
        //else return switched
        let rec switchTopLevel (wires: (ConnectionId*Wire) list option) = 
            //take list of wires, return shifted list of wires
            match ifSwitch wires with
            | Some (x) -> switchTopLevel (Some (x))
            | None -> wires

        Some(xShiftedWiresList)
        |> switchTopLevel
        |> Option.defaultValue xShiftedWiresList
       //run the recursive switch function

            
    let allWiresMap = 
        correctedShiftedWiresList @ (snd allWiresList) 
        |> Map.ofList

    {model with Wires = allWiresMap}
    
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


    //if a wire has a start point at at a certain y, then you want it to dip b4 a wire has an end point there
    //or if a wire has a seg2 at a certain y+- dy, then seg4 at that point must start after
    //switch positions of wires , keep making passes
    let correctedShiftedWiresList = 
        //xShiftedWiresList
        //test if segments 2 or 4 overlap
        let overlapRange = 0.08 * channel.H

        let switchMidSeg (i1,i2) (wires:(ConnectionId*Wire) list) =
            let w1 = snd wires[i1]
            let w2 = snd wires[i2]
            //check wire orientation so you know whether shift in x or y direction
            let delta = (fst (getAbsoluteSegmentPos w2 3)).X - (fst (getAbsoluteSegmentPos w1 3)).X
            
            let movedWire1 = fst wires[i1], moveSegment model w1.Segments[3] delta             
            let movedWire2 = fst wires[i2], moveSegment model w2.Segments[3] -delta
            
            wires
            |> List.updateAt i1 movedWire1
            |> List.updateAt i2 movedWire2

        let rec compareOverlap wire1 wire2 :bool= 
            let w1 = snd wire1
            let w2 = snd wire2

            if (fst (getAbsoluteSegmentPos w2 3)).X > (fst (getAbsoluteSegmentPos w1 3)).X then
                compareOverlap wire2 wire1
            else
                let w12 = fst (getAbsoluteSegmentPos w1 2)
                let w14 = fst (getAbsoluteSegmentPos w1 4)
                let w22 = fst (getAbsoluteSegmentPos w2 2)
                let w24 = fst (getAbsoluteSegmentPos w2 4)

                //if Y overlap
                if ((w24.Y < w12.Y + overlapRange  && w12.Y - overlapRange < w24.Y)
                    || (w14.Y < w22.Y + overlapRange && w14.Y > w12.Y - overlapRange)) then
                    //if X overlap
                    if(w12.X > w14.X || w12.X > w14.X) then
                        true
                    else
                        false
                else
                    false

        //Perform a single switch with given wire and wireList, if switch is needed
        let switchComparePass cidWire1 (wireList:(ConnectionId*Wire) list) :(ConnectionId*Wire) list option= 

            let overlapPredicate = compareOverlap cidWire1
            List.tryFindIndex overlapPredicate wireList
            |> function
            | None -> None
            | Some (i) -> Some(switchMidSeg (0,i) [cidWire1]@wireList)


        //return shifted list of wires
        let rec ifSwitch (wires: (ConnectionId*Wire) list option) = 
            //one full pass of checking and shifting

            match wires with
            | Some (hd::[]) -> wires
            | Some ([]) -> wires
            | Some (hd::tl) -> 
                //if there is an overlap with selected elem and tail, return an option of it, else return None
                let pass = switchComparePass hd tl

                match pass with 
                | None -> Some([hd] @ (Option.defaultValue tl (ifSwitch (Some(tl)) )    )) //no switch? -> go deeper and append result to current head
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
    
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
    let shiftWireList orien
    
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
        let overlapRange = 0.05 * channel.H

        let compareOverlap (w1, w2) = 
            //check if they overlap
            //let w1 = 
            let w12 = fst (getAbsoluteSegmentPos w1 2)
            let w14 = fst (getAbsoluteSegmentPos w1 4)
            let w22 = fst (getAbsoluteSegmentPos w2 2)
            let w24 = fst (getAbsoluteSegmentPos w2 4)
            if ((w24.Y < w12.Y + overlapRange  && w12.Y - overlapRange < w24.Y)
                || (w14.Y < w12.Y + overlapRange)) then
                true
            else
                false
                

        let rec comb n l = 
            match n, l with
            | 0, _ -> [[]]
            | _, [] -> []
            | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

        //let rec compareO l = 
        //    //take list of wires

        //    match l with
        //    | _ -> [[]]
        //    | [] -> []
        //    | hd::tl -> List.map compareOverlap 

        let mutable optimalSpacing = false
        let mutable iterCount = 0
        let mutable corrShiftedWiresList = xShiftedWiresList
        while(not optimalSpacing && iterCount < 10) do
            //Scan list for seg2 and seg4 overlaps
            iterCount <- iterCount + 1
            //corrShiftedWiresList <-
            //[0..((List.length xShiftedWiresList) - 1)]
            //|> comb 2 //combinations of indices
            //|> List.map2 compareOverlap 

        corrShiftedWiresList

            
    let allWiresMap = 
        correctedShiftedWiresList @ (snd allWiresList) 
        |> Map.ofList

    {model with Wires = allWiresMap}
    
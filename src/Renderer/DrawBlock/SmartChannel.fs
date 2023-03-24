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


///Top level function for auto-spacing wires in a bounding box
let smartChannelRoute 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    let tl = channel.TopLeft
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"

    ///Predicate function determining whether a wire is inside the specified channel/bounding box
    let wireInChannelAndAdjustable channel (connId,wire) :bool = 
        //7 seg wires allowed ONLY
        if not (List.length wire.Segments = 7 || 
                List.length wire.Segments = 5 && wire.Segments[3].Mode = Manual) then
            false
        else
            match wireIntersectsBoundingBox wire channel with
            | None -> false
            | Some x -> true

    ///List of all wires partitioned into in channel/not in channel
    let allWiresList = 
        model.Wires
        |> Map.toList
        |> List.partition (wireInChannelAndAdjustable channel)

    ///List of wires in channel sorted by X coord of starting pos
    let channelWiresList = 
        fst allWiresList
        |> List.sortBy (fun (x,y) -> y.StartPos.X)

    ///Further partitioned channelWiresList into vertical and horizontal wires
    let orientedWiresList = 

        let wireOrientation (cid,wire) = 
            getAbsoluteSegmentPos wire 3
            |> (fun (st,en) -> getSegmentOrientation st en) //Retrieve 3rd segment orientation
            |> function
            | Vertical -> true
            | Horizontal -> false

        channelWiresList
        |> List.partition wireOrientation

    let selectedWiresList = 
        match channelOrientation with
        |Vertical -> fst orientedWiresList
        |Horizontal -> snd orientedWiresList
    let unSelectedWiresList = 
        match channelOrientation with
        |Vertical -> snd orientedWiresList
        |Horizontal -> fst orientedWiresList

    ///List of wires after initial even spacing
    let shiftedWiresList =
        //Setup desired wire positions
        let wireSpacing = 
            match channelOrientation with
            |Vertical -> 0.7*channel.W/(float (selectedWiresList.Length))
            |Horizontal -> 0.8*channel.H/(float (selectedWiresList.Length))

        let spacedWirePos = 
            match channelOrientation with
            |Vertical -> List.map (fun i -> tl.X  + float(i) * wireSpacing) [1..selectedWiresList.Length]
            |Horizontal -> List.map (fun i -> tl.Y  + float(i) * wireSpacing) [1..selectedWiresList.Length]

        let getOrientedAxisValue (x:XYPos*XYPos) = 
            match channelOrientation with
            |Vertical -> (fst x).X
            |Horizontal -> (fst x).Y

        //Calculates current deviation of each wire's mid segment from desired position, then calls moveSegment to correct it
        selectedWiresList
        |> List.map (fun (cid,wire) -> getAbsoluteSegmentPos wire 3)
        |> List.map getOrientedAxisValue
        |> List.map2 (fun autoPos absPos  -> autoPos - absPos) spacedWirePos //List of adjustments
        |> List.map2 (fun (cid,wire) adj -> (cid, moveSegment model wire.Segments[3] adj)) selectedWiresList //Carry out adjustments


    ///Shifted wires switched to minimise overlap
    let minOverlapShiftedWiresList = 
        
        //How close together we disallow overlaps
        let overlapRange = 
            match channelOrientation with
            |Vertical -> 0.05 * channel.H
            |Horizontal -> 0.05 * channel.W
            

        let switchMidSeg updatedModel (i1,i2) (wires:(ConnectionId*Wire) list) =
            let w1 = snd wires[i1]
            let w2 = snd wires[i2]

            //Check wire orientation to determine whether the shift is in x or y direction
            let delta = 
                match channelOrientation with
                |Vertical -> (fst (getAbsoluteSegmentPos w2 3)).X - (fst (getAbsoluteSegmentPos w1 3)).X
                |Horizontal -> (fst (getAbsoluteSegmentPos w2 3)).Y - (fst (getAbsoluteSegmentPos w1 3)).Y

            let movedWire1 = fst wires[i1], moveSegment updatedModel w1.Segments[3] delta             
            let movedWire2 = fst wires[i2], moveSegment updatedModel w2.Segments[3] -delta
            
            wires
            |> List.updateAt i1 movedWire1
            |> List.updateAt i2 movedWire2
        
        //Returns true if two wires overlap
        let rec compareOverlap wire1 wire2 :bool= 
            let w1 = snd wire1
            let w2 = snd wire2
            
            let wire1Leftmost = (fst (getAbsoluteSegmentPos w2 3)).X < (fst (getAbsoluteSegmentPos w1 3)).X
            let wire1Topmost = (fst (getAbsoluteSegmentPos w2 3)).Y < (fst (getAbsoluteSegmentPos w1 3)).Y

            match channelOrientation, wire1Leftmost, wire1Topmost with
            |Vertical, true, _ -> compareOverlap wire2 wire1
            |Horizontal, _, true -> compareOverlap wire2 wire1
            |_ -> 
                //wire1 seg 4 overlaps with wire2 seg 2, or wire1 seg 2 overlaps with wire2 seg 4
                let w12 = fst (getAbsoluteSegmentPos w1 2)
                let w14 = fst (getAbsoluteSegmentPos w1 4)
                let w22 = fst (getAbsoluteSegmentPos w2 2)
                let w24 = fst (getAbsoluteSegmentPos w2 4)  

                //Different conditions depending on wire direction
                let overlapCondition = 
                    match channelOrientation with
                    | Vertical -> 
                        let w1Dir = sign (w14.X - w12.X)//pos if left to right, neg if right to left
                        let w2Dir = sign (w24.X - w22.X)

                        match w1Dir, w2Dir with
                        | 1,1 -> (w14.Y < w22.Y + overlapRange && w14.Y > w22.Y - overlapRange)
                        | -1,-1 -> (w12.Y < w24.Y + overlapRange && w12.Y > w24.Y - overlapRange)
                        | 1, -1 -> (w14.Y < w24.Y + overlapRange && w14.Y > w24.Y - overlapRange)
                        | -1, 1 -> (w12.Y < w22.Y + overlapRange && w12.Y > w22.Y - overlapRange)
                        |_,_ -> false //make compiler happy
                    | Horizontal ->
                        let w1Dir = sign (w14.Y - w12.Y)//pos if top to bottom, neg if bottom to top
                        let w2Dir = sign (w24.Y - w22.Y)

                        match w1Dir, w2Dir with
                        | 1,1 -> (w14.X < w22.X + overlapRange && w14.X > w22.X - overlapRange)
                        | -1,-1 -> (w12.X < w24.X + overlapRange && w12.X > w24.X - overlapRange)
                        | 1, -1 -> (w14.X < w24.X + overlapRange && w14.X > w24.X - overlapRange)
                        | -1, 1 -> (w12.X < w22.X + overlapRange && w12.X > w22.X - overlapRange)
                        |_,_ -> false //make compiler happy

                if overlapCondition then
                    true
                else
                    false

        //Perform a single switch with given wire and wireList, if switch is needed
        let switchCompareSinglePass updatedModel cidWire1 (wireList:(ConnectionId*Wire) list) :(ConnectionId*Wire) list option= 

            //Curry the predicate to set up comparison to wire1
            let overlapPredicate = compareOverlap cidWire1
            let index = List.tryFindIndex overlapPredicate wireList

            let fullList = [cidWire1]@wireList

            index
            |> function
            | None -> None
            | Some (i) -> Some(switchMidSeg updatedModel (0,i+1) fullList)

        //Detect any overlaps down the length of the whole list
        let rec scanAndSwitchList updatedModel (wires: (ConnectionId*Wire) list option) = 
            //one full pass of checking and shifting
            match wires with

            | Some (hd::[]) -> None //no switch found for this whole pass-> return None
            | Some (hd::tl) -> 
                switchCompareSinglePass updatedModel hd tl
                |> function
                | Some x-> Some(x) //Switched, so stop and return
                | None -> 
                    scanAndSwitchList updatedModel (Some(tl))
                    |> function
                    | None -> None //we've tested the whole list, no switches, we return None
                    | Some(x) -> Some([hd] @ x) //no switch -> go deeper and append result to current head

            | None -> raise (System.ApplicationException("Infinite recursion in wire switching")) // really won't happen, make compiler happy
            | Some ([]) -> None //won't happen, make compiler happy

        ///Take list of 7-seg wires, returns switched list of wires to minimise overlap
        let rec minimiseOverlap updatedModel depth (wires: (ConnectionId*Wire) list option)  = 

            if (depth < 1.0) then
                wires
            else
                match scanAndSwitchList updatedModel wires with
                | Some (x) -> 
                    //adjust model with new switched wires
                    let tmpModel = {updatedModel with Wires = Map.ofList x}
                    minimiseOverlap tmpModel (depth-1.0) (Some (x)) 

                | None -> wires //No switch in the pass, we can stop

        let maxDepth = (2.0** (float (List.length shiftedWiresList)))

        Some(shiftedWiresList)
        |> minimiseOverlap {model with Wires = Map.ofList shiftedWiresList} maxDepth
        |> Option.defaultValue shiftedWiresList
            
    let allWiresMap = 
        minOverlapShiftedWiresList  @ unSelectedWiresList @ (snd allWiresList) 
        |> Map.ofList

    {model with Wires = allWiresMap}
    
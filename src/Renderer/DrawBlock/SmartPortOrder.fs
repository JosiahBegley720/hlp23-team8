﻿module SmartPortOrder
open SymbolUpdate
open SmartWire
open CommonTypes
open SmartHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWireUpdateHelpers
open Optics
open Operators

// Authored exclusively by Josiah Begley CID: 01846829
// wire updated handeled in SheetUpdated.fs
(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)
(*
    To test this, it must be given two symbols interconnected by wires. It then reorders the ports on
    symbolToOrder so that the connecting wires do not cross.
    Tt should work out the interconnecting wires (wiresToOrder) from
    the two symbols, wModel.Wires and sModel.Ports
    It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).
*)
let reOrderPorts (wModel: BusWireT.Model) (symbolToOrder: Symbol) (otherSymbol: Symbol): BusWireT.Model =
    printfn $"SYMBOL TYPE: {symbolToOrder.Component.Type}"
    // gets the symbol model used for symbol manipulation
    let sModel = wModel.Symbol
    printfn $"symbolToOrder.Component.InputPorts.Id: {symbolToOrder.Component.InputPorts |> List.map (fun x -> x.Id)}"
    
    // uses the list of wires to determine a list of connected port ids
    let portConnections = getPortConnections symbolToOrder otherSymbol wModel

    // generates symbol map for each component           
    let getSymbolPortMap (symbol: Symbol) =
        let inputPortMap = symbol.Component.InputPorts
                            |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                            |> Map.ofList
        let outputPortMap = symbol.Component.OutputPorts
                            |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                            |> Map.ofList
        [ inputPortMap; outputPortMap ]
    
    let inline reverseTuple tupleList =
        tupleList
        |> List.map (fun (x,y) -> (y,x))
    
    // used to sort a list of port connections when some connections dont exist (None)
    let sortList myList =
        myList |> List.sortWith(fun (x1, y1) (x2, y2) ->
            match y1, y2 with
            | None, None -> 0
            | None, _ -> 1
            | _, None -> -1
            | _, _ -> compare y1 y2)

    // returns the port connections by port number
    let getConnectedNumbers (map1: Map<string, int>) (map2: Map<string, int>) (connections: (string * string) list) : (int * int option) list =
        map1
        |> Map.toList
        |> List.sortBy snd
        |> List.rev
        |> List.mapi (fun i (id, _) -> (id, i))
        |> Map.ofList
        |> Seq.map (fun x ->
            match List.tryFind (fun (_, id2) -> id2 = x.Key) (reverseTuple connections) with
            | Some (id1, _) ->
                match map2.TryFind(id1) with
                | Some (int2) -> (x.Value, Some(int2))
                | None -> (x.Value, None)
            | None -> (x.Value, None))
        |> List.ofSeq
        |> sortList
        |> List.sortBy fst
    
    //calculates the wires needed tobe updated, and passes them through smartAutoRoute 
    let wires = 
            [ [symbolToOrder.Id]; [otherSymbol.Id] ]
            |> List.map (getConnectedWires wModel)
            |> (fun lst -> Set.intersect ((List.head lst) |> Set) ((List.head (List.tail lst)) |> Set))
            |> Set.toList
    let updateWires =
        wModel.Wires
        |> Map.toList
        |> List.map(fun (x,y) -> match List.contains y wires with
                            | true -> (x,smartAutoroute wModel y)
                            | false -> x,y)
        |> Map.ofList
            
    let maps = [(getConnectedNumbers ((getSymbolPortMap otherSymbol)[1]) ((getSymbolPortMap symbolToOrder)[0]) portConnections)|> List.sortByDescending fst;
                (getConnectedNumbers ((getSymbolPortMap symbolToOrder)[1]) ((getSymbolPortMap otherSymbol)[0]) portConnections)|> List.sortByDescending snd ]
    
    match otherSymbol.Component.Type, symbolToOrder.Component.Type with
    | _, Custom _->
        // port reordering of custom components
        // reorders the ports based on the existing port order and the port connections
        let reorderList (portIds: string list list) (connections: (int*int option) list list) =
            let filteredList = 
                portIds[0] 
                |> List.mapi (fun i x -> (i, x)) 
                |> List.filter (fun (i, _) -> not (List.exists (fun (_, index) -> index = Some i) connections[0])) 
                |> List.map snd
                
            
            let numberFilteredList =
                filteredList
                |> List.map(fun x -> List.findIndex (fun y -> y = x) portIds[0])
                |> List.map (fun x -> (x,None))
            
            printfn $"Filtered List :{numberFilteredList}" // Filtered List :[]
            let filteredConnections =
                connections[0]
                |> List.filter (fun (_,x) -> x <> None)
                |> List.append numberFilteredList
                
            let mutable filteredIndex = 0
            let inputPorts = match portIds[0].Length with
                          | 0 -> portIds[0]
                          | _ -> List.map (fun (_,index) -> match index with
                                                                | Some int -> portIds[0][int]
                                                                | None -> match filteredList.Length with
                                                                    | 0 -> "hey"
                                                                    | _ ->
                                                                            let filtered = filteredList[filteredIndex]
                                                                            filteredIndex <- filteredIndex + 1
                                                                            filtered) filteredConnections
            let outputPorts = match portIds[1].Length with
                          | 0 -> portIds[1]
                          | _ -> List.map (fun (index,_) -> portIds[1][index]) connections[1]
            
            [inputPorts; outputPorts]

        // updates the corresponding area of the portMap      
        let updatedMapOrder =
                let portMap =  [Map.find Left symbolToOrder.PortMaps.Order; Map.find Right symbolToOrder.PortMaps.Order]
                let reorderedList = reorderList portMap maps
                symbolToOrder.PortMaps.Order |> Map.add Left reorderedList[0] |> Map.add Right reorderedList[1]
        
        let updatedPortMaps = { symbolToOrder.PortMaps with Order = updatedMapOrder }
        let symbol' = { symbolToOrder with PortMaps = updatedPortMaps }
        
        // returns updated model
        { wModel with
            Wires = updateWires
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }
        
    | Custom _, And _ |Custom _, Or _ |Custom _,Xor _ |Custom _,Nand _ |Custom _,Nor _ |Custom _,Xnor _ |Custom _,NbitsAdder _|Custom _,NbitsAdderNoCout _|Custom _,NbitsXor _|Custom _,NbitsAnd _|Custom _,NbitsOr _->
        // port reordering of standard components
        // determines whether the order of ports is optimal
        printfn $"map1:{maps[0]}"
        printfn $"map1:{maps[1]}"
        let rec isMonotonicallyIncreasing lst =
            match lst with
            | [] | [_] -> true
            | None :: tail -> isMonotonicallyIncreasing tail
            | Some x :: Some y :: tail -> x < y && isMonotonicallyIncreasing (Some y :: tail)
            | Some x :: None :: tail -> isMonotonicallyIncreasing (Some x :: tail)
            | _ -> false

        let increasingCheck = isMonotonicallyIncreasing (maps[0] |> List.map (fun (_,x) -> x))
        
        // determines whether flipping the component is beneficial based on heuristic above
        let symbol' = match increasingCheck with
                        | true -> symbolToOrder
                        | false -> flipSymbol FlipVertical symbolToOrder
                        
        { wModel with
            Wires = updateWires
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }
        
    | Custom _, Mux2| Custom _, Mux4| Custom _, Mux8 ->
        // port reordering of Mux components
        // heuristic used to determine whether flipping a mux of any size minimises wire crossing
        
        let rec calculatePenalty lst =
            match lst with
            | [] | [_] -> 0
            | None :: tail -> calculatePenalty tail
            | Some x :: Some y :: tail -> match x < y with
                                            | true -> calculatePenalty (Some y :: tail)
                                            | false -> 1 + calculatePenalty (Some x :: tail)
            | Some x :: None :: tail -> calculatePenalty (Some x :: tail)
        
        // based on this heuristic flips symbol
        let penalty = calculatePenalty (maps[0] |> List.map (fun (_,x) -> x))
        printfn $"PENALTY: {penalty}"
        printfn $"MAP: {(maps[0]|> List.filter (fun (_,x) -> x <> None)).Length/2}"
        let symbol' = match (penalty < ((maps[0]|> List.filter (fun (_,x) -> x <> None)).Length/2)) with
                        | true -> symbolToOrder
                        | false -> flipSymbol FlipVertical symbolToOrder
        
        { wModel with
            Wires = updateWires
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }
        
    | _,_ -> wModel
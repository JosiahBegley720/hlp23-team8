module ErrorCheck

open VerilogTypes
open Fable.Core.JsInterop


/// Helper function to find the line of an item in the original string
let findLineOfItem (newLinesLocations:int list) (currLocation:int) : int*int*int =
    let isSmallerThan x y = y <= x
    let isGreaterThan x y = y > x
    printfn "list %A"newLinesLocations
    printfn "curr %i" currLocation 
    
    let prevIndex = List.findIndexBack (fun x -> isSmallerThan currLocation x) newLinesLocations
    let nextIndex = List.findIndex (fun x -> isGreaterThan currLocation x) newLinesLocations
    (prevIndex+1,newLinesLocations[prevIndex],newLinesLocations[nextIndex])

/// Checks whether all ports given in the beginning of the module are defined as input/output
let portCheck ast errorList = 
    let portList = ast.Module.PortList |> Array.toList
    printfn "Ports: %A" portList
    let indexList = ast.Module.Locations |> Array.toList
    let indexMap =
        (portList, indexList) ||> List.map2 (fun p i -> (p,int i)) |> Map.ofList
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let decls = 
        items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [x]) 
                | None -> []
            | true -> []
        )
    let diff = Seq.except (decls |> List.toSeq) (portList |> List.toSeq)
    match Seq.isEmpty diff with
    | false -> 
        let mess = sprintf "Port(s): [%A] are not declared either as input or output" (diff |> Array.ofSeq)
        let index = indexMap[portList[0]]
        let length = indexMap[(List.last portList)] + String.length (List.last portList) - index            
        
        List.append errorList [{Line = 1; Col=index+1;Length=length;Message = mess}]

    | true -> errorList

/// Checks whether the portSizeMap contains valid size information
let portSizeCheck portSizeMap portLocationMap linesLocations errorList = 
    let localErrors = 
        portSizeMap
        |> Map.filter (fun n s -> s<1)
        |> Map.toList
        |> List.map fst
        |> List.map (fun port -> 
            let currLocation = Map.find port portLocationMap
            let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
            {Line = line; Col=currLocation-prevLineLocation+1 ; Length=nextLineLocation-currLocation-1 ; Message = sprintf "Port '%s' has wrong width declaration" port}
        )
    List.append errorList localErrors

/// Checks if the name of the module is valid (i.e. starts with a character)
let nameCheck ast errorList = 
    let name =  ast.Module.ModuleName
    printfn "Name of module: %s" ast.Module.ModuleName
    let notGoodName =
        name
        |> Seq.toList
        |> List.tryHead
        |> function | Some ch when  System.Char.IsLetter ch -> false | _ -> true
    match notGoodName with
    | true -> List.append errorList [{Line = 0; Col=0;Length=0;Message = sprintf "Module Name must start with a character to be valid"}]
    | false -> errorList

/// Checks the names used for parameters
/// Throws an error if the name is used for a port 
let parameterNameCheck ast parameterNames portMap parameterLocationMap linesLocations errorList = 
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> 
                let currLocation = Map.find name parameterLocationMap
                let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
                [{Line = line; Col=currLocation-prevLineLocation+1;Length=nextLineLocation-currLocation-1;Message = sprintf "Variable %s cannot be used as a parameter name because it is declared as a port" name}]
            | None -> []
        ) parameterNames
    List.append errorList localErrors

/// Checks the names used for wires
/// Throws an error if the name is used for a port 
let wireNameCheck ast portMap parameterSizeMap wireNameList errorList =   
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> [{Line = 0; Col=0;Length=0;Message = sprintf "Variable %s cannot be used as a wire name because it is declared as a port" name}]
            | None -> 
                match Map.tryFind name parameterSizeMap with
                | Some p -> [{Line = 0; Col=0;Length=0;Message = sprintf "Variable %s cannot be used as a wire name because it is declared as a parameter" name}]
                | None -> []
        ) wireNameList
    List.append errorList localErrors


/// Checks if all declared ports have a value assigned to them
/// The check is done bit-by-bit
let checkAllOutputsAssigned ast portMap portSizeMap portLocationMap (linesLocations: int list) errorList =
    
    // List of declared ports, bit by bit
    let outputPortListMap = 
        portMap 
        |> Map.filter (fun n s -> s = "output") 
        |> Map.toList 
        |> List.map fst
        |> List.collect (fun x -> 
            let size = Map.find x portSizeMap
            let names = [0..size-1] |> List.map (fun y -> (x+(string y),x))
            names 
        )

    let outputPortList = List.map fst outputPortListMap

    // List of assignments in the form of (port name, BitsStart, BitsEnd)
    let assignments = 
        ast.Module.ModuleItems.ItemList
        |> Array.toList 
        |> List.collect (fun x -> 
            match (x.Statement |> isNullOrUndefined) with
            | false -> 
                match x.Statement with
                | Some statement when statement.StatementType = "assign" -> [statement.Assignment.LHS]
                | _ -> []
            | true -> []
        )
        |> List.map (fun assignment ->
            match assignment with
            | a when isNullOrUndefined assignment.BitsStart -> (a.Primary,-1,-1)
            | a -> (a.Primary,(int (Option.get a.BitsStart)),(int (Option.get a.BitsEnd)))
        )
    
    
    // List of assigned ports, bit by bit
    let assignmentPortListMap =
        assignments
        |> List.collect ( fun x ->
            match x with
            |(name,-1,-1)->
                match Map.tryFind name portSizeMap with
                | Some size -> 
                    let names = [0..size-1] |> List.map (fun y -> (name+(string y),name))
                    names
                | None -> []
            |(name,x,y) when x=y ->
                [(name+(string x),name)]
            |(name,bStart,bEnd)->
                let names = [bEnd..bStart] |> List.map (fun y -> (name+(string y),name))
                names
        )
    let assignmentPortList = List.map fst assignmentPortListMap
    
    /// Returns the unassigned port names given the output port list and the assigned port list 
    let diffChecker exclude lst mapping message errorList =
        let diff = List.except (List.toSeq exclude) (lst)
        match List.isEmpty diff with
        |true -> errorList
        |false -> 
            // transform names from "b2" to "b[2]" 
            let unassignedPorts = 
                diff 
                |> List.collect(fun x ->
                    match Map.tryFind x (Map.ofList mapping) with
                    | Some name -> 
                        let length = (Seq.except name x) |> Seq.map string |> String.concat ""
                        [name+"["+length+"]"]
                    | None -> []
                )
            let portLoc = linesLocations[((List.length linesLocations)-2)]  
            let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations portLoc     
            // [{Line = line; Col=currLocation-prevLineLocation+2 ; Length=nextLineLocation-currLocation-3  ;Message = sprintf "Wrong width on assignment of output port: %s" lhs.Primary}]   
            List.append errorList [{Line = line; Col=portLoc-prevLineLocation+2 ; Length=nextLineLocation+1-portLoc-3; Message = sprintf "%s: %A" message unassignedPorts}]

    let localErrors =
        []
        |> diffChecker assignmentPortList outputPortList outputPortListMap "The following ports are declared but not assigned"
        // |> diffChecker outputPortList assignmentPortList assignmentPortListMap "The following ports are assigned, but not declared as output ports" 
    List.append errorList localErrors


/// Recursive function to get all the primaries used in the RHS of an expression
let rec usedInAssignment inLst (tree: ExpressionT) = 
    match tree.Type with
    | "unary" when (Option.get tree.Unary).Type = "primary" -> List.append inLst [Option.get (Option.get tree.Unary).Primary]
    | "unary" when (Option.get tree.Unary).Type = "parenthesis" -> usedInAssignment inLst (Option.get (Option.get tree.Unary).Expression)
    | "reduction_negation" when (Option.get tree.Unary).Type = "primary" -> List.append inLst [Option.get (Option.get tree.Unary).Primary] 
    | "reduction_negation" when (Option.get tree.Unary).Type = "parenthesis" -> usedInAssignment inLst (Option.get (Option.get tree.Unary).Expression)    
    | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
    | "additive"  -> List.append (usedInAssignment inLst (Option.get tree.Head)) (usedInAssignment inLst (Option.get tree.Tail))
    | _ -> inLst



//////////////////////////////

let checkAssignments (ast:VerilogInput) portMap portSizeMap portWidthDeclarationMap inputParameterWireSizeMap inputParameterWireList linesLocations errorList =
    
    
    let checkLHSNameAndWidth assignment errorString : string= 
        let lhs = assignment.LHS
        match Map.tryFind lhs.Primary portMap with
        | Some found when found = "output"  -> 
            match Map.tryFind lhs.Primary portWidthDeclarationMap with
            | Some (bStart,bEnd) -> 
                match isNullOrUndefined lhs.BitsStart with
                | false ->
                    if (bStart >= (int (Option.get lhs.BitsStart))) && (bEnd <= (int (Option.get lhs.BitsEnd))) then
                        errorString
                    else 
                        errorString + sprintf "- Wrong width of output port: '%s'" lhs.Primary
                | true -> errorString
            | None -> failwithf "Can't happen! PortMap and PortSizeMap should have the same keys"
        | _ -> 
            errorString + sprintf "- Variable '%s' is not declared as an output port" lhs.Primary
    
    
    let checkNamesOfAssignment (assignment: AssignmentT) errorString=
        let PrimariesRHS = usedInAssignment [] assignment.RHS
        
        let namesRHS = PrimariesRHS |> List.map (fun x -> x.Primary)

        let diff = List.except (List.toSeq inputParameterWireList) namesRHS
        match List.isEmpty diff with
        | true -> errorString
        | false -> 
            if errorString <> "" then
                errorString + sprintf " \n - The following variables are not defined as input/parameter/wire: [%A]" (Seq.toArray diff)
            else errorString + sprintf "- The following variables are not defined as input/parameter/wire: [%A]" (Seq.toArray diff) 

    let checkSizesOfAssignment (assignment: AssignmentT) errorString =
        let primariesRHS = usedInAssignment [] assignment.RHS
        let localErrors = 
            primariesRHS
            |> List.map (fun x -> 
                match isNullOrUndefined x.BitsStart with
                | false ->
                    let name = x.Primary
                    let bStart = int <| Option.get x.BitsStart 
                    let bEnd = int <| Option.get x.BitsEnd
                    match Map.tryFind name inputParameterWireSizeMap with
                    | Some size -> 
                        if (bStart<size) && (bEnd>=0) && (bStart>=bEnd) then
                            "" //ok
                        else sprintf "- Wrong width of variable: '%s'" name
                    | None -> "" //invalid name, error found by AssignmentRHSNameCheck 
                | true -> ""
            )
            
        let oneError = ("",localErrors) ||> List.fold(fun s err -> if err <> "" then s+err+" \n " else s)
        
        if errorString <> "" then 
            errorString + "\n" + oneError
        else errorString + oneError

    /// Checks whether the length of bits on LHS of assignment matches the length of bits on the RHS
    /// TODO: needs improvement -> need to search RHS for wires and parameters as well
    ///                         -> which item (currently returns item number)
    ///                         -> which variable has the wrong width
    let checkBitsOfAssignemnt (assignment: AssignmentT) errorString =
        let lengthLHS = 
            match isNullOrUndefined assignment.LHS.BitsStart with
            | true -> 
                match Map.tryFind assignment.LHS.Primary portSizeMap with
                | Some num -> [num]
                | None -> [-2]
            | false -> [((Option.get assignment.LHS.BitsStart) |> int) - ((Option.get assignment.LHS.BitsEnd) |> int)+1]

        let PrimariesRHS = usedInAssignment [] assignment.RHS
        let lengthRHS =
            PrimariesRHS
            |> List.map (fun x ->
                match isNullOrUndefined x.BitsStart with
                | true -> 
                    match Map.tryFind x.Primary inputParameterWireSizeMap with
                    | Some num -> num
                    | None -> lengthLHS[0] // if name doesn't exist skip it, error found by assignmentRHSNameCheck
                | false -> ((Option.get x.BitsStart) |> int) - ((Option.get x.BitsEnd) |> int) + 1
            )
        match lengthLHS with
        | [-2] -> errorString
        | _ ->
            let diff = List.except (List.toSeq lengthLHS) lengthRHS
            match List.isEmpty diff with
            | true -> 
                errorString
            | false -> 
                if errorString <> "" then 
                    errorString + sprintf "\n - Different bit size on right <-> left"
                else errorString + sprintf "- Different bit size on right <-> left"


    let assignmentsWithLocation = 
        ast.Module.ModuleItems.ItemList 
        |> Array.toList 
        |> List.filter (fun item -> item.ItemType = "statement")
        |> List.map (fun item -> (Option.get item.Statement),item.Location)
        |> List.filter (fun (statement,loc) -> statement.StatementType = "assign")
        |> List.map (fun (statement,loc) -> statement.Assignment,loc)
        // |> List.unzip

    let localErrors = 
        assignmentsWithLocation
        |> List.collect (fun (assignment,location) ->
            let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations location 
            let assignmentErrors = 
                checkLHSNameAndWidth assignment ""
                |> checkNamesOfAssignment assignment 
                |> checkSizesOfAssignment assignment
                |> checkBitsOfAssignemnt assignment
            match assignmentErrors with
            | "" -> []
            | _ -> [{Line = line; Col=location-prevLineLocation+2 ; Length=nextLineLocation-location-3  ; Message = assignmentErrors}]
        )

    List.append errorList localErrors
/////////////////////////////

/// Returns the port-size map (e.g. (port "a" => 4 bits wide))
let getPortSizeAndLocationMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let portSizeLocation = items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    let size = 
                        match isNullOrUndefined d.Range with
                        | true -> 1
                        | false -> ((Option.get d.Range).Start |> int) - ((Option.get d.Range).End |> int) + 1
                    let location = x.Location
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [(x,size,location)]) 
                | None -> []
            | true -> []
    )
    let ps = List.map (fun x -> match x with | p,s,l -> (p,s)) portSizeLocation
    let pl = List.map (fun x -> match x with | p,s,l -> (p,l)) portSizeLocation
    (Map.ofList ps, Map.ofList pl)




/// Returns the port-width declaration map (e.g. (  port "a" => (4,0)  ))
let getPortWidthDeclarationMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    let size = 
                        match isNullOrUndefined d.Range with
                        | true -> (0,0)
                        | false -> ((Option.get d.Range).Start |> int),((Option.get d.Range).End |> int)
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [(x,size)]) 
                | None -> []
            | true -> []
    ) |> Map.ofList

/// Returns the port-type map (e.g. (port "a" => INPUT))
let getPortMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [(x,d.DeclarationType)]) 
                | None -> []
            | true -> []
    ) |> Map.ofList
    
////// NOT FINISHED : TODO
let getParameterSizeAndLocationMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let parameterSizeLocation = items |> List.collect (fun x -> 
            match (x.ParamDecl |> isNullOrUndefined) with
            | false -> 
                match x.ParamDecl with
                | Some p -> 
                    let size = 1 ////// FOR NOW 
                        // match isNullOrUndefined d.Range with
                        // | true -> 1
                        // | false -> ((Option.get d.Range).Start |> int) - ((Option.get d.Range).End |> int) + 1
                    let location = x.Location
                    [(p.Parameter.Name,size,location)] 
                    // |> Array.toList 
                    // |> List.collect (fun x -> [(x,size)]) 
                | None -> []
            | true -> []
    )
    let ps = List.map (fun x -> match x with | p,s,l -> (p,s)) parameterSizeLocation
    let pl = List.map (fun x -> match x with | p,s,l -> (p,l)) parameterSizeLocation
    (Map.ofList ps, Map.ofList pl)

let getInputSizeMap inputNameList portSizeMap =
    portSizeMap
    |> Map.filter (fun n s -> (List.exists (fun x -> x = n) inputNameList))

/// Returns the names of the ports declared as INPUT
let getInputNames portMap = 
    portMap 
    |> Map.filter (fun n s -> s = "input") 
    |> Map.toList 
    |> List.map fst


/// Returns the names of the declared WIRES
let getWireNames ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
        match (x.Statement |> isNullOrUndefined) with
        | false -> 
            match x.Statement with
            | Some statement when statement.StatementType = "wire" -> [statement.Assignment.LHS.Primary]
            | _ -> []
        | true -> []
    )

/// Main error-finder function
/// Returns a list of errors (strings)
let getErrors ast model linesLocations =
    
    ///////// MAPS, LISTS NEEDED  ////////////////
    printfn "Parsed input: %A" ast
    let portMap  = getPortMap ast
    let portSizeMap,portLocationMap = getPortSizeAndLocationMap ast
    printfn "Port size map: %A" portSizeMap
    let portWidthDeclarationMap = getPortWidthDeclarationMap ast
    let parameterSizeMap, parameterLocationMap = getParameterSizeAndLocationMap ast
    let parameterNameList = parameterSizeMap |> Map.toList |> List.map fst
    let inputNameList = getInputNames portMap
    let inputSizeMap = getInputSizeMap inputNameList portSizeMap
    let wireNameList = getWireNames ast
    let inputParameterWireList =
        parameterNameList
        |> List.append inputNameList
        |> List.append wireNameList
    
    let inputParameterWireSizeMap = 
        Map.toList inputSizeMap
        |> List.append (Map.toList parameterSizeMap)
        // |> List.append (Map.toList wireSizeList)   //TODO: WireSizeMap
        |> Map.ofList
    //////////////////////////////////////////////

    
    []  //begin with empty list and add errors to it
    // |> nameCheck ast //valid module name
    |> portCheck ast //all ports are declared as input/output
    |> portSizeCheck portSizeMap portLocationMap linesLocations//correct port width declaration (e.g. [1:4] -> invalid)
    |> parameterNameCheck ast parameterNameList portMap parameterLocationMap linesLocations
    |> wireNameCheck ast portMap parameterSizeMap wireNameList
    |> checkAssignments ast portMap portSizeMap portWidthDeclarationMap inputParameterWireSizeMap inputParameterWireList linesLocations
    |> checkAllOutputsAssigned ast portMap portSizeMap portLocationMap linesLocations
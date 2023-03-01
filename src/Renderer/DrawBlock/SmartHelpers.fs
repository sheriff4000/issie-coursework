module SmartHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

//-----------------------------------------------------------------------------------------------//
//---------------------------HELPERS FOR SMART DRAW BLOCK ADDITIONS------------------------------//
//-----------------------------------------------------------------------------------------------//

(*
HOW TO USE THIS MODULE.

(1) Add well-documented useful functions - see updateModelSymbols and updateModelWires
    for examples. You do not need to add performance information as in updateModelSymbols. 
    Your priority should be writing clear code. Try to avoid very inefficient implementations
    if possible (e.g. 100X slower than a similar complexity solution), but do not worry 
    about this.
(2) Note from my examples distinction between XML documentation and additional details
    in header comments.
(3) HLP23: Note comments here labelled "HLP23" which are for HLP23 class and would be deleted in
    production (Group phase) code.
(2) HLP23: Each function must have a single author specified by "HLP23: AUTHOR" in an XML comment
    as in my example: give name as Family name only (unique within teams).
(3) HLP23: Inform other members that you have written a function they could maybe use.
(4) HLP23: If two people end up with near-identical functions here team phase can rationalise if
    needed normally you are expected to share when this makes code writing faster.
(5) Note best practice here using Optics for nested record update. This is NOT curently required
    in Issie but used appropriately results in better code. Use it if you are comfortable doing so.
(5) Note on qualifying types: do this when not doing it would be ambiguous - e.g. here
    the BusWire and Symbol Model types.
(6) Note on code layout. A limit of 100 characters per line is used here. Seems about right.
*)

//----------------------------------------------------------------------------------------------//

/// Update BusWire model with given symbols. Can also be used to add new symbols.
/// This uses a fold on the Map to add symbols which makes it fast in the case that the number
/// of symbols added is very small.
//  Performance scales as O(M*log2(N)) - M = number of symbols added, N = number of existing
//  Symbols. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelSymbols 
    (model: BusWireT.Model) 
    (symbols: Symbol list)
        : BusWireT.Model =
    // HLP23: note on fold implementation. symMap is both argument and result of the
    // fold function => sequential set of updates. In thsi case much more efficient than Map.map
    // over all symbols.
    // HLP23 - see also similar updateModelWires
    let symbols' =
        (model.Symbol.Symbols,symbols)
        ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap)
    Optic.set (symbol_ >-> symbols_) symbols' model

/// Update BusWire model with given wires. Can also be used to add new wires.
/// This uses a fold on the Map to add wires which makes it fast in the case that the number
/// of wires added is small.
//  Performance scales as O(M*log2(N)) - M = number of wires added, N = number of existing
//  wires. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelWires 
    (model: BusWireT.Model) 
    (wiresToAdd: Wire list)
        : BusWireT.Model =
    //
    // HLP23: note on fold implementation. In this (typical) example Map is
    // sequentially updated by the fold. A common and difficult to see coding mistake is to use the 
    // original wireMap (argument of Optic map function) not the updated one (wireMap argument of 
    // List.map folder) in the fold function! That is not possible here because both have the same 
    // name so the inner bound updated wireMap is always what is used in the folder function. 
    // This is good practice, and if you have ever debugged this type of mistake you will know it
    // is very necessary!

    // HLP23: note on this use of Optics.map in a pipeline. It is more "functional" than the 
    // equivalent implementation using a let definition and Optics.set. Is it clearer? Or less clear? 
    // Standard logic says we should prefer the pipeline if the name of the let definition adds 
    // nothing which is the case here. I have given you both ways of using Optics here so you can 
    // compare the two implementations and decide. NB - you are NOT required to use Optics in your 
    // own code.
    //
    // HLP23: Note how multiple updates to different parts of the model can be neatly pipelined 
    // like this using a separate Optic.map or Optic.set for each.
    //
    // HLP23: note that if the operation here was larger or part of some pipeline the
    // 2nd argument to Optic.map - which defines the model change - could be given a name and 
    // turned into a local function making the Optic.map line like:
    // |> Optic.map wires_ myNameForThisWireMapUpdate
    model
    |> Optic.map wires_ (fun wireMap  ->
        (wireMap,wiresToAdd)
        ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))

//HLP23: Sherif
//This function simply takes a model and extracts the map of <ComponentId, Symbol> in a usable form,
//I am using it to determine the total space occupied by the components

let getComponentInfo (model:Model) =
    model.Symbol.Symbols
    |> Map.map (fun id symbol -> symbol.Component)



/// HLP23: Sherif
/// This function takes in two lists and returns the elements in newList that aren't in oldList
let rec listDifference (newList: 'a list) (oldList: 'a list) = 
        match oldList with 
            | el::tl -> 
                let outList = List.except (seq {el}) newList
                listDifference outList tl
            | [] -> newList

// HLP23: Luke
// This function returns a list of the IDs of all the input ports of a symbol
let getInputPortIds
    (symbol: Symbol)
    : string list =
    List.map (fun (x:Port) -> x.Id) symbol.Component.InputPorts

// HLP23: Luke
// This function returns a list of the IDs of all the output ports of a symbol
let getOutputPortIds
    (symbol: Symbol)
    : string list =
    List.map (fun (x:Port) -> x.Id) symbol.Component.OutputPorts

// HLP23: Luke
// This function returns whether a wire is inputted into a symbol
let isSymbolInputForWire
    (symbol: Symbol)
    (wire: Wire)
    : bool =
    symbol
    |> getInputPortIds
    |> List.contains (string wire.InputPort)

// HLP23: Luke
// This function returns the value of VScale or HScale or 1 if it has no value
let getScale (scale: float option) = if scale=None then 1.0 else Option.get scale

// HLP23: Luke
// This function checks if a value is withing the range of two other numbers
let inRange (x: float) (a: float) (b: float) = (x >= a) && (x <= b)

// HLP23: Luke
// This function get the distance between ports on the edge of a symbol
let getPortDistances
    (symbol: Symbol)
    (edge: Edge)
    : float =

    let maxPortNumber =
        symbol.PortMaps.Order
        |> Map.toList
        |> List.filter (fun (x,_) -> x = edge)
        |> List.item 0
        |> snd
        |> List.length

    if (edge=Top || edge=Bottom)
    then (symbol.Component.W  * getScale symbol.HScale) / ( float (maxPortNumber) + 1.0 )
    else (symbol.Component.H  * getScale symbol.VScale) / ( float (maxPortNumber) + 0.4 )

// HLP23: Luke
// This function returns the port number for a wire connected to a symbol counting from left to right or top to bottom
// or None if the port is not on the symbol
let getPortPositionFromTopOrLeft
    (symbol: Symbol)
    (wire: Wire)
    : int option =
    let portId =
        if isSymbolInputForWire symbol wire
        then string wire.InputPort
        else string wire.OutputPort

    let edge = 
        symbol.PortMaps.Order
        |> Map.toList
        |> List.filter (fun (_,lst) -> List.contains portId lst)

    if edge.Length > 0
    then 
        let index =
            edge[0]
            |> snd
            |> List.findIndex (fun x -> x=portId)
        
        let edgeName = fst edge[0]
        if edgeName = Top || edgeName = Right
        then 
            Some ((snd edge[0]).Length - index)
        else Some (index + 1)
    else None


//HLP23: AUTHOR Ewan
//This function returns a list of all the elements in both input lists
let combineLists (list1: 'a List) (list2: 'a List) = 
        List.allPairs list1 list2
        |> List.filter (fun (x,y) -> x = y)
        |> List.map fst 

// HLP23: Luke
// This function returns all the wires that connect two symbols
let wiresBetweenSymbols
    (model: Model)
    (symbol1: Symbol)
    (symbol2: Symbol)
    : Wire list =
    model.Wires
    |> Map.toList
    |> List.filter (fun (_,wire) ->
        isSymbolInputForWire symbol1 wire && List.contains (string wire.InputPort) (getInputPortIds symbol1) && List.contains (string wire.OutputPort) (getOutputPortIds symbol2) ||
        isSymbolInputForWire symbol2 wire && List.contains (string wire.InputPort) (getInputPortIds symbol2) && List.contains (string wire.OutputPort) (getOutputPortIds symbol1) )
    |> List.map (fun (_,wire) -> wire)


// HLP23: Luke
// This function returns a list of wires between two components that are connected on two adjascent edges together with the
// edges the wire is connected to
let getAdjacentConnections
    (model: Model)
    (symbol1: Symbol)
    (symbol2: Symbol)
    : ((Edge*Edge) * Wire) list =

    wiresBetweenSymbols model symbol1 symbol2
    |> List.map (fun x ->
        let symbol1PortId, symbol2PortId =
            if isSymbolInputForWire symbol1 x
            then string x.InputPort, string x.OutputPort
            else string x.OutputPort, string x.InputPort

        let symbol1Edge = symbol1.PortMaps.Orientation.Item symbol1PortId
        let symbol2Edge = symbol2.PortMaps.Orientation.Item symbol2PortId

        match symbol1Edge, symbol2Edge with
        | Top,Bottom | Bottom,Top | Left,Right | Right,Left -> Some ((symbol1Edge,symbol2Edge),x)
        | _ -> None )
    |> List.filter (fun x -> not (x=None))
    |> List.map (fun x -> Option.get x)

//HLP23: AUTHOR Ewan
//This function find all the wires connected between two symbols
//This function returns a list of 3 part tuples
//containing the wire, the connected port ID for the first symbol, and the connected port ID for the second symbol

let connectingWires (symbol1:Symbol) (symbol2:Symbol) (model:Model)=
        let isConnected (wire)=
            if Map.tryFind (string wire.InputPort) symbol1.PortMaps.Orientation <> None
            then 
                if Map.tryFind (string wire.OutputPort) symbol2.PortMaps.Orientation <> None
                then Some (wire, (string wire.InputPort), (string wire.OutputPort))
                else None
            else 
                if Map.tryFind (string wire.OutputPort) symbol1.PortMaps.Orientation <> None
                then 
                    if Map.tryFind (string wire.InputPort) symbol2.PortMaps.Orientation <> None
                    then Some (wire, (string wire.OutputPort), (string wire.InputPort))
                    else None
                else None
        let removeOption =
            function
            |Some x -> x
            |None -> failwithf "can't happen"
        List.map (isConnected) (allWires model)
        |> List.filter (fun f -> f <> None) //removes None entries from list
        |> List.map removeOption
//This function returns a list of all the wires connected between two symbols
let getConnectedWires (symbol1: Symbol) (symbol2: Symbol) (model:Model) = 
    let wiresSymbol1 = BusWireUpdateHelpers.getConnectedWires model [symbol1.Id]
    let wiresSymbol2 = BusWireUpdateHelpers.getConnectedWires model [symbol2.Id]
    combineLists wiresSymbol1 wiresSymbol2

//HLP23: AUTHOR Ewan
//Given a wire connected to a symbol, this function returns the associated port
let getPortFromWire (model: SymbolT.Model)(symbol:Symbol) (wire:Wire)=
        let inputPort = Symbol.getInputPortIdStr (wire.InputPort)
        let outputPort = Symbol.getOutputPortIdStr (wire.OutputPort)
        //to check if input or output port on symbol, go through each side of symbol and check if input id is present
        let ports edge = symbol.PortMaps.Order[edge]
        let allPortsOnSymbol = List.collect ports [Left; Right; Top; Bottom]
        if List.exists (fun x -> if x = inputPort then true else false) allPortsOnSymbol
        then 
            Symbol.getPort model inputPort
        else 
            Symbol.getPort model outputPort


//HLP23: AUTHOR Ewan
//Returns true if two wires are intersecting each other and false if they are not
let isInterconnected (fstWire,sndWire)=
        let fstWireAseg = BusWire.getAbsSegments fstWire
        let sndWireAseg = BusWire.getAbsSegments sndWire
        let compareSegments (sndWire: ASegment list) (seg:ASegment) =
            let compareY (seg:ASegment) (wireSeg:ASegment)=
                let isTaller = wireSeg.Start.Y > seg.Start.Y
                if wireSeg.Start.X >= seg.Start.X
                then 
                    if wireSeg.Start.X <= seg.End.X
                    then 
                        [isTaller]
                    else 
                        []
                else 
                    if wireSeg.End.X >= seg.Start.X
                    then 
                        [isTaller]
                    else 

                        []
            List.collect (compareY seg) sndWire
        let lst = List.collect (compareSegments sndWireAseg) fstWireAseg
                               |> List.distinct                      
        if lst.Length <> 1
        then 
            //is interconnected
            true
        else 
            //not interconnected
            false

                    
                        getConnectedWires connectedWires (index+1) symbol1 symbol2 model


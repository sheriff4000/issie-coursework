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
// This function checks if a value is withing the range of two other numbers
let inRange (x: float) (upperLimit: float) (lowerLimit: float) = (x >= upperLimit) && (x <= lowerLimit)

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
    then
        let hScale = ((1.0, symbol.HScale) ||> Option.defaultValue )
        (symbol.Component.W  * hScale) / ( float (maxPortNumber) + 1.0 )
    else
        let vScale = ((1.0, symbol.VScale) ||> Option.defaultValue )
        (symbol.Component.H  * vScale) / ( float (maxPortNumber) + 0.4 )

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
        
        // let edgeName = fst edge[0]
        // if edgeName = Top || edgeName = Right
        // then 
        //     Some ((snd edge[0]).Length - index)
        // else Some (index + 1)

        match fst edge[0] with
        | Top | Right -> Some ((snd edge[0]).Length - index)
        | Bottom | Left -> Some (index + 1)
    else None

// HLP23: Luke
// This function returns if symbol1 is the input port and symbol2 is the output port for the wire passed in
let matchWireIO
    (wire: Wire)
    (symbol1: Symbol)
    (symbol2: Symbol)
    =
    isSymbolInputForWire symbol1 wire && List.contains (string wire.InputPort) (getInputPortIds symbol1) && List.contains (string wire.OutputPort) (getOutputPortIds symbol2)

// HLP23: Luke
// This function returns the edge that a wire is connected to on a symbol
// It assumes the wire is connected as it is currently only being used by getAdjacentConnections, which checks this first
let getWireEdge
    (wire: Wire)
    (symbol: Symbol)
    : Edge
    =
    if isSymbolInputForWire symbol wire
    then symbol.PortMaps.Orientation.Item (string wire.InputPort)
    else symbol.PortMaps.Orientation.Item (string wire.OutputPort)

// HLP23: Luke
// This function returns a list of wires between two components that are connected on two adjascent edges together with the
// edges the wire is connected to
let getAdjacentConnections
    (model: Model)
    (symbol1: Symbol)
    (symbol2: Symbol)
    : ((Edge*Edge) * Wire list) option =

    let wires =
        model.Wires
        |> Map.toList
        |> List.filter(fun (_,wire) ->
            if (matchWireIO wire symbol1 symbol2 || matchWireIO wire symbol2 symbol1)
            then
                match (getWireEdge wire symbol1), (getWireEdge wire symbol2) with
                | Top,Bottom | Bottom,Top | Left,Right | Right,Left -> true
                | _ -> false
            else
                false
            )
        |> List.map(fun (_,wire) -> wire)
    match wires with
    | hd::_ -> Some ( ((getWireEdge hd symbol1), (getWireEdge hd symbol2)), wires )
    | _ -> None
    
    
    // |> List.map(fun (_,wire) ->
    //     ((getWireEdge wire symbol1),(getWireEdge wire symbol2)),wire)

// HLP23: Luke
// This returns the X and Y posistion of a port.
// The port distance on the edge of the port is inputted as this is needed for smartResize.
// A second version of this function could be made easily without the need of inputting the port distance.
// (As shown by the commented out line)
let getPortXYPos
    (wire: Wire)
    (symbol: Symbol)
    (edgePortDistance: float)
    : XYPos
    =
    let symbolPortNumberFloat = 
        getPortPositionFromTopOrLeft symbol wire
        |> Option.get
        |> float
    let wireEdge = getWireEdge wire symbol
    // let edgePortDistance = getPortDistances symbol wireEdge

    match wireEdge with
    | Top -> {X = symbol.Pos.X + edgePortDistance*symbolPortNumberFloat; Y = symbol.Pos.Y}
    | Bottom -> {X = symbol.Pos.X + edgePortDistance*symbolPortNumberFloat; Y = symbol.Pos.Y + symbol.Component.H}
    | Left -> {X = symbol.Pos.X; Y = symbol.Pos.Y + edgePortDistance*(symbolPortNumberFloat-0.3)}
    | Right -> {X = symbol.Pos.X + symbol.Component.W; Y = symbol.Pos.Y + edgePortDistance*(symbolPortNumberFloat-0.3)}

// HLP23: Luke
// It also returns if the two symbols inputted overlap in the horizontal/vertical direction.
// (which one is set by bool input)
let inDeadZone
    (symbol1: Symbol)
    (symbol2: Symbol)
    (verticalOrHorizontal: bool)
    : bool =

    let xOrYPos symbol = if verticalOrHorizontal then symbol.Pos.X else symbol.Pos.Y
    let hOrW symbol = if verticalOrHorizontal then symbol.Component.W else symbol.Component.H
    let hOrVScale symbol = if verticalOrHorizontal then symbol.HScale else symbol.VScale

    let scale1 = (1.0, hOrVScale symbol1) ||> Option.defaultValue
    let scale2 = (1.0, hOrVScale symbol2) ||> Option.defaultValue

    // bOrR means Bottom or Right, and tOrL means Top or Left
    let tOrLSymbol1 = xOrYPos symbol1
    let tOrLSymbol2 = xOrYPos symbol2
    let bOrRSymbol1 = tOrLSymbol1 + (hOrW symbol1)*scale1
    let bOrRSymbol2 = tOrLSymbol2 + (hOrW symbol2)*scale2

    inRange bOrRSymbol1 tOrLSymbol2 bOrRSymbol2
    || inRange tOrLSymbol1 tOrLSymbol2 bOrRSymbol2 
    || inRange bOrRSymbol2 tOrLSymbol1 bOrRSymbol1
    || inRange tOrLSymbol2 tOrLSymbol1 bOrRSymbol1


// HLP23: Luke
// This returns on number of ports on the edge of a symbol
let portsOnEdge
    (symbol: Symbol)
    (edge: Edge)
    =
    symbol.PortMaps.Order.Item edge
    |> List.length

// HLP23: Luke
// This function will return the port ID when given a symbol, edge and the port posistion "distance" from the Top/Left (depenind on the edge)
// This is to be used after getPortPositionFromTopOrLeft to go back to the correct port ordering
let getPortIDFromTopOrLeft
    (symbol: Symbol)
    (edge: Edge)
    (portNumber: int)
    : string
    =
    let portIndex = 
        match edge with
        | Top | Right -> portsOnEdge symbol edge - portNumber
        | Bottom | Left -> portNumber - 1

    Map.find edge symbol.PortMaps.Order
    |> List.item portIndex

// HLP23: Luke
// This function takes in two symbols and returns the port ids for symbolToChange to swap so all wires are
// as "straight as they can be"/have the smallest middle segment.
// This function is intended to be used with SmartPortReordering so SmartSizing can create straight wires
let portMapping
    (wModel: Model)
    (symbolToChange: Symbol)
    (otherSymbol: Symbol)
    : (string*string) list
    =
    let adjacentConnections = getAdjacentConnections wModel symbolToChange otherSymbol

    match adjacentConnections with
    | Some ((symbolEdge,_),wires) ->
        // let symbolEdge = adjacentConnections |> fst |> fst

        let portsSymbol = portsOnEdge symbolToChange symbolEdge

        let rec portMap i posList =
            match posList with
            | (pos1,pos2)::tl ->
                if (pos2 > portsSymbol-i)
                then List.append [pos1,portsSymbol-i] (portMap (i+1) tl)
                else List.append [pos1,pos2] (portMap i tl)
            | [] -> []

        wires
        |> List.map (fun wire ->
            Option.get (getPortPositionFromTopOrLeft symbolToChange wire),
            Option.get (getPortPositionFromTopOrLeft otherSymbol wire) )
        |> List.sortByDescending id
        |> portMap 0
        |> List.filter (fun (x,y) -> x <> y)
        |> List.map (fun (x,y) ->
            getPortIDFromTopOrLeft symbolToChange symbolEdge x
            , getPortIDFromTopOrLeft symbolToChange symbolEdge y)
    | None -> []


//HLP23: AUTHOR Ewan
//This function returns a list of all the elements in both input lists
let combineLists (list1: 'a List) (list2: 'a List) = 
        List.allPairs list1 list2
        |> List.filter (fun (x,y) -> x = y)
        |> List.map fst 

//HLP23: AUTHOR Ewan
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

                    


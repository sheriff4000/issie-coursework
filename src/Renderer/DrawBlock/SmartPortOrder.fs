module SmartPortOrder
open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators



(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)
type BusWireHelpers = {
    UpdateWire: BusWireT.Model -> Wire -> bool -> Wire
    UpdateWires: BusWireT.Model -> List<ComponentId> -> XYPos -> BusWireT.Model
    UpdateSymbolWires: BusWireT.Model -> ComponentId -> BusWireT.Model
    }
/// To test this, it must be given two symbols interconnected by wires. It then reorders the ports on
/// symbolToOrder so that the connecting wires do not cross.
/// Tt should work out the interconnecting wires (wiresToOrder) from 
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).
/// 

let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol)
    (helpers: BusWireHelpers)
        : BusWireT.Model =
    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label}, Other:{otherSymbol.Component.Label}"
    let sModel = wModel.Symbol

    let rec compareSegments (sndWire: ASegment list) index (boolList: bool list) (seg:ASegment)=
        if sndWire[index].Start.X >= seg.Start.X
        then 
            if sndWire[index].Start.X <= seg.End.X
            then 
                let isTaller =
                    if sndWire[index].Start.Y > seg.Start.Y
                    then true
                    else false
                let isTallerList = List.append boolList [isTaller]
                if index = sndWire.Length - 2
                then 
                    isTallerList
                else 
                    let newIndex = index + 1
                    compareSegments sndWire (newIndex) isTallerList seg
            else 
                if index = sndWire.Length - 2
                then 
                    boolList
                else 
                    let newIndex = index + 1
                    compareSegments sndWire (newIndex) boolList seg
                
        else 
            if sndWire[index].End.X >= seg.Start.X
            then 
                let isTaller =
                    if sndWire[index].Start.Y > seg.Start.Y
                    then true
                    else false
                let isTallerList = List.append boolList [isTaller]
                if index = sndWire.Length - 2
                then 
                    isTallerList
                else 
                    let newIndex = index + 1
                    compareSegments sndWire (newIndex) isTallerList seg
            else 
                if index = sndWire.Length - 2
                then 
                    boolList
                else 
                    let newIndex = index + 1
                    compareSegments sndWire (newIndex) boolList seg
    //remove duplicate code in above function

    let getPortFromWire (model: SymbolT.Model)(symbol:Symbol) (wire:Wire)=
        let inputPort = Symbol.getInputPortIdStr (wire.InputPort)
        let outputPort = Symbol.getOutputPortIdStr (wire.OutputPort)
        //to check if input or output port on symbol, go through each side of symbol and check if input id is present
        let ports edge = symbol.PortMaps.Order[edge]
        let allPortsOnSymbol = List.collect ports [Left; Right; Top; Bottom]
        printfn $"allPorts on symbol {allPortsOnSymbol}"
        if List.exists (fun x -> if x = inputPort then true else false) allPortsOnSymbol
        then 
            printfn $"HEY THERE ALEERA"
            printfn $"HEY THERE {inputPort}"
            Symbol.getPort model inputPort
        else 
            printfn $"GOODBYE ALEERA"
            printfn $"What was input {inputPort}"
            printfn $"What was input {outputPort}"
            printfn $"Wire id {wire.WId}"
            Symbol.getPort model outputPort

    let swapPorts (symbol:Symbol) (port1:Port) (port2:Port) =
        printfn $"CHECK ALEERA {port2.Id}"
        printfn $"CHECK ALEERA {port1.Id}"
        let ports = symbol.PortMaps.Order[Left]
        printfn $"PORT LIST: {ports}"
        
        let newPos = symbol.Pos + (getPortPos symbol port2)
        let oldPos = symbol.Pos + (getPortPos symbol port1)
        let buffer = {
            X = 
                if newPos.X > oldPos.X
                then 25.0
                else 
                    if newPos.X < oldPos.X
                    then -25.0
                    else 0
            Y = 
                if newPos.Y > oldPos.Y
                then 25.0
                else   
                    if newPos.Y < oldPos.Y
                    then -25.0
                    else 0.0
        }
        let newPos' = newPos + buffer
        
        
        printfn $"New position: {newPos'}"
        printfn $"Old position: {oldPos}"
        SymbolUpdatePortHelpers.updatePortPos symbol newPos' port1.Id
        
    let isInterconnected  (index) (symbol:Symbol) (fstWire,sndWire)=
        printfn $"Wire id first: {fstWire.WId}"
        let fstWireAseg = BusWire.getAbsSegments fstWire
        let sndWireAseg = BusWire.getAbsSegments sndWire
        let lst = List.collect (compareSegments sndWireAseg 0 []) fstWireAseg
                             |> List.distinct
                                
        printfn $"list of segments: {lst}"
        if lst.Length <> 1
        then 
            //is interconnected
            printfn "interconnected"
            let port1 = getPortFromWire sModel symbol fstWire
            printfn $"get port from wire {port1.Id}"
            let port2 = getPortFromWire sModel symbol sndWire
            
            swapPorts symbol port1 port2
            
            
            //true
        else 
            //is not interconnected
            printfn "not inter"
            symbol

//input = all wires connected between the two relevant symbols 
// if this doesnt work then make a single list of interconnected wires by returning true for that wire if there are any interconnections
    let getWirePairs (wireList: Wire List) = 
        let isNotDuplicate (x,y) = 
            if x <> y
            then true
            else false
        let orderTupleByWireId (x, y) = 
            if x.WId < y.WId 
            then (x,y)
            else (y,x)
        printfn $"DOUBLE DOUBLE CHECK: {wireList|> List.allPairs wireList 
        |> List.filter isNotDuplicate }"
        wireList
        |> List.allPairs wireList 
        |> List.filter isNotDuplicate
        |> List.map orderTupleByWireId
        |> List.distinct

    let rec getAllInterconnected (symbol:Symbol) (index:int) (wireList: Wire List):Symbol = 
        let wirePairs = (getWirePairs wireList)//need to change to all connected wires
        printfn $"Wire PAIRS : {wirePairs}"
        let newSymbol = isInterconnected 0 symbol wirePairs[index]
        printfn $"After: {newSymbol.PortMaps.Order}"
        printfn $"Before: {symbol.PortMaps.Order}"
        let newWireList =
            if newSymbol <> symbol
            then 
                printfn "REPEAT LOOP"
                (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add newSymbol.Id newSymbol sModel.Symbols}})  symbolToOrder.Id)
                |> SmartHelpers.getConnectedWires [] 0 symbolToOrder otherSymbol
                
            else 
                wireList

        let changedIndex =
            if newSymbol.PortMaps.Order <> symbol.PortMaps.Order
            then 
                0
            else 
                index + 1
        if (index < wirePairs.Length-1)//need to go down list: if is interconnected = true then swap occurs
        then 
            let newIndex = index + 1
            getAllInterconnected newSymbol newIndex newWireList
        else
            newSymbol

    let wiresToOrder = []
    let componentList = [symbolToOrder.Id; otherSymbol.Id]
    
    
    let initialList: Wire List = []
    let initialIndex = 0
    let wires: Wire List = SmartHelpers.getConnectedWires initialList initialIndex symbolToOrder otherSymbol wModel    
        
    printfn $"CHECK wire list : {wires}"
    let isInterconnected2 (fstWire,sndWire)=
        printfn $"Wire id first: {fstWire.WId}"
        let fstWireAseg = BusWire.getAbsSegments fstWire
        let sndWireAseg = BusWire.getAbsSegments sndWire
        let lst = List.collect (compareSegments sndWireAseg 0 []) fstWireAseg
                             |> List.distinct
                                
        printfn $"list of segments: {lst}"
        if lst.Length <> 1
        then 
            //is interconnected
            printfn "interconnected"
            true
        else 
            printfn "not interconnected"
            false
    let anyInterconnected (wire: Wire List)=
        wire
        |> getWirePairs
        |> List.map isInterconnected2
        |> List.exists (fun x -> x = true)

    //input each wire from connected wire list
    let anyInterconnected' (symbol1:Symbol) (symbol2:Symbol)  (model: SymbolT.Model) (wire: Wire)=
        let port1Pos  =  
                let port1 = (getPortFromWire model symbol1 wire)
                printfn "hello arrived"
                SymbolUpdatePortHelpers.getPosIndex symbol1 (getPortPos symbol1 port1) (symbol1.PortMaps.Orientation[port1.Id])
        printfn "MADE PORT1 POS"
        let port2Pos = 
                let port2 = (getPortFromWire model symbol2 wire)
                printfn "hello arrived 2"
                SymbolUpdatePortHelpers.getPosIndex symbol2 (getPortPos symbol2 port2) (symbol2.PortMaps.Orientation[port2.Id])
        printfn "MADE PORT2 POS"
        port1Pos, port2Pos

    let changePortEdge (edge:Edge) (symbol:Symbol) (portId:string) =
        let h,w = getRotatedHAndW symbol
        let getXY (x,y) = {
            X = x
            Y = y
            }
        printfn $"GRAZIE {symbol.Pos + getXY (w, (-h/2.0))}"
        printfn $"{symbol.Pos}"
        printfn $"{h} {w}"
        let newPosition = 
            match edge with
            |Top -> symbol.Pos + getXY ((w/2.0),0.0)
            |Left -> symbol.Pos + getXY (0, (h/2.0))
            |Bottom -> symbol.Pos + getXY ((w/2.0), h)
            |Right -> symbol.Pos + getXY (w, (h/2.0))
        SymbolUpdatePortHelpers.updatePortPos symbol newPosition portId

    let comparePortEdge   (otherSymbol:Symbol) (model: SymbolT.Model) (symbolToChange:Symbol) (wire:Wire) =
        let port1 = getPortFromWire model symbolToChange wire
        let port2 = getPortFromWire model otherSymbol wire
        let portEdge = symbolToChange.PortMaps.Orientation[port1.Id]
        printfn $"BONJOUR CIAO"
        match otherSymbol.PortMaps.Orientation[port2.Id] with
        |Top -> if portEdge = Bottom then symbolToChange else 
                                                              
                                                              changePortEdge Bottom symbolToChange (port1.Id)
        |Left -> if portEdge = Right then symbolToChange else 
                                                              
                                                              changePortEdge Right symbolToChange (port1.Id)
        |Bottom -> if portEdge = Top then symbolToChange else changePortEdge Top symbolToChange (port1.Id)
        |Right -> if portEdge = Left then symbolToChange else 
                                                              printfn "CHANGED TO RIGHT BONJOUR"
                                                              changePortEdge Left symbolToChange (port1.Id)
    let checkPortPositions (wire:Wire List) (symbol1: Symbol) (model:DrawModelType.SymbolT.Model)=
        let secondSymbolList = 
            wire
            |> List.map (anyInterconnected' symbol1 otherSymbol model)
            |> List.sort
            |> List.unzip
            |> (fun (x,y) -> y)
        printfn $"check port positions{wire
            |> List.map (anyInterconnected' symbol1 otherSymbol model)}"
        printfn $"check after sorted { wire
            |> List.map (anyInterconnected' symbol1 otherSymbol model)
            |> List.sort}"
        printfn $"SECOND SYMBOL LIST {secondSymbolList}"
        printfn $"SECOND SYMBOL LIST SORTED {List.sortDescending secondSymbolList}"
        if (List.sortDescending secondSymbolList) = secondSymbolList
        then 
            //no interconnections
            false
        else
            true
            //interconnections

    
    (*let rec changeSymbol (symbol:Symbol) (symbolWires:Wire List) (index:int) = 
        
            if index > 10//timeout
            then 
                printfn "STUCK IN LOOP"
                symbol
            else 
                if anyInterconnected wires = true
                    then
                        printfn "INTERCONNECTED"
                        if checkPortPositions wires symbol (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add symbol.Id symbol sModel.Symbols}})  symbol.Id).Symbol= false
                            then
                                
                                symbol
                            else
                                let newSymbol = getAllInterconnected symbol 0 symbolWires
                                let changedWires = (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add newSymbol.Id newSymbol sModel.Symbols}})  newSymbol.Id)
                                                 |> SmartHelpers.getConnectedWires [] 0 symbolToOrder otherSymbol
                                let newIndex = index + 1
                                changeSymbol newSymbol changedWires newIndex
                    else 
                        symbol*)
   
    //let finalSymbol = changeSymbol symbolToOrder wires 0
   // let changeWires = (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add finalSymbol.Id finalSymbol sModel.Symbols}})  finalSymbol.Id)
     //                      |> SmartHelpers.getConnectedWires [] 0 symbolToOrder otherSymbol

     
   // let symbol' = getAllInterconnected symbolToOrder 0 wires// no change at the moment
    
  //  let changedWires = helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}})  symbolToOrder.Id
    (*let tempModel = {wModel with 
                            Wires = changedWires.Wires//wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                                                 // to make that happen the tyest function which calls this would need to provide an updateWire
                                                 // function to this as a parameter (as was done in Tick3)
                            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }*)
  //  let connectedWires' = SmartHelpers.getConnectedWires [] 0 symbol' otherSymbol tempModel
    let rec changeSymbol (symbol':Symbol)(wires:Wire List) = 
        match anyInterconnected wires with
        | true -> match (checkPortPositions wires symbol' (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}})  symbol'.Id).Symbol) with
                    | false -> symbol'
                    | true -> 
                              let newSymbol = getAllInterconnected symbol' 0 wires
                              let newWires = (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add newSymbol.Id newSymbol sModel.Symbols}})  symbolToOrder.Id) |> SmartHelpers.getConnectedWires [] 0 newSymbol otherSymbol
                              changeSymbol newSymbol newWires
        | false -> symbol'
    let reOrderPortEdges = List.fold (comparePortEdge otherSymbol sModel) symbolToOrder wires
    let newWires = (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add reOrderPortEdges.Id reOrderPortEdges sModel.Symbols}})  symbolToOrder.Id) |> SmartHelpers.getConnectedWires [] 0 reOrderPortEdges otherSymbol
    //input wire output symbol
    //symbol = new symbol
    //let newWires = helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add new.Id finalSymbol' sModel.Symbols}})  symbolToOrder.Id
    let finalSymbol' = changeSymbol reOrderPortEdges newWires
    let newChangedWires = helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols}})  symbolToOrder.Id


    {wModel with 
        Wires = newChangedWires.Wires//wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                             // to make that happen the tyest function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols}
    }

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
    //change below to functional programming
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
    let compareSegments' (sndWire: ASegment list) (seg:ASegment) =
        let testFunction (seg:ASegment) (wireSeg:ASegment)=
            let isTaller =
                        if wireSeg.Start.Y > seg.Start.Y
                        then true
                        else false
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
        List.collect (testFunction seg) sndWire
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

    let swapPorts (symbol:Symbol) (port1:Port) (port2:Port) =
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

        SymbolUpdatePortHelpers.updatePortPos symbol newPos' port1.Id

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

        wireList
        |> List.allPairs wireList 
        |> List.filter isNotDuplicate
        |> List.map orderTupleByWireId
        |> List.distinct

    let isInterconnected (fstWire,sndWire)=
        let fstWireAseg = BusWire.getAbsSegments fstWire
        let sndWireAseg = BusWire.getAbsSegments sndWire
        //let lst = List.collect (compareSegments sndWireAseg 0 []) fstWireAseg
          //                   |> List.distinct
        let lst = List.collect (compareSegments' sndWireAseg) fstWireAseg
                               |> List.distinct                      
        if lst.Length <> 1
        then 
            //is interconnected
            printfn "inter"
            true
        else 
            //not interconnected
            printfn "not not inter"
            false


    let swapInterconnectedPorts  (model:SymbolT.Model) (symbol:Symbol) (fstWire,sndWire)=
        match isInterconnected (fstWire,sndWire) with
        | false -> symbol
        | true ->
                 let port1 = getPortFromWire model symbol fstWire
                 let port2 = getPortFromWire model symbol sndWire
         
                 swapPorts symbol port1 port2

    let getAllInterconnected'' (model: Model) (symbol:Symbol) (wireList:Wire List) = 
        let wirePairs = (getWirePairs wireList)
        let newSymbol  (symbol1: Symbol) (model:Model) (pair: Wire * Wire)= 
            let changeSymbol = swapInterconnectedPorts model.Symbol model.Symbol.Symbols[symbol1.Id] pair
            if changeSymbol <> model.Symbol.Symbols[symbol1.Id]
            then 
                let newWireModel = (helpers.UpdateSymbolWires ({model with Symbol = {model.Symbol with Symbols = Map.add changeSymbol.Id changeSymbol model.Symbol.Symbols}})  symbolToOrder.Id)
  
                let newWireList = 
                                  printfn "REPEAT LOOP"
                                  newWireModel
                                  |> SmartHelpers.getConnectedWires symbolToOrder otherSymbol
                //getAllInterconnected'' newWireModel changeSymbol newWireList
                newWireModel
                //if this changes then need to go through list again?
            else 
                printfn "END END LOOP"
                model
                //symbol
        
        List.fold (newSymbol symbol) model wirePairs
    
        

(*    let rec getAllInterconnected' (model:Model) (symbol:Symbol) (wireList: Wire List) :Symbol =
        let wirePairs = (getWirePairs wireList)
        let newSymbol (model:Model) (symbol1: Symbol) (pair: Wire * Wire)= 
            let changeSymbol = swapInterconnectedPorts model.Symbol symbol1 pair
            if changeSymbol <> symbol1
            then 
                let newWireModel = (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add changeSymbol.Id changeSymbol sModel.Symbols}})  symbolToOrder.Id)
  
                let newWireList = 
                                  printfn "REPEAT LOOP"
                                  (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add changeSymbol.Id changeSymbol sModel.Symbols}})  symbolToOrder.Id)
                                  |> SmartHelpers.getConnectedWires symbolToOrder otherSymbol
                getAllInterconnected' newWireModel changeSymbol newWireList

                //if this changes then need to go through list again?
            else 
                symbol
        List.fold (newSymbol model) symbol wirePairs*)
            
        
    
    let wiresToOrder = []
    let componentList = [symbolToOrder.Id; otherSymbol.Id]
    let wires: Wire List = SmartHelpers.getConnectedWires symbolToOrder otherSymbol wModel    

    let anyInterconnected (wire: Wire List)=
        wire
        |> getWirePairs
        |> List.map isInterconnected
        |> List.exists (fun x -> x = true)

    //input each wire from connected wire list
    let anyInterconnected' (symbol1:Symbol) (symbol2:Symbol)  (model: SymbolT.Model) (wire: Wire)=
        let port1Pos  =  
                let port1 = (getPortFromWire model symbol1 wire)
                SymbolUpdatePortHelpers.getPosIndex symbol1 (getPortPos symbol1 port1) (symbol1.PortMaps.Orientation[port1.Id])
 
        let port2Pos = 
                let port2 = (getPortFromWire model symbol2 wire)
                SymbolUpdatePortHelpers.getPosIndex symbol2 (getPortPos symbol2 port2) (symbol2.PortMaps.Orientation[port2.Id])

        port1Pos, port2Pos
        
    let changePortEdge (edge:Edge) (symbol:Symbol) (portId:string) =
        let h,w = getRotatedHAndW symbol
        let getXY (x,y) = {
            X = x
            Y = y
            }
        let newPosition = 
            match edge with
            |Top -> symbol.Pos + getXY ((w/2.0),0.0)
            |Left -> symbol.Pos + getXY (0, (h/2.0))
            |Bottom -> symbol.Pos + getXY ((w/2.0), h)
            |Right -> symbol.Pos + getXY (w, (h/2.0))
        SymbolUpdatePortHelpers.updatePortPos symbol newPosition portId
    
    let connectedPorts (symbol1: Symbol) (symbol2: Symbol) (model: SymbolT.Model) (wireModel: Model)=
        let wires = SmartHelpers.getConnectedWires symbol1 symbol2 wireModel
        List.map (getPortFromWire model symbol2) wires

    //added functionality for future: will change positioning based on symbol positioning e.g. is one symbol above the other
    let comparePortEdge   (otherSymbol:Symbol) (model: SymbolT.Model) (symbolToChange:Symbol) (wire:Wire) =
        let getPorts = List.map (fun (x: Port) -> string x.Id) (connectedPorts symbolToChange otherSymbol model wModel)
        printfn $"GET PORTS{(getPorts)}"
        let port1 = getPortFromWire model symbolToChange wire
        let port2 = getPortFromWire model otherSymbol wire
        let portEdge = symbolToChange.PortMaps.Orientation[port1.Id]
        let largestEdge = 
                        let TopList = otherSymbol.PortMaps.Order[Top] |> SmartHelpers.combineLists getPorts
                        printfn $"{TopList}"
                        let BottomList = otherSymbol.PortMaps.Order[Bottom] |> SmartHelpers.combineLists getPorts
                        let LeftList = otherSymbol.PortMaps.Order[Left] |> SmartHelpers.combineLists getPorts
                        let RightList = otherSymbol.PortMaps.Order[Right] |> SmartHelpers.combineLists getPorts
                        let PortList = [TopList, Top; BottomList, Bottom; LeftList, Left; RightList,Right]
                        List.filter (fun  ((x:string List),y) -> x.Length > 0) PortList
                        |> List.sortByDescending (fun ((x:string List),y) -> x.Length) 
        
        let changeEdge (edge:Edge)=
            match edge with
                |Top -> if portEdge = Bottom then symbolToChange else changePortEdge Bottom symbolToChange (port1.Id)
                |Left -> if portEdge = Right then symbolToChange else changePortEdge Right symbolToChange (port1.Id)
                |Bottom -> if portEdge = Top then symbolToChange else changePortEdge Top symbolToChange (port1.Id)
                |Right -> if portEdge = Left then symbolToChange else printfn "GUTEN TAG"
                                                                      changePortEdge Left symbolToChange (port1.Id)
        if largestEdge.Length = 1
        then 
            changeEdge otherSymbol.PortMaps.Orientation[port2.Id]
        else
            if (fst largestEdge[0]).Length = (fst largestEdge[1]).Length
            then                
                changeEdge otherSymbol.PortMaps.Orientation[port2.Id]
            else 
                changeEdge (snd largestEdge[0])
        
    let checkPortPositions (wire:Wire List) (symbol1: Symbol) (model:DrawModelType.SymbolT.Model)=
        let secondSymbolList = 
            wire
            |> List.map (anyInterconnected' symbol1 otherSymbol model)
            |> List.sort
            |> List.unzip
            |> (fun (x,y) -> y)
        if (List.sortDescending secondSymbolList) = secondSymbolList
        then 
            //correct port indexing order
            false
        else
            true
            //incorrect port indexing order

    
    let rec changeSymbol (symbol':Symbol) (wires:Wire List) (model:Model) = 
        match anyInterconnected wires with
        | true -> match (checkPortPositions wires symbol' (helpers.UpdateSymbolWires ({model with Symbol = {model.Symbol with Symbols = Map.add symbol'.Id symbol' model.Symbol.Symbols}})  symbol'.Id).Symbol) with
                    | false -> symbol'
                    | true -> 
                              //let newSymbol = getAllInterconnected symbol' 0 wires
                              printfn "CHECK HERE EWAN"
                              //let newSymbol = getAllInterconnected' model symbol' wires
                              //let newWireModel = (helpers.UpdateSymbolWires ({model with Symbol = {model.Symbol with Symbols = Map.add newSymbol.Id newSymbol model.Symbol.Symbols}})  symbolToOrder.Id) 
                              let newWireModel = getAllInterconnected'' model symbol' wires
                              let newSymbol = newWireModel.Symbol.Symbols[symbol'.Id]
                              let newWires = newWireModel |> SmartHelpers.getConnectedWires newSymbol otherSymbol
                              changeSymbol newSymbol newWires newWireModel
        | false -> symbol'
    let reOrderPortEdges = List.fold (comparePortEdge otherSymbol sModel) symbolToOrder wires
    let newWireModel = (helpers.UpdateSymbolWires ({wModel with Symbol = {sModel with Symbols = Map.add reOrderPortEdges.Id reOrderPortEdges sModel.Symbols}})  symbolToOrder.Id) 
  //  let newWires = newWireModel|> SmartHelpers.getConnectedWires reOrderPortEdges otherSymbol

   // let finalSymbol' = changeSymbol reOrderPortEdges newWires
 //   let newChangedWires = helpers.UpdateSymbolWires ({newWireModel with Symbol = {newWireModel.Symbol with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols}})  symbolToOrder.Id


   (* {wModel with 
        Wires = newChangedWires.Wires//wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                             // to make that happen the tyest function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols}
    }*)     

    let changedModel = {wModel with 
                            Wires = newWireModel.Wires//wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                                                 // to make that happen the tyest function which calls this would need to provide an updateWire
                                                 // function to this as a parameter (as was done in Tick3)
                            Symbol = {sModel with Symbols = Map.add reOrderPortEdges.Id reOrderPortEdges sModel.Symbols}
                        }
    let newWires = changedModel|> SmartHelpers.getConnectedWires reOrderPortEdges otherSymbol
    printfn "AFROGALA"
    let finalSymbol' = changeSymbol reOrderPortEdges newWires changedModel
    let newChangedWires = helpers.UpdateSymbolWires ({changedModel with Symbol = {changedModel.Symbol with Symbols = Map.add finalSymbol'.Id finalSymbol' changedModel.Symbol.Symbols}})  symbolToOrder.Id

    {wModel with 
        Wires = newChangedWires.Wires//wModel.Wires // no change for now, but probably this function should use update wires after reordering.
                             // to make that happen the tyest function which calls this would need to provide an updateWire
                             // function to this as a parameter (as was done in Tick3)
        Symbol = {sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols}
    }  

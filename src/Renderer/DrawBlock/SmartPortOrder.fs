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
open BusWire



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

    let updateWires (symbol:Symbol) (model:Model) (originalSymbol:Symbol) = 
        (helpers.UpdateSymbolWires ({model with Symbol = {model.Symbol with Symbols = Map.add symbol.Id symbol model.Symbol.Symbols}})  originalSymbol.Id) 
    
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


    let swapInterconnectedPorts  (model:SymbolT.Model) (symbol:Symbol) (fstWire,sndWire)=
        match SmartHelpers.isInterconnected (fstWire,sndWire) with
        | false -> symbol
        | true ->
                 let port1 = SmartHelpers.getPortFromWire model symbol fstWire
                 let port2 = SmartHelpers.getPortFromWire model symbol sndWire
         
                 swapPorts symbol port1 port2

    let getAllInterconnected (model: Model) (symbol:Symbol) (wireList:Wire List) = 
        let wirePairs = (getWirePairs wireList)
        let newSymbol  (symbol1: Symbol) (model:Model) (pair: Wire * Wire)= 
            let changeSymbol = swapInterconnectedPorts model.Symbol model.Symbol.Symbols[symbol1.Id] pair
            if changeSymbol <> model.Symbol.Symbols[symbol1.Id]
            then 
                let newWireModel = updateWires changeSymbol model symbolToOrder
                let newWireList = 
                                  newWireModel
                                  |> SmartHelpers.getConnectedWires symbolToOrder otherSymbol
                newWireModel
            else 
                model        
        List.fold (newSymbol symbol) model wirePairs

    let wiresToOrder = []
    let componentList = [symbolToOrder.Id; otherSymbol.Id]
    let wires: Wire List = SmartHelpers.getConnectedWires symbolToOrder otherSymbol wModel    

    let anyInterconnected (wire: Wire List)=
        wire
        |> getWirePairs
        |> List.map SmartHelpers.isInterconnected
        |> List.exists (fun x -> x = true)
    let isLeft (symbol1:Symbol) (symbol2:Symbol)=
            if symbol1.Pos.X > symbol2.Pos.X
            then false
            else true

    //input each wire from connected wire list
    let anyCrossingPorts (symbol1:Symbol) (symbol2:Symbol)  (model: SymbolT.Model) (wire: Wire)=
        let port1Pos  =  
                let port1 = (SmartHelpers.getPortFromWire model symbol1 wire)
                SymbolUpdatePortHelpers.getPosIndex symbol1 (getPortPos symbol1 port1) (symbol1.PortMaps.Orientation[port1.Id])
 
        let port2Pos = 
                let port2 = (SmartHelpers.getPortFromWire model symbol2 wire)
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
        List.map (SmartHelpers.getPortFromWire model symbol2) wires

    let getSymbolPos (symbol1: Symbol) (symbol2: Symbol) = //gets symbol positioning with respect to the other
        let xDifference = symbol1.Pos.X - symbol2.Pos.X
        let yDifference = symbol1.Pos.Y - symbol2.Pos.Y
        if (abs xDifference) < (abs yDifference)
        then
            printfn "Vertical"
            if yDifference > 0
            then 
                //symbol1 is above symbol 2
                "Top"
            else
                //symbol1 is below symbol 2
                "Bottom"
        else 
            printfn "Horizontal"
            if xDifference > 0 
            then
                //symbol1 is to the left of symbol1
                "Left"
            else
                //symbol1 is to the right of symbol2
                "Right"
    printfn $"{(getSymbolPos symbolToOrder otherSymbol)}"
    //get wires that are not connected to both symbols by getting wires only connected to that one symbol
    //maybe use list.filter

    let reorderUnconnectedWires (otherSymbol:Symbol) (symbolToChange:Symbol) (model:SymbolT.Model) (wireModel:Model) = 
        let getSingleConnectedWires = //wires only connected to symbolToChange
            let connectedWires = SmartHelpers.getConnectedWires otherSymbol symbolToChange wireModel
            let symbolToChangeWires = BusWireUpdateHelpers.getConnectedWires wireModel [symbolToChange.Id]
            SmartHelpers.listDifference symbolToChangeWires connectedWires
        //get the list of all the ports from port map and then swap each port with index 1
        //Also only necessary if connected wires are on the same edge
        //go through each edge - if there is a connected wire on this edge + a single connected wire
        //swap the order: either index 1 or last index depending on symbol positioning
        let testFunction = 
            match (getSymbolPos symbolToChange otherSymbol) with //need to make the output of this either edge or new type with four options
            | "Top" -> 0
            | "Bottom" -> 0
            | "Left" -> 0 
            | "Right"-> 0
        //need to get second symbol connected to wire
        //input = wire and symbol
        //output = second symbol
        let getSecondSymbol = 0
        //input should be edge and symbolToChange
        //output should be symbol
        //List.fold

        //

        let ports symbol edge = 
            let portsList = List.map (SmartHelpers.getPortFromWire model symbolToChange) getSingleConnectedWires |> List.map (fun (x:Port) -> x.Id)
            let portListEdge = symbolToChange.PortMaps.Order[edge] 
            //need to create another function that gets a symbol from a wire and another symbol
            
            let additionalSymbol = SmartHelpers.getSymbolFromWire model symbolToChange
            if (SmartHelpers.combineLists portsList portListEdge).Length <> 0//if there is a port that is connected to a different symbol
            then 
                //need to get list of ports on this edge
                let portListSingleConnected = SmartHelpers.combineLists portsList portListEdge
                                              |> List.map (fun (x:string) -> model.Ports[x])
                let test2  symbol (port:Port)= 
                    /// Returns the IDs of the wires in the model connected to a list of components given by compIds
                    let wire =
                        let containsPorts (wire:Wire) =
                            if (Symbol.getInputPortIdStr wire.InputPort) <> port.Id
                            then 
                                if (Symbol.getOutputPortIdStr wire.OutputPort) <> port.Id
                                then false
                                else true
                            else
                                true                              

                        let test = wireModel.Wires
                                    |> Map.toList
                                    |> List.map (fun (x,y) -> y)
                                    |> List.filter containsPorts
                        test[0]
                    //let wire = wireModel.Wires[port.]
                    match (getSymbolPos symbolToChange otherSymbol) with
                        | "Top" -> match edge with 
                                   | Top -> 
                                            printfn("CHECK1")
                                            if (additionalSymbol wire).Pos.X > symbolToChange.Pos.X
                                            then swapPorts symbol port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbol port (Symbol.getPort model (List.last portListEdge))
                                   | Bottom -> 
                                            printfn("CHECK2")
                                            if (additionalSymbol wire).Pos.X > symbolToChange.Pos.X
                                            then swapPorts symbolToChange port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbolToChange port (Symbol.getPort model (List.last portListEdge))
                                   | _ -> symbol
                        | "Bottom" -> match edge with 
                                   | Top -> 
                                            printfn("CHECK3")
                                            if (additionalSymbol wire).Pos.X > symbolToChange.Pos.X
                                            then swapPorts symbolToChange port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbolToChange port (Symbol.getPort model (List.last portListEdge))
                                   | Bottom -> 
                                            printfn("CHECK4")
                                            if (additionalSymbol wire).Pos.X > symbolToChange.Pos.X
                                            then swapPorts symbol port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbol port (Symbol.getPort model (List.last portListEdge))
                                   | _ -> symbol
                        | "Left" -> match edge with 
                                   | Left -> 
                                            printfn("CHECK5")
                                            printfn($"{(additionalSymbol wire).Pos.Y} and {symbolToChange.Pos.Y}")
                                            if (additionalSymbol wire).Pos.Y < symbolToChange.Pos.Y
                                            then printfn("HELLO")
                                                 
                                                 swapPorts symbol port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbol port (Symbol.getPort model (List.last portListEdge))
                                   | Right -> 
                                            printfn("CHECK6")
                                            if (additionalSymbol wire).Pos.Y < symbolToChange.Pos.Y
                                            then swapPorts symbol port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbol port (Symbol.getPort model (List.last portListEdge))
                                   | _ -> symbol
                        | "Right"-> match edge with 
                                   | Left -> 
                                   //might be able to just get rid of left etc since not affecting anything
                                            printfn("CHECK7")
                                            if (additionalSymbol wire).Pos.Y < symbolToChange.Pos.Y
                                            then swapPorts symbol port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbol port (Symbol.getPort model (List.last portListEdge))
                                   | Right -> 
                                            printfn("CHECK8")
                                            printfn($"{portListEdge.Length}")
                                            if (additionalSymbol wire).Pos.Y > symbolToChange.Pos.Y
                                            then swapPorts symbol port (Symbol.getPort model portListEdge[0])
                                            else swapPorts symbol port (Symbol.getPort model (List.last portListEdge))//(List.last portListEdge))
                                   | _ -> symbol
                List.fold test2 symbol portListSingleConnected 
            else symbol
            
            //checks if any ports are in single connected wires
            //if any in portslist are in that edge then
            //match edge with Top/Bottom/Left/right
            //if symbol position is top and edge is top then move to left/right based on position
            //if symbol position is top and edge is bottom then move to left/right based on position
            //if symbol position is top and edge is left/right then move to highest (first index)
            //if symbol position is left and edge left/right then move to top/bottom based on position
            //if symbol position is left and edge is top/bottom then move to left
        let allEdges = [Top;Bottom;Left;Right]    
        List.fold ports symbolToChange allEdges
    //added functionality for future: will change positioning based on symbol positioning e.g. is one symbol above the other
    let comparePortEdge   (otherSymbol:Symbol) (model: SymbolT.Model) (symbolToChange:Symbol) (wire:Wire) =
        let getPorts = List.map (fun (x: Port) -> string x.Id) (connectedPorts symbolToChange otherSymbol model wModel)
        let port1 = SmartHelpers.getPortFromWire model symbolToChange wire
        let port2 = SmartHelpers.getPortFromWire model otherSymbol wire
        let portEdge = symbolToChange.PortMaps.Orientation[port1.Id]
        let largestEdge = 
                        let TopList = otherSymbol.PortMaps.Order[Top] |> SmartHelpers.combineLists getPorts
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
                |Right -> if portEdge = Left then symbolToChange else changePortEdge Left symbolToChange (port1.Id)
        
        let edge = otherSymbol.PortMaps.Orientation[port2.Id]
        let newEdge edge'= match edge' with
                                    | Top -> Bottom
                                    | Bottom -> Top
                                    | _ -> edge'        
        if largestEdge.Length = 1
        then 
            changeEdge (newEdge edge)
        else
            if (fst largestEdge[0]).Length = (fst largestEdge[1]).Length
            then                
                changeEdge (newEdge edge)
            else 
                match (snd largestEdge[0]) with
                | Top -> changeEdge (newEdge edge)
                | Bottom -> changeEdge (newEdge edge)
                |_ ->
                        if newEdge edge = edge
                        then changeEdge (snd largestEdge[0])
                        else changeEdge (newEdge edge)
                
        
    let checkPortPositions (wire:Wire List) (symbol1: Symbol) (model:DrawModelType.SymbolT.Model)=
        let secondSymbolList = 
            wire
            |> List.map (anyCrossingPorts symbol1 otherSymbol model)
            |> List.sort
            |> List.unzip
            |> (fun (x,y) -> y)
        if (List.sortDescending secondSymbolList) = secondSymbolList
        then 
            //correct port indexing order
            false
        else
            //incorrect port indexing order
            true

    
    let rec changeSymbol (symbol':Symbol) (wires:Wire List) (model:Model)(n: int) = 
        if n > Constants.recursionLimit
        then symbol'
        else 
            match anyInterconnected wires with
            | true -> 
                      match (checkPortPositions wires symbol' (updateWires symbol' model symbolToOrder).Symbol) with
                                | false -> symbol'
                                | true -> 
                                          let newWireModel = getAllInterconnected model symbol' wires
                                          let newSymbol = newWireModel.Symbol.Symbols[symbol'.Id]
                                          let newWires = newWireModel |> SmartHelpers.getConnectedWires newSymbol otherSymbol
                                          changeSymbol newSymbol newWires newWireModel (n+1)
            | false -> symbol'
    let testOtherPorts = reorderUnconnectedWires otherSymbol symbolToOrder sModel wModel
    let testWireModel = updateWires testOtherPorts wModel symbolToOrder
    (*let reOrderPortEdges = 
        match anyInterconnected wires with
        | true -> List.fold (comparePortEdge otherSymbol sModel) symbolToOrder wires
        | false -> symbolToOrder*)
    let changedTestModel = {wModel with 
                                Wires = testWireModel.Wires
                                Symbol = {sModel with Symbols = Map.add testOtherPorts.Id testOtherPorts sModel.Symbols}
                            }
    let wires': Wire List = SmartHelpers.getConnectedWires testOtherPorts otherSymbol testWireModel  
    let reOrderPortEdges = 
        match anyInterconnected wires with
        | true -> List.fold (comparePortEdge otherSymbol changedTestModel.Symbol) testOtherPorts wires
        | false -> testOtherPorts
    let newWireModel = updateWires reOrderPortEdges wModel symbolToOrder

    let changedModel = {wModel with 
                            Wires = newWireModel.Wires
                            Symbol = {sModel with Symbols = Map.add reOrderPortEdges.Id reOrderPortEdges sModel.Symbols}
                        }
    let newWires = changedModel|> SmartHelpers.getConnectedWires reOrderPortEdges otherSymbol

    let finalSymbol' = changeSymbol reOrderPortEdges newWires changedModel 0
    
    let newChangedWires = updateWires finalSymbol' changedModel symbolToOrder

    {wModel with 
        Wires = newChangedWires.Wires
        Symbol = {sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols}
    }   

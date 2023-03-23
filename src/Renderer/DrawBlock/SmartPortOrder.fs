﻿module SmartPortOrder

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
open SmartHelpers



(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)
type BusWireHelpers =
    { UpdateWire: BusWireT.Model -> Wire -> bool -> Wire
      UpdateWires: BusWireT.Model -> List<ComponentId> -> XYPos -> BusWireT.Model
      UpdateSymbolWires: BusWireT.Model -> ComponentId -> BusWireT.Model }




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

    ///updates the wire model to new wire positions based on port positioning
    ///returns wire model (type BusWireT.Model)
    let updateWires (symbol: Symbol) (model: Model) (originalSymbol: Symbol) =
        (helpers.UpdateSymbolWires
            ({ model with Symbol = { model.Symbol with Symbols = Map.add symbol.Id symbol model.Symbol.Symbols } })
            originalSymbol.Id)

    ///given two XY coordinates calculates the necessary buffer to move
    ///a port from the old position to the new position
    let buffer (newPos: XYPos) (oldPos: XYPos) =
        let scaledSign (newX: float) (oldX: float) =
            Symbol.Constants.halfPortSep * float (System.Math.Sign(newX - oldX))
        { X = scaledSign newPos.X oldPos.X
          Y = scaledSign newPos.Y oldPos.Y }

    ///given two ports on a symbol, swaps the ports position
    //returns symbol with changed port positioning
    let swapPorts (symbol: Symbol) (port1: Port) (port2: Port) =
        let newPos = symbol.Pos + (getPortPos symbol port2)
        let oldPos = symbol.Pos + (getPortPos symbol port1)

        let newPos' = newPos + (buffer newPos oldPos)
        let port2NewPos = oldPos + (buffer newPos oldPos)

        let changedSymbol = SymbolUpdatePortHelpers.updatePortPos symbol newPos' port1.Id
        SymbolUpdatePortHelpers.updatePortPos changedSymbol port2NewPos port2.Id

    //given two ports on a symbol, moves the first port to the second ports position
    //returns symbol with changed port positioning
    let changePorts (symbol: Symbol) (port1: Port) (port2: Port) =
        let newPos = symbol.Pos + (getPortPos symbol port2)
        let oldPos = symbol.Pos + (getPortPos symbol port1)

        let newPos' = newPos + (buffer newPos oldPos)
        SymbolUpdatePortHelpers.updatePortPos symbol newPos' port1.Id



    //given two wires connected between two symbols
    //if the wires intersect each other, swaps the corresponding ports
    //returns the changed symbol
    let swapInterconnectedPorts (model: SymbolT.Model) (symbol: Symbol) (fstWire, sndWire) =
        let position = getSymbolPos symbolToOrder otherSymbol

        match isInterconnected position (fstWire, sndWire) with
        | false ->
            symbol
        | true ->
            let port1 = SmartHelpers.getPortFromWire model symbol fstWire
            let port2 = SmartHelpers.getPortFromWire model symbol sndWire
            let portEdge (port: Port) = symbol.PortMaps.Orientation[port.Id]

            if portEdge port1 <> portEdge port2 then
                symbol
            else
                swapPorts symbol port1 port2

    ///given a model, a specified symbol and a list of wires, goes through the list of wires
    ///and swaps the symbol ports of the intersecting wires
    ///returns the new model after ports have swapped
    let getAllInterconnected (model: Model) (symbol: Symbol) (wireList: Wire List) =
        let wirePairs = getWirePairs wireList

        let newSymbol (model: Model) (pair: Wire * Wire) =
            let changeSymbol =
                swapInterconnectedPorts model.Symbol model.Symbol.Symbols[symbol.Id] pair

            if changeSymbol <> model.Symbol.Symbols[symbol.Id] then
                let newWireModel = updateWires changeSymbol model model.Symbol.Symbols[symbol.Id]

                newWireModel
            else
                model

        List.fold newSymbol model wirePairs

    ///given a list of wires
    ///returns true if there are any overlapping wires
    let anyInterconnected (wire: Wire List) =
        let position = getSymbolPos symbolToOrder otherSymbol

        wire
        |> getWirePairs
        |> List.map (isInterconnected position)
        |> List.exists (fun x -> x = true)


    let anyCrossingPorts (symbol1: Symbol) (symbol2: Symbol) (model: SymbolT.Model) (wire: Wire) =
        let getPortIndex (symbol: Symbol) =
            let port = SmartHelpers.getPortFromWire model symbol wire
            let portEdge = symbol.PortMaps.Orientation[port.Id]
            portEdge, SmartHelpers.getPortPositionFromTopOrLeft symbol wire

        let port1Pos = getPortIndex symbol1

        let port2Pos = getPortIndex symbol2
        port1Pos, port2Pos

    ///changes the edge of a specified port on a symbol
    ///returns the symbol with the new port ordering
    let changePortEdge (edge: Edge) (symbol: Symbol) (portId: string) =
        let h, w = getRotatedHAndW symbol
        let getXY (x, y) = { X = x; Y = y }

        let newPosition =
            match edge with
            | Top -> symbol.Pos + getXY ((w / 2.0), 0.0)
            | Left -> symbol.Pos + getXY (0, (h / 2.0))
            | Bottom -> symbol.Pos + getXY ((w / 2.0), h)
            | Right -> symbol.Pos + getXY (w, (h / 2.0))

        SymbolUpdatePortHelpers.updatePortPos symbol newPosition portId

    ///reorders wires only connected to the symbolToChange symbol
    ///moves the corresponding port to the end or beginning of the port ordering on that edge
    ///based on the position of the second symbol that wire is connected to
    ///returns the new symbolToChange with the changed port positioning
    let reorderUnconnectedWires'
        (otherSymbol: Symbol)
        (model: SymbolT.Model)
        (wireModel: Model)
        (symbolToChange: Symbol)
        =
        /// returns a list of wires only connected to symbolToChange
        let getSingleConnectedWires =
            let connectedWires =
                SmartHelpers.getConnectedWires otherSymbol symbolToChange wireModel

            let symbolToChangeWires =
                BusWireUpdateHelpers.getConnectedWires wireModel [ symbolToChange.Id ]

            SmartHelpers.listDifference symbolToChangeWires connectedWires

        ///changes all unconnected ports
        let ports symbol edge =
            let portsList =
                List.map (SmartHelpers.getPortFromWire model symbolToChange) getSingleConnectedWires
                |> List.map (fun (x: Port) -> x.Id)

            let portListEdge = symbolToChange.PortMaps.Order[edge]

            ///given a wire, returns the second symbol that wire is connected to other than symbolToChange
            let additionalSymbol = SmartHelpers.getSymbolFromWire model symbolToChange

            if
                (SmartHelpers.combineLists portsList portListEdge).Length <> 0
            //if there is a port that is connected to a different symbol
            then

                ///returns list of all ports on that edge
                let portListSingleConnected =
                    SmartHelpers.combineLists portsList portListEdge
                    |> List.map (fun (x: string) -> model.Ports[x])

                let changeUnconnectedPos symbol (port: Port) =

                    /// given a symbol and a port returns the first corresponding wire
                    let wire =
                        let containsPorts (wire: Wire) =
                            if (Symbol.getInputPortIdStr wire.InputPort) <> port.Id then
                                (Symbol.getOutputPortIdStr wire.OutputPort) = port.Id //boolean condition
                            else
                                true

                        let getWire =
                            wireModel.Wires
                            |> Map.toList
                            |> List.map (fun (x, y) -> y)
                            |> List.filter containsPorts

                        getWire[0]

                    let changePortPos XorY isGreater =
                        let getPositionXY (symbol: Symbol) =
                            match XorY with
                            | "X" -> symbol.Pos.X
                            | "Y" -> symbol.Pos.Y

                        let position1 = getPositionXY (additionalSymbol wire)
                        let position2 = getPositionXY symbolToChange

                        let compare =
                            match isGreater with
                            | true -> position1 > position2
                            | false -> position1 < position2

                        if compare then
                            changePorts symbol port (Symbol.getPort model portListEdge[0])
                        else
                            changePorts symbol port (Symbol.getPort model (List.last portListEdge))


                    match (getSymbolPos symbolToChange otherSymbol) with
                    | Above
                    | Below ->
                        match edge with
                        | Top
                        | Bottom -> changePortPos "X" true
                        | _ -> symbol
                    | OnLeft
                    | OnRight ->
                        match edge with
                        | Left -> changePortPos "Y" false
                        | Right -> changePortPos "Y" true
                        | _ -> symbol

                List.fold changeUnconnectedPos symbol portListSingleConnected
            else
                symbol

        let allEdges = [ Top; Bottom; Left; Right ]
        List.fold ports symbolToChange allEdges

    ///given two symbols connected by a wire, changes the edge of the port based on the
    ///relative symbol position to make the sheet appear more clean and simple
    ///returns the changed symbol
    let comparePortEdge'' (otherSymbol: Symbol) (model: SymbolT.Model) (symbolToChange: Symbol) (wire: Wire) =
        let port1 = SmartHelpers.getPortFromWire model symbolToChange wire
        let port2 = SmartHelpers.getPortFromWire model otherSymbol wire
        let portEdge = symbolToChange.PortMaps.Orientation[port1.Id]
        let position = getSymbolPos symbolToChange otherSymbol

        let changeEdge (edge: Edge) (position: SymbolPosition) =
            let checkToChangeEdge (edge: Edge) =
                if portEdge = edge then
                    symbolToChange
                else
                    changePortEdge edge symbolToChange (port1.Id)

            match position with
            | Above ->
                match edge with
                | Top
                | Bottom -> checkToChangeEdge Top
                | Left -> checkToChangeEdge Left
                | Right -> checkToChangeEdge Right
            | Below ->
                match edge with
                | Top
                | Bottom -> checkToChangeEdge Bottom
                | Left -> checkToChangeEdge Left
                | Right -> checkToChangeEdge Right
            | OnLeft ->
                match edge with
                | Top -> checkToChangeEdge Top
                | Bottom -> checkToChangeEdge Bottom
                | Left
                | Right -> checkToChangeEdge Left
            | OnRight ->
                match edge with
                | Top -> checkToChangeEdge Top
                | Bottom -> checkToChangeEdge Bottom
                | Left
                | Right -> checkToChangeEdge Right

        let edge = otherSymbol.PortMaps.Orientation[port2.Id]
        changeEdge edge position

    ///given a symbol and a wire list checks whether the port index order is aligned
    ///with the corresponding wire's symbol port index order
    ///returns false if the index order is correct and there are no intersections
    let checkPortPositions (wire: Wire List) (symbol1: Symbol) (model: DrawModelType.SymbolT.Model) =
        ///returns the a tuple of a tuple of the port edge and its index
        let firstSymbolList =
            wire
            |> List.map (anyCrossingPorts symbol1 otherSymbol model) //separate into 4 based on otherSymbol
            |> List.sort

        let position = getSymbolPos symbol1 otherSymbol

        let checkBothEdges (isSame: bool) (edge: Edge) ((x: Edge, y), (z: Edge, a)) =

            let direction =
                match edge with
                | Left
                | Right -> x = Left || x = Right
                | Top
                | Bottom -> x = Top || x = Bottom

            match isSame with
            | true ->
                if x <> z then false
                else if direction then false
                else true
            | false ->
                if x <> z then true
                else if direction then true
                else false

        let checkIndex (isSame: bool) =
            match position with
            | OnLeft -> checkBothEdges isSame Left
            | OnRight -> checkBothEdges isSame Right
            | Above -> checkBothEdges isSame Top
            | Below -> checkBothEdges isSame Bottom

        let checkSameEdgeIndex = checkIndex true
        let checkDifEdgeIndex = checkIndex false

        let lstSame = List.filter checkSameEdgeIndex firstSymbolList
        let lstDif = List.filter checkDifEdgeIndex firstSymbolList

        let checkSame =
            let portLst =
                lstSame
                |> List.sortBy (fun ((x, y: int option), (z, a)) -> y)
                |> List.unzip
                |> fun (x, y) -> y

            (List.sortByDescending (fun (x, y: int option) -> y)) portLst <> portLst

        let checkDifferent =
            let portLst =
                lstDif
                |> List.sortBy (fun ((x, y: int option), (z, a)) -> y)
                |> List.unzip
                |> fun (x, y) -> y

            (List.sortBy (fun (x, y: int option) -> y) portLst) <> portLst

        if checkDifferent = false && checkSame = false then
            false
        else
            true

    ///recursive function that implements port reordering by checking if there are
    ///intersecting functions and if the port order is correct
    ///returns the final changed symbol
    let rec changeSymbol (symbol': Symbol) (wires: Wire List) (model: Model) (n: int) =
        if n > Constants.recursionLimit then
            symbol'
        else
            match anyInterconnected wires with
            | true ->
                match (checkPortPositions wires symbol' (updateWires symbol' model symbolToOrder).Symbol) with
                | false -> symbol'
                | true ->
                    let newWireModel = getAllInterconnected model symbol' wires
                    let newSymbol = newWireModel.Symbol.Symbols[symbol'.Id]
                    let newWires = newWireModel |> SmartHelpers.getConnectedWires newSymbol otherSymbol
                    changeSymbol newSymbol newWires newWireModel (n + 1)
            | false ->
                symbol'

    let testOtherPorts = symbolToOrder
    let testWireModel = updateWires testOtherPorts wModel symbolToOrder

    let changedTestModel =
        { wModel with
            Wires = testWireModel.Wires
            Symbol = { sModel with Symbols = Map.add testOtherPorts.Id testOtherPorts sModel.Symbols } }

    let wires': Wire List =
        SmartHelpers.getConnectedWires testOtherPorts otherSymbol testWireModel

    let reOrderPortEdges =
        List.fold (comparePortEdge'' otherSymbol changedTestModel.Symbol) testOtherPorts wires'

    let newWireModel = updateWires reOrderPortEdges wModel symbolToOrder

    let changedModel =
        { wModel with
            Wires = newWireModel.Wires
            Symbol = { sModel with Symbols = Map.add reOrderPortEdges.Id reOrderPortEdges sModel.Symbols } }

    let newWires =
        changedModel |> SmartHelpers.getConnectedWires reOrderPortEdges otherSymbol

    ///reverses the inputs for symbolToOrder if there are any interconnecting wires
    ///returns the changed symbol
    let changeMux =
        let wires: Wire List =
            SmartHelpers.getConnectedWires symbolToOrder otherSymbol wModel
        match anyInterconnected wires with
        | false -> symbolToOrder
        | true -> SymbolReplaceHelpers.changeReversedInputs sModel symbolToOrder.Id

    ///uses port mapping helping function to swap specified ports to ensure wires
    ///are straight (to use with SmartResizing)
    ///returns final changed symbol
    let testPortMapping (symbolToChange: Symbol) =
        let portMapList = SmartHelpers.portMapping changedModel symbolToChange otherSymbol

        let getSwappedSymbol (symbol: Symbol) (x: string, y: string) =

            let port1 = getPort changedModel.Symbol x
            let port2 = getPort changedModel.Symbol y
            swapPorts symbol port1 port2

        match portMapList.Length with
        | 0 -> symbolToChange
        | _ -> List.fold getSwappedSymbol symbolToChange portMapList

    ///implements symbolToOrder port reordering based on custom vs non-custom component
    ///returns symbol
    let finalSymbol' =
        match symbolToOrder.Component.Type with
        | And
        | Or
        | Xor
        | Nand
        | Nor
        | Xnor
        | Mux2
        | Demux2 -> changeMux
        | _ ->
            let newSymbol =
                changeSymbol reOrderPortEdges newWires changedModel 0 |> testPortMapping

            let newWireModel = updateWires newSymbol changedModel symbolToOrder

            newSymbol
            |> reorderUnconnectedWires' otherSymbol newWireModel.Symbol newWireModel

    let newChangedWires =
        match symbolToOrder.Component.Type with
        | And
        | Or
        | Xor
        | Nand
        | Nor
        | Xnor
        | Mux2
        | Demux2 -> updateWires finalSymbol' wModel symbolToOrder
        | _ -> updateWires finalSymbol' changedModel symbolToOrder

    { wModel with
        Wires = newChangedWires.Wires
        Symbol = { sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols } }

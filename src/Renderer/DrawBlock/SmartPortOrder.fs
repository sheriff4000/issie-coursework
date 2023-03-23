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
type BusWireHelpers =
    { UpdateWire: BusWireT.Model -> Wire -> bool -> Wire
      UpdateWires: BusWireT.Model -> List<ComponentId> -> XYPos -> BusWireT.Model
      UpdateSymbolWires: BusWireT.Model -> ComponentId -> BusWireT.Model }

type SymbolPosition =
    | OnLeft
    | OnRight
    | Above
    | Below


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
    printfn $"PORT LIST {sModel.Symbols[symbolToOrder.Id].PortMaps}"

    ///updates the wire model to new wire positions based on port positioning
    ///returns wire model (type BusWireT.Model)
    let updateWires (symbol: Symbol) (model: Model) (originalSymbol: Symbol) =
        (helpers.UpdateSymbolWires
            ({ model with Symbol = { model.Symbol with Symbols = Map.add symbol.Id symbol model.Symbol.Symbols } })
            originalSymbol.Id)

    ///given two ports on a symbol, moves the first port to the second ports position
    //returns symbol with changed port positioning
    let swapPorts (symbol: Symbol) (port1: Port) (port2: Port) =
        let newPos = symbol.Pos + (getPortPos symbol port2)
        let oldPos = symbol.Pos + (getPortPos symbol port1)

        let scaledSign (newX: float) (oldX: float) =
            Symbol.Constants.halfPortSep * float (System.Math.Sign(newX - oldX)) //potentially scale this

        let buffer =
            { X = scaledSign newPos.X oldPos.X
              Y = scaledSign newPos.Y oldPos.Y }

        let newPos' = newPos + buffer

        SymbolUpdatePortHelpers.updatePortPos symbol newPos' port1.Id

    //input = list of wires
    //output = list of all wire combinations as tuples
    let getWirePairs (wireList: Wire List) =
        let isNotDuplicate (x, y) = x <> y

        let orderTupleByWireId (x, y) =
            if x.WId < y.WId then (x, y) else (y, x)

        wireList
        |> List.allPairs wireList
        |> List.filter isNotDuplicate
        |> List.map orderTupleByWireId
        |> List.distinct

    //returns the relative symbol position of symbol1 compared to symbol2 on the model
    //output type = type SymbolPosition
    let getSymbolPos (symbol1: Symbol) (symbol2: Symbol) : SymbolPosition =
        let xDifference = symbol1.Pos.X - symbol2.Pos.X
        let yDifference = symbol1.Pos.Y - symbol2.Pos.Y

        if (abs xDifference) < (abs yDifference) then
            printfn "Vertical"

            if yDifference > 0 then
                //symbol1 is above symbol 2
                Above
            else
                //symbol1 is below symbol 2
                Below
        else
            printfn "Horizontal"

            if xDifference > 0 then
                //symbol1 is to the left of symbol1
                OnLeft
            else
                //symbol1 is to the right of symbol2
                OnRight

    ///input = a tuple of 2 wires, and the position of one symbol wrt the other
    ///output = boolean, which returns true if the wires are connected
    ///and false if they are not
    let isInterconnected' (position: SymbolPosition) (fstWire, sndWire) =
        let fstWireAseg = BusWire.getAbsSegments fstWire
        let sndWireAseg = BusWire.getAbsSegments sndWire

        let compareSegments (sndWire: ASegment list) (seg: ASegment) =
            let compareY (seg: ASegment) (wireSeg: ASegment) =

                let isTaller = wireSeg.Start.Y > seg.Start.Y

                match seg.Start.X < seg.End.X with
                | true ->
                    if wireSeg.Start.X >= seg.Start.X then
                        if wireSeg.Start.X <= seg.End.X then [ isTaller ] else []
                    else if wireSeg.End.X >= seg.Start.X then
                        [ isTaller ]
                    else
                        []
                | false ->
                    if wireSeg.Start.X <= seg.Start.X then
                        if wireSeg.Start.X >= seg.End.X then [ isTaller ] else []
                    else if wireSeg.End.X <= seg.Start.X then
                        [ isTaller ]
                    else
                        []

            let compareX (seg: ASegment) (wireSeg: ASegment) =
                let isLeft = wireSeg.Start.X > seg.Start.X

                match seg.Start.Y < seg.End.Y with
                | true ->
                    printfn $"CHECKPOINT" //DEBUGGING REQUIRED

                    if wireSeg.Start.Y >= seg.Start.Y then
                        if wireSeg.Start.Y <= seg.End.Y then [ isLeft ] else []
                    else if wireSeg.End.Y >= seg.Start.Y then
                        [ isLeft ]
                    else
                        []
                | false ->
                    if wireSeg.Start.Y <= seg.Start.Y then
                        if wireSeg.Start.Y >= seg.End.Y then [ isLeft ] else []
                    else if wireSeg.End.Y <= seg.Start.Y then
                        [ isLeft ]
                    else
                        []


            match position with
            | OnLeft
            | OnRight -> List.collect (compareY seg) sndWire
            | Above
            | Below -> List.collect (compareX seg) sndWire

        let lst = List.collect (compareSegments sndWireAseg) fstWireAseg |> List.distinct

        if lst.Length <> 1 then
            //is interconnected
            true
        else
            //not interconnected
            false

    //given two wires connected between two symbols
    //if the wires intersect each other, swaps the corresponding ports
    //returns the changed symbol
    let swapInterconnectedPorts (model: SymbolT.Model) (symbol: Symbol) (fstWire, sndWire) =
        let position = getSymbolPos symbolToOrder otherSymbol

        match isInterconnected' position (fstWire, sndWire) with
        | false ->
            printfn $"not intersecting"
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
        |> List.map (isInterconnected' position)
        |> List.exists (fun x -> x = true)
    //CHANGE TO MAKE INTERCONNECTED A HELPER FUNCTION

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

    ///given two symbols, returns a list of all the ports corresponding
    ///to the wires connected between the two symbols
    let connectedPorts (symbol1: Symbol) (symbol2: Symbol) (model: SymbolT.Model) (wireModel: Model) =
        let wires = SmartHelpers.getConnectedWires symbol1 symbol2 wireModel
        List.map (SmartHelpers.getPortFromWire model symbol2) wires

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

        ///change to remove listDifference

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

                let test2 symbol (port: Port) =
                    /// Returns a list of the wires in the model connected to a list of components given by compIds
                    let wire =
                        let containsPorts (wire: Wire) =
                            if (Symbol.getInputPortIdStr wire.InputPort) <> port.Id then
                                (Symbol.getOutputPortIdStr wire.OutputPort) = port.Id //boolean condition
                            else
                                true
                            

                        let test =
                            wireModel.Wires
                            |> Map.toList
                            |> List.map (fun (x, y) -> y)
                            |> List.filter containsPorts

                        test[0]

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
                            swapPorts symbol port (Symbol.getPort model portListEdge[0])
                        else
                            swapPorts symbol port (Symbol.getPort model (List.last portListEdge))


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

                List.fold test2 symbol portListSingleConnected
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
            let checkToChangeEdge (edge:Edge)=
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

    let checkPortPositions (wire: Wire List) (symbol1: Symbol) (model: DrawModelType.SymbolT.Model) =
        let firstSymbolList =
            wire
            |> List.map (anyCrossingPorts symbol1 otherSymbol model) //separate into 4 based on otherSymbol
            |> List.sort //make multiple lists for top bottom left right?
        let position = getSymbolPos symbol1 otherSymbol
        let checkBothEdges (edge:Edge) ((x: Edge, y), (z: Edge, a)) =

            let direction = 
                match edge with
                |Left
                |Right -> x = Left || x = Right
                |Top
                |Bottom -> x = Top || x = Bottom
            if x<> z 
            then false
            else if direction then false else true
        let checkTopFun position =
            match position with
            | OnLeft ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Left || x = Right then false
                    else true)
            | OnRight ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Left || x = Right then false
                    else true)
            | Above ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Top || x = Bottom then false
                    else true)
            | Below ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Top || x = Bottom then false
                    else true)

        let checkTopFun2 position =
            match position with
            | OnLeft ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Left || x = Right then true
                    else false)
            | OnRight ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Left || x = Right then true
                    else false)
            | Above ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Top || x = Bottom then true
                    else false)
            | Below ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Top || x = Bottom then true
                    else false)

        let lstTop' = List.filter (checkTopFun position) firstSymbolList
        let lstTop2' = List.filter (checkTopFun2 position) firstSymbolList

        let checkTop =
            lstTop'
            |> List.sortBy (fun ((x, y: int option), (z, a)) -> y)
            |> List.unzip
            |> fun (x, y) -> y

        let checkTop2 =
            lstTop2'
            |> List.sortBy (fun ((x, y: int option), (z, a)) -> y)
            |> List.unzip
            |> fun (x, y) -> y

        let checkSame =
            (List.sortByDescending (fun (x, y: int option) -> y)) checkTop <> checkTop

        let checkDifferent =
            (List.sortBy (fun (x, y: int option) -> y) checkTop2) <> checkTop2

        printfn ($" different {checkDifferent}")
        printfn ($"same {checkSame}")

        if checkDifferent = false && checkSame = false then
            false
        else
            true


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
                printfn $"NO INTERCONNECTED WIRES"
                symbol'

    let testOtherPorts = symbolToOrder // reorderUnconnectedWires otherSymbol symbolToOrder sModel wModel
    let testWireModel = updateWires testOtherPorts wModel symbolToOrder

    let changedTestModel =
        { wModel with
            Wires = testWireModel.Wires
            Symbol = { sModel with Symbols = Map.add testOtherPorts.Id testOtherPorts sModel.Symbols } }

    let wires: Wire List =
        SmartHelpers.getConnectedWires symbolToOrder otherSymbol wModel

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

    //let finalSymbol' = changeSymbol reOrderPortEdges newWires changedModel 0
    let changeMux =
        match anyInterconnected wires with
        | false -> symbolToOrder
        | true -> SymbolReplaceHelpers.changeReversedInputs sModel symbolToOrder.Id

    let testPortMapping (symbolToChange: Symbol) =
        let portMapList = SmartHelpers.portMapping changedModel symbolToChange otherSymbol

        let getSwappedSymbol (symbol: Symbol) (x: string, y: string) =

            let port1 = getPort changedModel.Symbol x
            let port2 = getPort changedModel.Symbol y
            swapPorts symbol port1 port2

        match portMapList.Length with
        | 0 -> symbolToChange
        | _ -> List.fold getSwappedSymbol symbolToChange portMapList

    let finalSymbol' =
        match symbolToOrder.Component.Type with
        | And | Or | Xor | Nand | Nor | Xnor 
        | Mux2 | Demux2 -> changeMux
        | _ ->
            let newSymbol =
                changeSymbol reOrderPortEdges newWires changedModel 0 |> testPortMapping //sometimes has errors so need something to check if not adjacent

            let newWireModel = updateWires newSymbol changedModel symbolToOrder

            newSymbol
            |> reorderUnconnectedWires' otherSymbol newWireModel.Symbol newWireModel

    let newChangedWires =
        match symbolToOrder.Component.Type with
        | Mux2 -> updateWires finalSymbol' wModel symbolToOrder
        | _ -> updateWires finalSymbol' changedModel symbolToOrder

    { wModel with
        Wires = newChangedWires.Wires
        Symbol = { sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols } }

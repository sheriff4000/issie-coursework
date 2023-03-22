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

    let updateWires (symbol: Symbol) (model: Model) (originalSymbol: Symbol) =
        (helpers.UpdateSymbolWires
            ({ model with Symbol = { model.Symbol with Symbols = Map.add symbol.Id symbol model.Symbol.Symbols } })
            originalSymbol.Id)

    // FEEDBACK: function xml comment should say what function output is
    let swapPorts (symbol: Symbol) (port1: Port) (port2: Port) =
        let newPos = symbol.Pos + (getPortPos symbol port2)
        let oldPos = symbol.Pos + (getPortPos symbol port1)
        // FEEDBACK: could rewrite buffer definition using one tiny subfunction as below
        // FP allows you to indentify an use functional abstraction at every level to simplify code
        // I have used existing functions (look at Math module) since "sign" is a well-known function.
        // But if I did not use it, it would be easy to write yourself here
        // Although it is so useful you should probably make it a general purpose helper (only it is in Math library)
        // **More serious**. The constant 25.0 should be in Constants submodule (and I suspect it should NOT be a separate nconstant, but
        // derived from some other size constant. I have added it here temporarily. You can deal with this.
        let halfPortSep = 25.

        let scaledSign (newX: float) (oldX: float) =
            halfPortSep * float (System.Math.Sign(newX - oldX))

        let buffer =
            { X = scaledSign newPos.X oldPos.X
              Y = scaledSign newPos.Y oldPos.Y }

        let newPos' = newPos + buffer

        SymbolUpdatePortHelpers.updatePortPos symbol newPos' port1.Id

    //input = all wires connected between the two relevant symbols
    // FEEDBACK - function xml comment should say what output is
    let getWirePairs (wireList: Wire List) =
        let isNotDuplicate (x, y) = x <> y

        let orderTupleByWireId (x, y) =
            if x.WId < y.WId then (x, y) else (y, x)

        wireList
        |> List.allPairs wireList
        |> List.filter isNotDuplicate
        |> List.map orderTupleByWireId
        |> List.distinct

    let isInterconnected' (position: string) (fstWire, sndWire) =
        let fstWireAseg = BusWire.getAbsSegments fstWire
        let sndWireAseg = BusWire.getAbsSegments sndWire

        let compareSegments (sndWire: ASegment list) (seg: ASegment) =
            let compareY (seg: ASegment) (wireSeg: ASegment) =
                
                let isTaller = wireSeg.Start.Y > seg.Start.Y
                match seg.Start.X < seg.End.X with
                |true -> 
                        if wireSeg.Start.X >= seg.Start.X then
                            if wireSeg.Start.X <= seg.End.X then [ isTaller ] else []
                        else if wireSeg.End.X >= seg.Start.X then
                            [ isTaller ]
                        else

                                []
                |false -> 
                          if wireSeg.Start.X <= seg.Start.X then
                            if wireSeg.Start.X >= seg.End.X then [ isTaller ] else []
                          else if wireSeg.End.X <= seg.Start.X then
                                [ isTaller ]
                            else

                                    []

            let compareX (seg: ASegment) (wireSeg: ASegment) =
                //printfn $"SEGMENT COMPARE {seg.Start.X}, {seg.Start.Y}, SECOND {wireSeg.Start.X}, {wireSeg.Start.Y}"
                let isLeft = wireSeg.Start.X > seg.Start.X
                match seg.Start.Y < seg.End.Y with
                |true -> printfn $"CHECKPOINT"//DEBUGGING REQUIRED
                         if wireSeg.Start.Y >= seg.Start.Y then
                            if wireSeg.Start.Y <= seg.End.Y then [ isLeft ] else []
                         else if wireSeg.End.Y >= seg.Start.Y then
                                [ isLeft ]
                         else

                                []
                | false -> printfn "ALTERNATIVE"
                           if wireSeg.Start.Y <= seg.Start.Y then
                            if wireSeg.Start.Y >= seg.End.Y then [ isLeft ] else []
                           else if wireSeg.End.Y <= seg.Start.Y then
                                    [ isLeft ]
                           else

                                    []
                

            match position with
            | "Left"
            | "Right" -> List.collect (compareY seg) sndWire
            | "Top"
            | "Bottom" -> List.collect (compareX seg) sndWire
        
        let lst = List.collect (compareSegments sndWireAseg) fstWireAseg |> List.distinct

        if lst.Length <> 1 then
            //is interconnected
            true
        else
            //not interconnected
            false


    let getSymbolPos (symbol1: Symbol) (symbol2: Symbol) = //gets symbol positioning with respect to the other
        let xDifference = symbol1.Pos.X - symbol2.Pos.X
        let yDifference = symbol1.Pos.Y - symbol2.Pos.Y

        if (abs xDifference) < (abs yDifference) then
            printfn "Vertical"

            if yDifference > 0 then
                //symbol1 is above symbol 2
                "Top"
            else
                //symbol1 is below symbol 2
                "Bottom"
        else
            printfn "Horizontal"

            if xDifference > 0 then
                //symbol1 is to the left of symbol1
                "Left"
            else
                //symbol1 is to the right of symbol2
                "Right"

    printfn $"{(getSymbolPos symbolToOrder otherSymbol)}"
    //get wires that are not connected to both symbols by getting wires only connected to that one symbol
    //maybe use list.filter

    let swapInterconnectedPorts (model: SymbolT.Model) (symbol: Symbol) (fstWire, sndWire) =
        let position = getSymbolPos symbolToOrder otherSymbol

        match isInterconnected' position (fstWire, sndWire) with
        | false ->
            printfn $"not intersecting"
            symbol
        | true ->
            let port1 = SmartHelpers.getPortFromWire model symbol fstWire
            let port2 = SmartHelpers.getPortFromWire model symbol sndWire
            let portEdge (port:Port) = symbol.PortMaps.Orientation[port.Id]
            if portEdge port1 <> portEdge port2
            then 
                printfn $"PORT EDGES {portEdge port1} {portEdge port2}"
                symbol 
            else 
                printfn $"intersecting {port1.Id} {port2.Id}"
                swapPorts symbol port1 port2

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



    let anyInterconnected (wire: Wire List) =
        let position = getSymbolPos symbolToOrder otherSymbol
        wire
        |> getWirePairs
        |> List.map (isInterconnected' position) //SmartHelpers.isInterconnected
        |> List.exists (fun x -> x = true)



    //input each wire from connected wire list
    //needs index
    let anyCrossingPorts (symbol1: Symbol) (symbol2: Symbol) (model: SymbolT.Model) (wire: Wire) =
        let getPortIndex (symbol: Symbol) =
            let port = SmartHelpers.getPortFromWire model symbol wire
            let portEdge = symbol.PortMaps.Orientation[port.Id]
            portEdge, SmartHelpers.getPortPositionFromTopOrLeft symbol wire

        let port1Pos = getPortIndex symbol1

        let port2Pos = getPortIndex symbol2
        port1Pos, port2Pos

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

    let connectedPorts (symbol1: Symbol) (symbol2: Symbol) (model: SymbolT.Model) (wireModel: Model) =
        let wires = SmartHelpers.getConnectedWires symbol1 symbol2 wireModel
        List.map (SmartHelpers.getPortFromWire model symbol2) wires


    let reorderUnconnectedWires'
        (otherSymbol: Symbol)
        (model: SymbolT.Model)
        (wireModel: Model)
        (symbolToChange: Symbol)
        =
        let getSingleConnectedWires = //wires only connected to symbolToChange
            let connectedWires =
                SmartHelpers.getConnectedWires otherSymbol symbolToChange wireModel

            let symbolToChangeWires =
                BusWireUpdateHelpers.getConnectedWires wireModel [ symbolToChange.Id ]

            SmartHelpers.listDifference symbolToChangeWires connectedWires

        let ports symbol edge =
            let portsList =
                List.map (SmartHelpers.getPortFromWire model symbolToChange) getSingleConnectedWires
                |> List.map (fun (x: Port) -> x.Id)

            let portListEdge = symbolToChange.PortMaps.Order[edge]
            //need to create another function that gets a symbol from a wire and another symbol

            let additionalSymbol = SmartHelpers.getSymbolFromWire model symbolToChange

            if
                (SmartHelpers.combineLists portsList portListEdge).Length <> 0 //if there is a port that is connected to a different symbol
            then
                //need to get list of ports on this edge
                let portListSingleConnected =
                    SmartHelpers.combineLists portsList portListEdge
                    |> List.map (fun (x: string) -> model.Ports[x])

                let test2 symbol (port: Port) =
                    /// Returns the IDs of the wires in the model connected to a list of components given by compIds
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
                    //let wire = wireModel.Wires[port.]
                    match (getSymbolPos symbolToChange otherSymbol) with
                    | "Top"
                    | "Bottom" ->
                        match edge with
                        | Top
                        | Bottom ->
                            printfn ("CHECK4")

                            if (additionalSymbol wire).Pos.X > symbolToChange.Pos.X then
                                swapPorts symbol port (Symbol.getPort model portListEdge[0])
                            else
                                swapPorts symbol port (Symbol.getPort model (List.last portListEdge))
                        | _ -> symbol
                    | "Left"
                    | "Right" ->
                        match edge with
                        | Left -> 
                            if (additionalSymbol wire).Pos.Y < symbolToChange.Pos.Y then //test this!
                                swapPorts symbol port (Symbol.getPort model portListEdge[0])
                            else
                                swapPorts symbol port (Symbol.getPort model (List.last portListEdge)) //(List.last portListEdge))
                        | Right ->
                            printfn ("CHECK8")
                            printfn ($"{portListEdge.Length}")

                            if (additionalSymbol wire).Pos.Y > symbolToChange.Pos.Y then //test this!
                                swapPorts symbol port (Symbol.getPort model portListEdge[0])
                            else
                                swapPorts symbol port (Symbol.getPort model (List.last portListEdge)) //(List.last portListEdge))
                        | _ -> symbol

                List.fold test2 symbol portListSingleConnected
            else
                symbol

        let allEdges = [ Top; Bottom; Left; Right ]
        List.fold ports symbolToChange allEdges

    let comparePortEdge'' (otherSymbol: Symbol) (model: SymbolT.Model) (symbolToChange: Symbol) (wire: Wire) =
        let getPorts =
            List.map (fun (x: Port) -> string x.Id) (connectedPorts symbolToChange otherSymbol model wModel)

        let port1 = SmartHelpers.getPortFromWire model symbolToChange wire
        let port2 = SmartHelpers.getPortFromWire model otherSymbol wire
        let portEdge = symbolToChange.PortMaps.Orientation[port1.Id]
        let position = getSymbolPos symbolToChange otherSymbol


        let changeEdge (edge: Edge) (position: string) =
            //if position = left then below is fine but with top to top and bottom to bottom
            //if position = right, left and left?
            //if bottom/top - left to left/right to right but bottom to bottom and top to bottom
            printfn $"position {position}"
            match position with
            | "Top" ->
                match edge with
                | Top
                | Bottom ->
                    if portEdge = Top then
                        symbolToChange
                    else
                        changePortEdge Top symbolToChange (port1.Id)
                | Left ->
                    if portEdge = Left then
                        symbolToChange
                    else
                        changePortEdge Left symbolToChange (port1.Id)
                | Right ->
                    if portEdge = Right then
                        symbolToChange
                    else
                        changePortEdge Right symbolToChange (port1.Id)
            | "Bottom" ->
                printfn $"EDGE: {edge}"
                match edge with
                | Top
                | Bottom ->
                    if portEdge = Bottom then
                        symbolToChange
                    else
                        changePortEdge Bottom symbolToChange (port1.Id)
                | Left ->
                    if portEdge = Left then
                        symbolToChange
                    else
                        changePortEdge Left symbolToChange (port1.Id)
                | Right ->
                    if portEdge = Right then
                        symbolToChange
                    else
                        changePortEdge Right symbolToChange (port1.Id)
            | "Left" ->
                match edge with
                | Top ->
                    if portEdge = Top then
                        symbolToChange
                    else
                        changePortEdge Top symbolToChange (port1.Id)
                | Bottom ->
                    if portEdge = Bottom then
                        symbolToChange
                    else
                        changePortEdge Bottom symbolToChange (port1.Id)
                | Left
                | Right ->
                    if portEdge = Left then
                        symbolToChange
                    else
                        changePortEdge Left symbolToChange (port1.Id)
            | "Right" ->
                match edge with
                | Top ->
                    if portEdge = Top then
                        symbolToChange
                    else
                        changePortEdge Top symbolToChange (port1.Id)
                | Bottom ->
                    if portEdge = Bottom then
                        symbolToChange
                    else
                        changePortEdge Bottom symbolToChange (port1.Id)
                | Left
                | Right ->
                    if portEdge = Right then
                        symbolToChange
                    else
                        changePortEdge Right symbolToChange (port1.Id)

        let edge = otherSymbol.PortMaps.Orientation[port2.Id]

        printf ($"Position: {position}")
        changeEdge edge position

    let checkPortPositions (wire: Wire List) (symbol1: Symbol) (model: DrawModelType.SymbolT.Model) =
        let firstSymbolList =
            wire
            |> List.map (anyCrossingPorts symbol1 otherSymbol model) //separate into 4 based on otherSymbol
            |> List.sort //make multiple lists for top bottom left right?

        let funTest ((x: Edge, y), (z: Edge, a)) = x = z
        let funTest2 ((x: Edge, y), (z: Edge, a)) = x <> z
        let position = getSymbolPos symbol1 otherSymbol

        let checkTopFun position =
            match position with
            | "Left" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Left || x = Right then false
                    else true)
            | "Right" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Left || x = Right then false
                    else true)
            | "Top" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Top || x = Bottom then false
                    else true)
            | "Bottom" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then false
                    else if x = Top || x = Bottom then false
                    else true)

        let checkTopFun2 position =
            match position with
            | "Left" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Left || x = Right then true
                    else false)
            | "Right" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Left || x = Right then true
                    else false)
            | "Top" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Top || x = Bottom then true
                    else false)
            | "Bottom" ->
                (fun ((x: Edge, y), (z: Edge, a)) ->
                    if x <> z then true
                    else if x = Top || x = Bottom then true
                    else false)

        let lstTop = List.filter funTest firstSymbolList
        let lstTop2 = List.filter funTest2 firstSymbolList
        let lstTop' = List.filter (checkTopFun position) firstSymbolList
        let lstTop2' = List.filter (checkTopFun2 position) firstSymbolList

        let checkTop =
            printf ($"list 1 {lstTop'}")

            lstTop'
            |> List.sortBy (fun ((x, y: int option), (z, a)) -> y)
            |> List.unzip
            |> fun (x, y) -> y

        let checkTop2 =
            printf ($"list 2 {lstTop2'}")

            lstTop2'
            |> List.sortBy (fun ((x, y: int option), (z, a)) -> y)
            |> List.unzip
            |> fun (x, y) -> y

        let checkSame =
            if (List.sortByDescending (fun (x, y: int option) -> y)) checkTop = checkTop then
                false
            else
                true

        let checkDifferent =
            if (List.sortBy (fun (x, y: int option) -> y) checkTop2) = checkTop2 then
                false
            else
                true

        let secondSymbolList =
            wire
            |> List.map (anyCrossingPorts symbol1 otherSymbol model) //separate into 4 based on otherSymbol
            |> List.sort //make multiple lists for top bottom left right?
            |> List.unzip
            |> (fun (x, y) -> y)
        //the port ordering is dependent on edge index:
        //if they have the same edge then list.sort
        //if they have different ports then list.sort descending
        (*if (List.sortDescending secondSymbolList) = secondSymbolList
        then 
            //correct port indexing order
            false
        else
            //incorrect port indexing order
            true*)
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
            | false -> printfn $"NO INTERCONNECTED WIRES"
                       symbol'

    let testOtherPorts = symbolToOrder // reorderUnconnectedWires otherSymbol symbolToOrder sModel wModel
    let testWireModel = updateWires testOtherPorts wModel symbolToOrder
    (*let reOrderPortEdges = 
        match anyInterconnected wires with
        | true -> List.fold (comparePortEdge otherSymbol sModel) symbolToOrder wires
        | false -> symbolToOrder*)
    let changedTestModel =
        { wModel with
            Wires = testWireModel.Wires
            Symbol = { sModel with Symbols = Map.add testOtherPorts.Id testOtherPorts sModel.Symbols } }

    let wires: Wire List =
        SmartHelpers.getConnectedWires symbolToOrder otherSymbol wModel

    let wires': Wire List =
        SmartHelpers.getConnectedWires testOtherPorts otherSymbol testWireModel

    let reOrderPortEdges = //fix
        match anyInterconnected wires with
        | true -> List.fold (comparePortEdge'' otherSymbol changedTestModel.Symbol) testOtherPorts wires'
        | false -> testOtherPorts

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

        List.fold getSwappedSymbol symbolToChange portMapList

    let finalSymbol' =
        match symbolToOrder.Component.Type with
        | Mux2 -> changeMux
        | _ ->
            let newSymbol =
                changeSymbol reOrderPortEdges newWires changedModel 0 //|> testPortMapping

            let newWireModel = updateWires newSymbol changedModel symbolToOrder

            newSymbol
            //|> reorderUnconnectedWires' otherSymbol newWireModel.Symbol newWireModel

    let newChangedWires =
        match symbolToOrder.Component.Type with
        | Mux2 -> updateWires finalSymbol' wModel symbolToOrder
        | _ -> updateWires finalSymbol' changedModel symbolToOrder

    { wModel with
        Wires = newChangedWires.Wires
        Symbol = { sModel with Symbols = Map.add finalSymbol'.Id finalSymbol' sModel.Symbols } }
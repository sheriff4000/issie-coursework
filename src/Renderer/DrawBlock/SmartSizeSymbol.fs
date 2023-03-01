module SmartSizeSymbol

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

open SmartHelpers
open BusWireUpdateHelpers

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart resize symbol" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and one symbols in the BusWire model so could use the SmartHelper 
    function for the wires.
*)


/// HLP23: To test this, it must be given two symbols interconnected by wires. It then resizes symbolToSize
/// so that the connecting wires are exactly straight
/// HLP23: It should work out the interconnecting wires (wires) from 
////the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has adjustable size).
/// HLP23: when this function is written replace teh XML comment by something suitable concisely
/// stating what it does.
/// 
/// 
/// HLP23: Luke
/// 

let resizeAndShift
    (wModel: Model)
    (connections: ((Edge*Edge)*Wire) list)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    (symbolOnTopOrLeft: bool)
    (vertical: bool)
    =
    let firstEdges,firstWire = connections[0]

    let connectionIds =
        connections
        |> List.map (fun ((_,_),wire) -> wire.WId)

    let symbolPortNumberFloat = float (Option.get ( if vertical then (getPortPositionFromTopOrLeft symbolToSize firstWire) else (getPortPositionFromTopOrLeft symbolToSize firstWire) ))
    
    let portDistanceSymbol = if vertical then getPortDistances symbolToSize (fst firstEdges) else getPortDistances symbolToSize (fst firstEdges)
    let portDistanceOther = if vertical then getPortDistances otherSymbol (snd firstEdges) else getPortDistances otherSymbol (snd firstEdges)

    let isTopOrLeft = if symbolOnTopOrLeft then 1.0 else -1.0

    let wireshift = (firstWire.Segments[firstWire.Segments.Length / 2].Length ) * (firstWire.Segments[firstWire.Segments.Length / 2 - 1].Length |> sign |> float) * isTopOrLeft
    let shift =if vertical then -symbolPortNumberFloat * (portDistanceOther - portDistanceSymbol) + wireshift else ( (symbolPortNumberFloat - 0.3) * (portDistanceSymbol - portDistanceOther) ) + wireshift

    let hScale = getScale symbolToSize.HScale
    let vScale = getScale symbolToSize.VScale

    let scalingH = if vertical then (portDistanceOther / portDistanceSymbol) * hScale else hScale
    let scalingV = if not vertical then (portDistanceOther / portDistanceSymbol) * vScale else vScale

    let verticalNum = if vertical then 1.0 else 0.0
    let horizontalNum = if not vertical then 1.0 else 0.0

    let symbol' = 
        {
            symbolToSize with 
                HScale = Some scalingH
                VScale = Some scalingV
                Pos = 
                    {
                        symbolToSize.Pos with 
                            X = symbolToSize.Pos.X + shift*verticalNum
                            Y = symbolToSize.Pos.Y + shift*horizontalNum
                    }
        }

    let wires' =
        wModel.Wires
        |> Map.map (fun id wire ->
            if List.contains id connectionIds
            then
                let wirePortNumberFloat: float = float (Option.get (getPortPositionFromTopOrLeft symbolToSize wire))
                let wiremove = wireshift + (wirePortNumberFloat - symbolPortNumberFloat)*(portDistanceOther - portDistanceSymbol)

                if isSymbolInputForWire symbolToSize firstWire
                then
                    {
                        wire with
                            Segments =
                                wire.Segments
                                |> List.map (fun x ->
                                    if x.Index = wire.Segments.Length / 2
                                    then { x with Length = x.Length + wiremove }
                                    else x )
                    }
                else
                    {
                        wire with
                            StartPos = 
                                { 
                                    wire.StartPos with 
                                        X = wire.StartPos.X + wiremove*verticalNum
                                        Y = wire.StartPos.Y + wiremove*horizontalNum
                                }
                            Segments =
                                wire.Segments
                                |> List.map (fun x ->
                                    if x.Index = wire.Segments.Length / 2
                                    then { x with Length = x.Length - wiremove }
                                    else x )
                    }

            else wire)        

    let sModel = wModel.Symbol
    {
        wModel with 
            Wires = wires'
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }

let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    match symbolToSize.Component.Type, otherSymbol.Component.Type with
    | Custom _, Custom _ ->

        let bottomOfSymbol = symbolToSize.Pos.Y + symbolToSize.Component.H*(getScale symbolToSize.VScale)
        let bottomOfOther = otherSymbol.Pos.Y + otherSymbol.Component.H*(getScale otherSymbol.VScale)
        let topOfSymbol = symbolToSize.Pos.Y
        let topOfOther = otherSymbol.Pos.Y

        let symbolOnTop = bottomOfSymbol < topOfOther
        let deadZoneV = inRange topOfSymbol topOfOther bottomOfOther || inRange bottomOfSymbol topOfOther bottomOfOther 
                                || inRange topOfOther topOfSymbol bottomOfSymbol || inRange bottomOfOther topOfSymbol bottomOfSymbol

        let rightOfSymbol = symbolToSize.Pos.X + symbolToSize.Component.W*(getScale symbolToSize.HScale)
        let rightOfOther = otherSymbol.Pos.X + otherSymbol.Component.W*(getScale otherSymbol.HScale)
        let leftOfSymbol = symbolToSize.Pos.X
        let leftOfOther = otherSymbol.Pos.X

        let symbolOnLeft = rightOfSymbol < leftOfOther
        let deadZoneH = inRange rightOfSymbol leftOfOther rightOfOther || inRange leftOfSymbol leftOfOther rightOfOther 
                                || inRange rightOfOther leftOfSymbol rightOfSymbol || inRange leftOfOther leftOfSymbol rightOfSymbol

        let adjacentConnections = getAdjacentConnections wModel symbolToSize otherSymbol

        let verticalConnections =
            adjacentConnections
            |> List.filter (fun ((edge1, edge2), _) -> (symbolOnTop && edge1=Bottom && edge2=Top) || (not symbolOnTop && edge1=Top && edge2=Bottom))
            // |> List.map (fun ((_, _), wire) -> wire)

        let horizontalConnections =
            adjacentConnections
            |> List.filter (fun ((edge1, edge2), _) -> (symbolOnLeft && edge1=Right && edge2=Left) || (not symbolOnLeft && edge1=Left && edge2=Right))
            // |> List.map (fun ((_, _), wire) -> wire)

        match verticalConnections.Length, horizontalConnections.Length with
        | x,_ when x>0 && not deadZoneV ->
            resizeAndShift wModel verticalConnections symbolToSize otherSymbol symbolOnTop true
        | _,x when x>0 && not deadZoneH ->
            resizeAndShift wModel horizontalConnections symbolToSize otherSymbol symbolOnLeft false
        | _,_ -> wModel

    | _, _ ->  wModel
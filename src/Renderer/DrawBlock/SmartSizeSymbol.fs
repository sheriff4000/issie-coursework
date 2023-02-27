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
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
        : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    match symbolToSize.Component.Type, otherSymbol.Component.Type with
    | Custom(_), Custom(_) ->
        let connectingWires = getConnectedWires wModel [symbolToSize.Id; otherSymbol.Id]
        let connectingWireIds = getConnectedWireIds wModel [symbolToSize.Id; otherSymbol.Id]
        let firstWire = connectingWires[0]

        let symbolPortNumberFloat = float (Option.get (getPortPositionFromLeft symbolToSize firstWire))
        let portDistanceSymbol = getPortDistancesH symbolToSize
        let portDistanceOther = getPortDistancesH otherSymbol

        let closerWire =
            if isSymbolInputForWire symbolToSize firstWire
            then 1
            else -1

        let wireshift = firstWire.Segments[firstWire.Segments.Length / 2].Length * float (sign firstWire.Segments[firstWire.Segments.Length / 2 + closerWire].Length) * (float closerWire)
        let scaling = (portDistanceOther / portDistanceSymbol) * Option.get symbolToSize.HScale
        let shift = -symbolPortNumberFloat * (portDistanceOther - portDistanceSymbol) + wireshift

        let symbol' = 
            {
                symbolToSize with 
                    HScale = Some scaling
                    Pos = {symbolToSize.Pos with X = symbolToSize.Pos.X + shift}
            }

        let wires' =
            wModel.Wires
            |> Map.map (fun id wire ->
                if List.contains id connectingWireIds
                then
                    let wirePortNumberFloat = float (Option.get (getPortPositionFromLeft symbolToSize wire))

                    if isSymbolInputForWire symbolToSize firstWire
                    then
                        {
                            wire with
                                Segments =
                                    wire.Segments
                                    |> List.map (fun x ->
                                        if x.Index = wire.Segments.Length / 2
                                        then
                                            {
                                                x with
                                                    Length = x.Length + wireshift + (wirePortNumberFloat - symbolPortNumberFloat)*(portDistanceOther - portDistanceSymbol)
                                            }
                                        else x)
                        }
                    else
                        {
                            wire with
                                StartPos = 
                                    {
                                        wire.StartPos with
                                            X = wire.StartPos.X + wireshift + (wirePortNumberFloat - symbolPortNumberFloat)*(portDistanceOther - portDistanceSymbol)
                                    }
                                Segments =
                                    wire.Segments
                                    |> List.map (fun x ->
                                        if x.Index = wire.Segments.Length / 2
                                        then
                                            {
                                                x with
                                                    Length = x.Length - wireshift - (wirePortNumberFloat - symbolPortNumberFloat)*(portDistanceOther - portDistanceSymbol)
                                            }
                                        else x)
                        }

                else wire)        

        let sModel = wModel.Symbol
        {
            wModel with 
                Wires = wires'
                Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }

    | _, _ ->  wModel
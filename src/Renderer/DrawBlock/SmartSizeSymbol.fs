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
    let sModel = wModel.Symbol

    let connectingWires = getConnectedWires wModel [symbolToSize.Id; otherSymbol.Id]
    let connectingWireIds = getConnectedWireIds wModel [symbolToSize.Id; otherSymbol.Id]

    let symbol',wires = 
        if List.length connectingWires > 1
        then 
            let firstWire = connectingWires[0]
            
            let symbolPortPosition =
                if isSymbolInputForWire symbolToSize firstWire
                then getPortPositionFromLeft symbolToSize (string firstWire.InputPort)
                else getPortPositionFromLeft symbolToSize (string firstWire.OutputPort)

            let symbolPortNumberFloat = 
                symbolPortPosition
                |> Option.get
                |> snd
                |> (fun x -> x+1)
                |> float

            let portDistanceSymbol = getPortDistancesH symbolToSize
            let portDistanceOther = getPortDistancesH otherSymbol

            let scaling = (portDistanceOther/ portDistanceSymbol) * Option.get symbolToSize.HScale
            let shift = -symbolPortNumberFloat * (portDistanceOther - portDistanceSymbol) + firstWire.Segments[firstWire.Segments.Length / 2].Length

            let wires' = wModel.Wires
                // wModel.Wires
                // |> Map.map (fun id wire ->
                //     if List.contains id connectingWireIds
                //     then
                //         if isSymbolInputForWire symbolToSize wire
                //         then
                //             {
                //                 wire with
                //                     StartPos = 
                //                         {
                //                             wire.StartPos with
                //                                 X = wire.StartPos.X + shift
                //                         }
                //                     Segments =
                //                         wire.Segments
                //                         |> List.map (fun x ->
                //                             if x.Index = wire.Segments.Length / 2
                //                             then
                //                                 {
                //                                     x with
                //                                         Length = x.Length + shift
                //                                 }
                //                             else x)
                //             }
                //         else 
                //             {
                //                 wire with
                //                     StartPos = 
                //                         {
                //                             wire.StartPos with
                //                                 X = wire.StartPos.X + shift
                //                         }
                //                     Segments =
                //                         wire.Segments
                //                         |> List.map (fun x ->
                //                             if x.Index = wire.Segments.Length / 2
                //                             then
                //                                 {
                //                                     x with
                //                                         Length = 0
                //                                 }
                //                             else x)
                //             }
                //     else wire)
            
            {
                symbolToSize with 
                    HScale = Some scaling
                    Pos = {symbolToSize.Pos with X = symbolToSize.Pos.X + shift}
            }, wires'

        else symbolToSize,wModel.Wires
                
    {
        wModel with 
            Wires = wires // no change for now, but probably this function should use update wires after resizing.
                                // to make that happen the test function which calls this would need to provide an updateWire
                                // function to this as a parameter (as was done in Tick3)
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }

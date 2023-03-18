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

type sizeHelpers = {
    UpdateSymbolWiresHelper: BusWireT.Model -> ComponentId -> BusWireT.Model
}


// HLP23: Luke
// This function is used to resize a component to a size to match the port distance of anther symbol
// and then shift the symbol so that it has straight wires connecting to that symbol.
// The function can do this when symbols are stacked vertically or next to each other horizontally.

// This function can only reshape 7 segment wires correctly with it's rerouting
let resizeAndShift
    (wModel: Model)
    (connections: (Edge*Edge) * Wire list)
    (symbolToSize: Symbol)
    (otherSymbol: Symbol)
    (sizeHelpers: sizeHelpers)
    =
    let firstEdges = fst connections
    let firstWire =
        connections
        |> snd
        |> List.item 0

    let stackedVertically = (fst firstEdges) = Top || (fst firstEdges) = Bottom
    
    let portDistanceSymbol = getPortDistances symbolToSize (fst firstEdges)
    let portDistanceOther = getPortDistances otherSymbol (snd firstEdges)

    let hScale = (1.0, symbolToSize.HScale) ||> Option.defaultValue 
    let vScale = (1.0, symbolToSize.VScale) ||> Option.defaultValue 

    let scalingH = if stackedVertically then Some ((portDistanceOther / portDistanceSymbol) * hScale) else None
    let scalingV = if not stackedVertically then Some ((portDistanceOther / portDistanceSymbol) * vScale) else None

    let verticalNum = if stackedVertically then 1.0 else 0.0
    let horizontalNum = if not stackedVertically then 1.0 else 0.0

    let symbolFirstPortPos = getPortXYPos firstWire symbolToSize portDistanceOther
    let otherFirstPortPos = getPortXYPos firstWire otherSymbol portDistanceOther

    let shift =
        if stackedVertically
        then otherFirstPortPos.X - symbolFirstPortPos.X
        else otherFirstPortPos.Y - symbolFirstPortPos.Y

    let symbol' = 
        {
            symbolToSize with 
                HScale = scalingH
                VScale = scalingV
                Pos = 
                    {
                        symbolToSize.Pos with 
                            X = symbolToSize.Pos.X + shift*verticalNum
                            Y = symbolToSize.Pos.Y + shift*horizontalNum
                    }
                LabelBoundingBox =
                    {
                        symbolToSize.LabelBoundingBox with 
                            TopLeft =
                                {
                                    symbolToSize.LabelBoundingBox.TopLeft with 
                                        X = symbolToSize.LabelBoundingBox.TopLeft.X + shift*verticalNum
                                        Y = symbolToSize.LabelBoundingBox.TopLeft.Y + shift*horizontalNum
                                }
                    }
        }

    let sModel = wModel.Symbol

    let wireModel =
        ({
                wModel with 
                    Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }, symbolToSize.Id)
        ||> sizeHelpers.UpdateSymbolWiresHelper       

    {
        wModel with 
            Wires = wireModel.Wires
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
    }

// HLP23: Luke
// This function is the main function for SmartSizeSymbol.
// It first checks if the two components are Custom components and if they are, works out in which
// direction the symbol should be resized and shifted, if the two components have at least one connected wire.
let reSizeSymbol 
    (wModel: BusWireT.Model) 
    (symbolToSize: Symbol) 
    (otherSymbol: Symbol) 
    (sizeHelpers: sizeHelpers)
        : BusWireT.Model =
    printfn $"ReSizeSymbol: ToResize:{symbolToSize.Component.Label}, Other:{otherSymbol.Component.Label}"

    match symbolToSize.Component.Type, otherSymbol.Component.Type with
    | Custom _, Custom _ ->
        let deadZoneV = inDeadZone symbolToSize otherSymbol false
        let deadZoneH = inDeadZone symbolToSize otherSymbol true

        let adjacentConnections = getAdjacentConnections wModel symbolToSize otherSymbol
        let symbolEdge = adjacentConnections |> fst |> fst

        match symbolEdge with
        | x when (x = Top || x = Bottom) && not deadZoneV ->
            resizeAndShift wModel adjacentConnections symbolToSize otherSymbol sizeHelpers
        | x when (x = Left || x = Right) && not deadZoneH ->
            resizeAndShift wModel adjacentConnections symbolToSize otherSymbol sizeHelpers
        | _ -> wModel
    | _, _ ->  wModel
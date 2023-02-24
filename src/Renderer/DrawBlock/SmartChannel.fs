module SmartChannel

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open SmartHelpers
open Optics
open Operators

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart channel route" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires in the BusWire model so could use the SmartHelper function for
    this purpose.
*)

type MovementDirection = Left | Right

let print x = 
    printfn "%A" x

let getXOfVerticalSegmentWire (wire: Wire) = //used for checkWireInChannel
    wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length

let getYOfVerticalSegmentWire (wire: Wire) = //used for checkWireInChannel
    wire.StartPos.Y, (wire.StartPos.Y + wire.Segments[3].Length)

let checkWireInChannel (channel: BoundingBox) (wire: Wire) = // checks if a wire is in a channel (returns a bool)
    //used in getWiresInChannel
    
    //define the leftmost, topmost, rightmost, and downmost of the bounding box
    let right = channel.TopLeft.X
    let top = channel.TopLeft.Y
    let left = right - channel.W
    let bottom = top + channel.H
    // print "left:"
    // print left
    // print "right:"
    // print right
    // print "wireX:"
    
    //check whether the x values are valid
    let wireX = getXOfVerticalSegmentWire wire
    let validX = (wireX > left) && (wireX < right)
    //check whether the y values are valid
    let wireTopY, wireBottomY = getYOfVerticalSegmentWire wire
    let validY = (wireTopY < bottom) && (wireBottomY > top)
    //check if the whole wire segment is valid
    (validX && validY && ((List.length wire.Segments) = 7 ))
    
let getWiresInChannel (wires: Map<ConnectionId, Wire>) (channel: BoundingBox) = //returns list of ID's of all wires in the channel 
    (Map.toList wires) 
    |> List.filter (fun (x,y) -> checkWireInChannel channel y)
    |> List.map (fun (x, y) -> x)

let orderWires (modelWires: Map<ConnectionId, Wire>) (ids: List<ConnectionId>) = //takes list of id's of wires in the channel and sorts them left to right
    
    let getWidth (wire: Wire) = 
        print "end wire lengths are:"
        print wire.Segments[0].Length
        print wire.Segments[6].Length
        wire.Segments[0].Length + wire.Segments[2].Length + wire.Segments[4].Length + wire.Segments[6].Length

    let getMidpoint (wire: Wire) = 
        wire.StartPos.X + ((getWidth wire) / 2.0)
    ids
    |> List.sortBy (fun id -> (Map.find id modelWires) |> getMidpoint ) 
    |> List.sortBy (fun id -> (Map.find id modelWires) |> getWidth)


let wireSpacer (channel: BoundingBox) (ids: list<ConnectionId>) = 
    //returns an array of what X coordinate each vertical segment should be
    let numberOfWires = float (List.length ids)
    [1.0..numberOfWires] |> List.map (fun x -> channel.TopLeft.X - channel.W + ((channel.W / (numberOfWires + 1.0 )) * x))



let moveVerticalSegment (segments: List<Segment>) (amount: float) (direction: MovementDirection) = 
    //moves middle segment to the left for negative amount and to the right for positive amount
    let totalAvailableLength = segments[2].Length + segments[4].Length
    let possibleRightLength = max 0.0 (segments[4].Length - amount)
    let possibleLeftLength = max 0.0 (segments[2].Length - amount)
    match direction with
    | Left -> segments |> List.mapi (fun i x -> if i = 2 then {x with Length=possibleLeftLength} else if i = 4 then {x with Length=totalAvailableLength - possibleLeftLength} else x)
    | Right -> segments |> List.mapi (fun i x -> if i = 2 then {x with Length=totalAvailableLength - possibleRightLength} else if i = 4 then {x with Length=possibleRightLength} else x)
    
let moveWire (xCoordinate: float) (wire: Wire) = //moves middle segment of wire so that it is either the xcoordinate or as close as possible to it
    let verticalSegmentX = getXOfVerticalSegmentWire wire
    let amount = xCoordinate - verticalSegmentX
    match amount with
    | x when x < 0 -> {wire with Segments = moveVerticalSegment wire.Segments (-x) Left}
    | x when x > 0 -> {wire with Segments = moveVerticalSegment wire.Segments x Right}
    | _ -> wire
    
let moveWires (wires: Map<ConnectionId, Wire>) (wireSpacings: list<float>) (ids: list<ConnectionId>)  = 
    let combined = List.zip wireSpacings ids
    let updateWires (wires: Map<ConnectionId, Wire>) (wireSpacing, id)  = 
        let wire = Map.find id wires
        let updatedWire = moveWire wireSpacing wire
        wires |> Map.add id updatedWire

    combined |> List.fold (updateWires) wires
    





let smartChannelRoute //spaces wires evenly 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    //new steps:
    // 1. check which wires go through the channel
    // 2. straighten the wires so that any wires a vertical section within the channel have the vertical section outside the channel
    // 3. create an order for the wires
    // 4. create a list of the height each wire should be
    // 5. move the wires so that they are that height
    // 6. test and make sure vertical is fully implemented
    // 7. think about corner cases for both vertical and horizontal and try and make more complex

    let tr = channel.TopLeft

    //printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    let wireIdsInChannel = getWiresInChannel model.Wires channel
    print "number of wires in general: "
    print (List.length (Map.toList model.Wires))
    print "number of wires in channel:"
    print (List.length wireIdsInChannel)
    let sortedIdsInChannel = orderWires model.Wires wireIdsInChannel
    let wireSpacings = wireSpacer channel sortedIdsInChannel
    let left = channel.TopLeft.X - channel.W
    let right = channel.TopLeft.X
    print "left, "
    print left
    print "right, "
    print right
    print wireSpacings
    {model with Wires = moveWires model.Wires wireSpacings sortedIdsInChannel}
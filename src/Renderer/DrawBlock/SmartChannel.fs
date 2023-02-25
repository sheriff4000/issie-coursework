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

let getXOfLeftHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.X + wire.Segments[0].Length, wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length

let getYOfLeftHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.Y + wire.Segments[1].Length

let getXOfRightHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length, wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length + wire.Segments[4].Length

let getYOfRightHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.Y + wire.Segments[1].Length + wire.Segments[3].Length

let moveVerticalSegment (segments: List<Segment>) (amount: float) (direction: MovementDirection) = 
    //moves middle segment to the left for negative amount and to the right for positive amount
    let totalAvailableLength = segments[2].Length + segments[4].Length
    let possibleRightLength = max 0.0 (segments[4].Length - amount)
    let possibleLeftLength = max 0.0 (segments[2].Length - amount)
    match direction with
    | Left -> segments |> List.mapi (fun i x -> if i = 2 then {x with Length=possibleLeftLength} else if i = 4 then {x with Length=totalAvailableLength - possibleLeftLength} else x)
    | Right -> segments |> List.mapi (fun i x -> if i = 2 then {x with Length=totalAvailableLength - possibleRightLength} else if i = 4 then {x with Length=possibleRightLength} else x)
    
let moveWireVerticalSegment (xCoordinate: float) (wire: Wire) = //moves middle segment of wire so that it is either the xcoordinate or as close as possible to it
    (xCoordinate - (getXOfVerticalSegmentWire wire))
    |> function
        | x when x < 0 -> {wire with Segments = moveVerticalSegment wire.Segments (-x) Left}
        | x when x > 0 -> {wire with Segments = moveVerticalSegment wire.Segments x Right}
        | _ -> wire

let checkHorizontalWireInChannel (channel: BoundingBox) (wire: Wire) =
    let right = channel.TopLeft.X + channel.W
    let top = channel.TopLeft.Y
    let left = channel.TopLeft.X
    let bottom = top + channel.H
    //check whether the x values are valid
    //checking left horizontal segment
    let leftSegmentY = getYOfLeftHorizontalSegmentWire wire
    let leftValidY = (leftSegmentY > top) && (leftSegmentY < bottom)
    //check whether the y values are valid
    let leftSegmentLeft, leftSegmentRight = getXOfLeftHorizontalSegmentWire wire
    let leftValidX = (leftSegmentLeft < right) && (leftSegmentRight > left)
    //check if the whole wire segment is valid
    let leftSegmentValid = leftValidX && leftValidY

    //checking right horizontal segment
    let rightSegmentY = getYOfRightHorizontalSegmentWire wire
    let rightValidY = (rightSegmentY > top) && (rightSegmentY < bottom)
    //check whether the y values are valid
    let rightSegmentLeft, rightSegmentRight = getXOfRightHorizontalSegmentWire wire
    let rightValidX = (rightSegmentLeft < right) && (rightSegmentRight > left)
    //check if the whole wire segment is valid
    let rightSegmentValid = rightValidX && rightValidY 

    //returning true if either of the horizontal segments are within the channel
    (leftSegmentValid || rightSegmentValid) && ((List.length wire.Segments) = 7 )

let checkVerticalWireInChannel (channel: BoundingBox) (wire: Wire)  = // checks if a wire is in a channel (returns a bool)
    //define the leftmost, topmost, rightmost, and downmost of the bounding box
    let right = channel.TopLeft.X + channel.W
    let top = channel.TopLeft.Y
    let left = channel.TopLeft.X
    let bottom = top + channel.H
    //check whether the x values are valid
    let wireX = getXOfVerticalSegmentWire wire
    let validX = (wireX > left) && (wireX < right)
    //check whether the y values are valid
    let wireTopY, wireBottomY = getYOfVerticalSegmentWire wire
    let validY = (wireTopY < bottom) && (wireBottomY > top)
    //check if the whole wire segment is valid
    (validX && validY && ((List.length wire.Segments) = 7 ))

let getCheckWireInChannelFunction = 
    function
        | Horizontal -> checkHorizontalWireInChannel
        | Vertical -> checkVerticalWireInChannel

let getWiresInChannel (channelOrientation: Orientation) (wires: Map<ConnectionId, Wire>) (channel: BoundingBox) = //returns list of ID's of all wires in the channel 
    let checkWireFunction = getCheckWireInChannelFunction channelOrientation
    (Map.toList wires) 
    |> List.filter (fun (x,y) -> checkWireFunction channel y)
    |> List.map (fun (x, y) -> x)
    

let checkWireNeedsStraightening (channel: BoundingBox) (modelWires: Map<ConnectionId, Wire>) (id: ConnectionId)  = 
    let wire = Map.find id modelWires
    let right = channel.TopLeft.X + channel.W
    let top = channel.TopLeft.Y
    let left = channel.TopLeft.X
    let bottom = top + channel.H
    let verticalX = getXOfVerticalSegmentWire wire
    let verticalTopY, verticalBottomY = getYOfVerticalSegmentWire wire
    (verticalX > left) && (verticalX < right) && (verticalTopY > top) && (verticalBottomY < bottom) && (wire.Segments[3].Length <> 0)

let straightenHorizontalWire (modelWires: Map<ConnectionId, Wire>) (id: ConnectionId) = 
    let wire = Map.find id modelWires
    let newWire = moveWireVerticalSegment (wire.StartPos.X) wire
    Map.add id newWire modelWires
    
let straightenHorizontalWires (model: Model) (ids: List<ConnectionId>) (channel: BoundingBox) = 
    let modelWires = model.Wires
    let newModelWires = 
        ids 
        |> List.filter (fun x -> checkWireNeedsStraightening channel model.Wires x)
        |> List.fold (fun state x -> straightenHorizontalWire state x) modelWires
    {model with Wires = newModelWires}

let orderWires (modelWires: Map<ConnectionId, Wire>) (ids: List<ConnectionId>) = //takes list of id's of wires in the channel and sorts them left to right
    
    let getWidth (wire: Wire) = 
        wire.Segments[0].Length + wire.Segments[2].Length + wire.Segments[4].Length + wire.Segments[6].Length

    let getMidpoint (wire: Wire) = 
        wire.StartPos.X + ((getWidth wire) / 2.0)
    ids
    |> List.sortBy (fun id -> (Map.find id modelWires) |> getMidpoint ) 
    |> List.sortBy (fun id -> (Map.find id modelWires) |> getWidth)


let wireSpacer (channel: BoundingBox) (ids: list<ConnectionId>) = 
    //returns an array of what X coordinate each vertical segment should be
    let numberOfWires = float (List.length ids)
    [1.0..numberOfWires] |> List.map (fun x -> channel.TopLeft.X + ((channel.W / (numberOfWires + 1.0 )) * x))




    
let moveWires (wires: Map<ConnectionId, Wire>) (wireSpacings: list<float>) (ids: list<ConnectionId>)  = 
    let updateWires (wires: Map<ConnectionId, Wire>) (wireSpacing, id)  = 
        let wire = Map.find id wires
        let updatedWire = moveWireVerticalSegment wireSpacing wire
        wires |> Map.add id updatedWire

    List.zip wireSpacings ids
    |> List.fold (updateWires) wires
    





let smartChannelRoute //spaces wires evenly 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =

    //tasks to do:
    // 1. create horizontal bounding boxes, in addition to vertical ones (done)
    // 2. check which wires go through the channel (done)
    // 3. straighten the wires so that any wires a vertical section within the channel have the vertical section outside the channel (done)
    // 4. create an order for the wires
    // 5. create a list of the height each wire should be
    // 6. move the wires so that they are that height
    // 7. change let left, let right, let top etc.. to be one function, and create data structure Bounds and use it everywhere
    // 8. Work on fixing when the channels aren't spread properly (find two wires which are very close to each other and swap them then re-space)
    // 9. make everything neat and tidy

    if channelOrientation = Horizontal then
        print "we are in a horizional channel!"
        let wireIdsInChannel = getWiresInChannel channelOrientation model.Wires channel 
        print "number of wires in general: "
        print (List.length (Map.toList model.Wires))
        print "number of wires in channel:"
        print (List.length wireIdsInChannel)
    

        straightenHorizontalWires model wireIdsInChannel channel
    else
        print "we are in a vertical channel"
        let tr = channel.TopLeft

        //printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
        let wireIdsInChannel = getWiresInChannel channelOrientation model.Wires channel 
        print "number of wires in general: "
        print (List.length (Map.toList model.Wires))
        print "number of wires in channel:"
        print (List.length wireIdsInChannel)
        let sortedIdsInChannel = orderWires model.Wires wireIdsInChannel
        let wireSpacings = wireSpacer channel sortedIdsInChannel
        
        print wireSpacings
        {model with Wires = moveWires model.Wires wireSpacings sortedIdsInChannel}
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

let print x =
    printfn "%A" x

type BoxSides = { 
//defines the left x coordinate, right x coordinate, top y coordinate, bottom y coordinate of a bounding box.
        Left: float;
        Right: float;
        Top: float;
        Bottom: float
    }

type WireMovementInfo = {
    Wire: ConnectionId;
    StartSegment: int;
    EndSegment: int
} 

type MovementDirection = Left | Right 

let YCoordinateEachSegment (wire: Wire) =
    //this can be a smart helper -- make another one for XCoordinate
    let folder (initialOrientation: Orientation) (state: float) (index:int) (segment: Segment) = 
        if (index % 2 = 0 && initialOrientation = Vertical) || (index % 2 = 1 && initialOrientation = Horizontal) then
            state + segment.Length
        else
            state
    wire.Segments
    |> List.zip [0..(List.length wire.Segments)-1]
    |> List.scan (fun state (index, segment) -> folder wire.InitialOrientation state index segment) wire.StartPos.Y  


let getInOutSegments (channel: BoundingBox) (wire: Wire)  =
        let segMap = WireToLineSegs wire
        let channelBox = boundingBoxToBoxLines channel
        let channelIntersects (line: LineSeg) = 
            let topIntersect = LineSegIntersect channelBox.top line
            let leftIntersect = LineSegIntersect channelBox.left line
            let botIntersect = LineSegIntersect channelBox.bottom line
            let rightIntersect = LineSegIntersect channelBox.right line
            let topOut : Edge list = 
                if Option.isSome topIntersect then
                    [Top]
                else 
                    []
            let botOut : Edge list = 
                if Option.isSome botIntersect then
                    [Bottom]
                else 
                    []
            let leftOut : Edge list = 
                if Option.isSome leftIntersect then
                    [Edge.Left]
                else 
                    []
            let rightOut : Edge list = 
                if Option.isSome rightIntersect then
                    [Edge.Right]
                else 
                    []
            line , (topOut @ botOut @ leftOut @ rightOut)

        let intersectList =
            (List.ofSeq (Map.values segMap))
            |> List.map channelIntersects
            |> List.filter (fun (_,list) -> list <> [])

        let rec leftSeg (intersects: (LineSeg * Edge list) list) = 
            print "current len of left list"
            print (List.length intersects)
            match intersects with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Left) edges) then
                       Some (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        leftSeg tl
                | [] -> None

        let rec rightSeg (intersects: (LineSeg * Edge list) list) = 
            print "current length of list right:"
            print (List.length intersects)
            match intersects with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Right) edges) then
                       Some  (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        rightSeg tl
                | [] -> None 

        let finalLeft = leftSeg intersectList
        let finalRight = rightSeg intersectList


        finalLeft, finalRight

let getWiresInChannel (channel: BoundingBox) (model: Model)  = 
    model.Wires
    |> Map.toList
    |> List.map (fun (connectionId, wire) -> connectionId, (getInOutSegments channel wire))
    |> List.filter (fun (connectionId, (left, right)) -> (Option.isSome left) && (Option.isSome right))
    |> List.map (fun (connectionId: ConnectionId, (left, right)) -> {Wire = connectionId; StartSegment = (Option.get left); EndSegment = (Option.get right)})

let straightenWire (model: Model) (wire: Wire) (startSegment: int) (endSegment: int) : Wire =
    let startStart, startEnd = getSegPositions wire startSegment
    let endStart, endEnd = getSegPositions wire endSegment
    let segments = wire.Segments
    if (startStart.Y = startEnd.Y) && (endStart.Y = endEnd.Y) then //if both segments are horizontal
        
        let segMove = endStart.Y - startStart.Y
        let horiDist = endStart.X - startEnd.X
        let segFolder currWire seg =
            let segStart, segEnd = getSegPositions wire seg
            let segmove = endStart.Y - segStart.Y
            if segStart.Y = segEnd.Y then
                moveSegment model currWire.Segments[seg] segmove
            else
                currWire

        let newSegments = 
            [startSegment+1..endSegment-1]
            |> List.map (fun x -> 
                let segStart, segEnd = getSegPositions wire x
                if segStart.X = segEnd.X then 
                    {segments[x] with Length = 0}
                else
                    segments[x]   
                )

        let newSegments2 wire = 
            (wire, [startSegment+1..endSegment-1])
            ||> List.fold segFolder



        let newStartSeg = {segments[startSegment] with Length = segments[startSegment].Length + horiDist}
        let segList = segments[..startSegment-1] @ [newStartSeg] @ (newSegments2 wire).Segments[startSegment+1..endSegment-1] @ segments[endSegment..]
        let wireToMove = {wire with Segments = segList}
        moveSegment model wireToMove.Segments[startSegment] segMove
    else
        wire

let straightenWires (model:Model) (wiresToStraighten: List<WireMovementInfo>) = 
    let wires = model.Wires
    let newWires = 
        wiresToStraighten
        |> List.fold  (fun state wireInfo -> Map.add wireInfo.Wire (straightenWire model (Map.find wireInfo.Wire state) wireInfo.StartSegment wireInfo.EndSegment) state) wires
    {model with Wires = newWires}


let getWireOrder (model: Model) (wires: list<ConnectionId>) = 
    let getFinalHeight (wire: Wire) = 
        let folder (initialOrientation: Orientation) (state: float) (index:int) (segment: Segment) = 
            if (index % 2 = 0 && initialOrientation = Vertical) || (index % 2 = 1 && initialOrientation = Horizontal) then
                state + segment.Length
            else
                state
        wire.Segments
        |> List.zip [0..(List.length wire.Segments)-1]
        |> List.fold (fun state (index, segment) -> folder wire.InitialOrientation state index segment) wire.StartPos.Y

    wires
    |> List.sortBy (fun wire -> (Map.find wire model.Wires) |> getFinalHeight)

let getWireSpacings (channel: BoundingBox) (ids: list<ConnectionId>) = 
    //returns an array of what X coordinate each vertical segment should be
    let numberOfWires = float (List.length ids)
    [1.0..numberOfWires] 
    |> List.map (fun x -> channel.TopLeft.Y + ((channel.H / (numberOfWires + 1.0 )) * x))



let moveWire (model: Model) (wireId: ConnectionId) (startSegment: int) (endSegment: int) (yCoordinate: float): Wire = 
    let wire = Map.find wireId model.Wires
    let segmentYCoordinates = YCoordinateEachSegment wire
    let currentY = segmentYCoordinates[startSegment]
    let movement = yCoordinate - currentY
    let remainder = startSegment % 2
    wire.Segments
    |> List.zip [0..(List.length wire.Segments) - 1]
    |> List.fold (fun wire (index, segment) -> if ((index % 2 = remainder) && (index >= startSegment) && (index <= endSegment)) then (moveSegment model segment movement) else wire) wire

let moveWires (model: Model) (wiresToMove: List<WireMovementInfo>) (wireSpacings: List<float>)=
    let newWires = 
        wiresToMove
        |> List.zip wireSpacings 
        |> List.fold (fun state (wireSpacing, wire) -> Map.add wire.Wire (moveWire model wire.Wire wire.StartSegment wire.EndSegment wireSpacing) state) model.Wires
    {model with Wires = newWires}


let smartChannelRoute //spaces wires evenly 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =

    //for now the implementation only works with horizontal wires
    //to be used for vertical, we will either map the functions to the 
    //vertical equivalent or just rotate the whole channel and then do smart channel then rotate back
    //functions to be used:
    ///     - getWiresInChannel -> returns all the wires in the channel (AJ)
    ///     - getStartSegment -> returns the starting wire segment of the channel entry (SA) (return an int)
    ///     - getEndSegment -> returns the wire segment ending the channel exit (SA) (return an int)
    ///     - straightenWire -> straightens the wire in between the start and end segments (SA) (return a Wire)
    ///     - getWireConstraints -> determines the highest y and lowest y each wire can be in the channel (AJ)
    ///     - getWireOrder -> gets the order the wires should be in based on the wire constraints (AJ)
    ///     - getWireSpacings -> determines the y value each straightened channel section should be at (AJ)
    ///     - moveWires -> moves the straightened sections to these y values. (AJ)
    print "hi"
    if channelOrientation = Vertical then
        model
    else
        print "here doing horizontal"
        let wiresInChannel = getWiresInChannel channel model
        print "got out of function"
        print (List.length wiresInChannel)
        


        //model
        straightenWires model wiresInChannel



   
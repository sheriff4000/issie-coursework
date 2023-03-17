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


let XCoordinateEachSegment (wire: Wire) =
    //this can be a smart helper -- make another one for XCoordinate
    let folder (initialOrientation: Orientation) (state: float) (index:int) (segment: Segment) = 
        if (index % 2 = 0 && initialOrientation = Vertical) || (index % 2 = 1 && initialOrientation = Horizontal) then
            state
        else
            state + segment.Length
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
            let rightOut =
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

let getWireOrder (model: Model) (wires: list<WireMovementInfo>) = 
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
    |> List.sortBy (fun wire -> (Map.find wire.Wire model.Wires) |> getFinalHeight)

let getWireSpacings (channel: BoundingBox) (ids: list<WireMovementInfo>) = 
    //returns an array of what X coordinate each vertical segment should be
    let numberOfWires = float (List.length ids)
    [1.0..numberOfWires] 
    |> List.map (fun x -> channel.TopLeft.Y + ((channel.H / (numberOfWires + 1.0 )) * x))


let moveSegment2 (currentHeight: float) (yCoordinate: float) (wire: Wire) (segmentIndex: int) =
    let moveHorizontalSegment (segments: List<Segment>) (amount: float) = 
        let rightHorizontal: float = segments[segmentIndex+1].Length
        let leftHorizontal: float = segments[segmentIndex-1].Length
        let amountSign = amount > 0
        let leftSign = leftHorizontal > 0
        let rightSign = rightHorizontal > 0
        segments
        |> List.mapi (fun i x -> 
            if (i = segmentIndex - 1 && (leftHorizontal <> 0 || i = 1)) then 
                {x with Length=x.Length+amount} 
            
            elif (i = segmentIndex + 1 && (rightHorizontal <> 0 || i = (List.length segments) - 2))then
                {x with Length=x.Length-amount} 
            else x 
            )

    let amount = yCoordinate - currentHeight
    {wire with Segments = moveHorizontalSegment wire.Segments amount}


let moveWire (model: Model) (wireId: ConnectionId) (startSegment: int) (endSegment: int) (yCoordinateTarget: float): Wire = 
    let wire = Map.find wireId model.Wires
    let segmentYCoordinates = YCoordinateEachSegment wire
    
    let remainder = startSegment % 2
  
    [0..(List.length wire.Segments) - 1]
    |> List.zip segmentYCoordinates[..(List.length segmentYCoordinates)-2]
    |> List.fold (fun wire (yCoordinate, index) -> if ((index % 2 = remainder) && (index >= startSegment) && (index <= endSegment)) then (moveSegment2 yCoordinate yCoordinateTarget wire index) else wire) wire

let moveWires (model: Model) (wiresToMove: List<WireMovementInfo>) (wireSpacings: List<float>) =
    let modelWires = model.Wires
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


    print "hi"
    if channelOrientation = Vertical then
        model
    else
        print "here doing horizontal"
        let wiresInChannel = getWiresInChannel channel model
        print "got out of function"
        print (List.length wiresInChannel)
        let spacings = getWireSpacings channel wiresInChannel
        let sortedWires = getWireOrder model wiresInChannel
        moveWires model sortedWires spacings
        // let wireInChannel = wiresInChannel[0]
        // let wireId = wireInChannel.Wire
        // let wire = Map.find wireId (model.Wires)
        // print "hi"
        // let startSegment = wireInChannel.StartSegment
        // let currentHeights = YCoordinateEachSegment wire
        // print "here"
        // let newWire = moveSegment2 currentHeights[startSegment] (currentHeights[startSegment] - 100.0) wire startSegment
        // {model with Wires = Map.add wireId newWire model.Wires} 
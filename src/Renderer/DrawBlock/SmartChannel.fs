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
open SmartWire

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

// type BoxSides = { 
// //defines the left x coordinate, right x coordinate, top y coordinate, bottom y coordinate of a bounding box.
//         Left: float;
//         Right: float;
//         Top: float;
//         Bottom: float
//     }

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
    |> List.scan (fun state (index, segment) -> folder wire.InitialOrientation state index segment) wire.StartPos.X  


/// Returns a tuple of int options defining which segments of the wire enter and exit the channel. 
let getInOutSegments (channelOrientation: Orientation) (channel: BoundingBox) (wire: Wire)  =
        let segMap = WireToLineSegs wire
        //let channelBox = boundingBoxToBoxLines channel
        let channelIntersects (line: LineSeg) = 
            let topIntersect = LineSegIntersect channel.Top line
            let leftIntersect = LineSegIntersect channel.Left line
            let botIntersect = LineSegIntersect channel.Bottom line
            let rightIntersect = LineSegIntersect channel.Right line
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
            // print "current len of left list"
            // print (List.length intersects)
            match intersects with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Left) edges) then
                       Some (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        leftSeg tl
                | [] -> None

        let rec rightSeg (intersects: (LineSeg * Edge list) list) = 
            // print "current length of list right:"
            // print (List.length intersects)
            match intersects with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Right) edges) then
                       Some  (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        rightSeg tl
                | [] -> None 
                
        let rec topSeg (intersects: (LineSeg * Edge list) list) = 
            //print "current len of left list"
            //print (List.length intersects)
            match intersects with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Top) edges) then
                       Some (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        topSeg tl
                | [] -> None

        let rec botSeg (intersects: (LineSeg * Edge list) list) = 
            //print "current length of list right:"
            //print (List.length intersects)
            match intersects with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Bottom) edges) then
                       Some  (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        botSeg tl
                | [] -> None 

        let tmpIn = if channelOrientation = Horizontal then leftSeg intersectList else topSeg intersectList
        let tmpOut = if channelOrientation = Horizontal then rightSeg intersectList else botSeg intersectList

        tmpIn, tmpOut

/// determines which wires are in a given channel
let getWiresInChannel (channelOrientation: Orientation)(channel: BoundingBox) (model: Model)  = 
    model.Wires
    |> Map.toList
    |> List.map (fun (connectionId, wire) -> connectionId, (getInOutSegments channelOrientation channel wire))
    |> List.filter (fun (connectionId, (inSeg, outSeg)) -> (Option.isSome inSeg) && (Option.isSome outSeg))
    |> List.map (fun (connectionId, (inSeg, outSeg)) -> 
        let seg1 = Option.get inSeg
        let seg2 = Option.get outSeg
        (connectionId, ((min seg1 seg2), (max seg1 seg2))))
    |> List.map (fun (connectionId: ConnectionId, (inSeg, outSeg)) -> {Wire = connectionId; StartSegment = (inSeg); EndSegment = (outSeg)})

let getWireOrder (channelOrientation: Orientation) (model: Model) (wires: list<WireMovementInfo>) = 
    let getFinalHeight (wire: Wire) = 
        let folder (initialOrientation: Orientation) (state: float) (index:int) (segment: Segment) = 
            if (index % 2 = 0 && initialOrientation = Vertical) || (index % 2 = 1 && initialOrientation = Horizontal) then
                state + segment.Length
            else
                state
        wire.Segments
        |> List.zip [0..(List.length wire.Segments)-1]
        |> List.fold (fun state (index, segment) -> folder wire.InitialOrientation state index segment) wire.StartPos.Y

    let getFinalLength (wire: Wire) = 
        let folder (initialOrientation: Orientation) (state: float) (index:int) (segment: Segment) = 
            if (index % 2 = 0 && initialOrientation = Vertical) || (index % 2 = 1 && initialOrientation = Horizontal) then
                state
            else
                state + segment.Length
        wire.Segments
        |> List.zip [0..(List.length wire.Segments)-1]
        |> List.fold (fun state (index, segment) -> folder wire.InitialOrientation state index segment) wire.StartPos.Y

    match channelOrientation with
    | Horizontal -> 
        wires
        |> List.sortBy (fun wire -> (Map.find wire.Wire model.Wires) |> getFinalHeight)
    | Vertical ->
        wires
        |> List.sortBy (fun wire -> (Map.find wire.Wire model.Wires) |> getFinalLength)

let getWireSpacings (channelOrientation: Orientation) (channel: BoundingBox) (ids: list<WireMovementInfo>) = 
    //returns an array of what X coordinate each vertical segment should be
    let numberOfWires = float (List.length ids)
    match channelOrientation with
    | Horizontal -> 
        [1.0..numberOfWires] 
        |> List.map (fun x -> channel.TopLeft.Y + ((channel.H / (numberOfWires + 1.0 )) * x))
    | Vertical ->
        [1.0..numberOfWires] 
        |> List.map (fun x -> channel.TopLeft.X + ((channel.W / (numberOfWires + 1.0 )) * x))

let moveSegment2 (currentHeight: float) (yCoordinate: float) (wire: Wire) (segmentIndex: int) =
    let moveHorizontalSegment (segments: List<Segment>) (amount: float) = 
        let rightHorizontal: float = segments[segmentIndex+1].Length
        let leftHorizontal: float = segments[segmentIndex-1].Length
        let amountSign = amount > 0
        let leftSign = leftHorizontal > 0
        let rightSign = rightHorizontal > 0
        segments
        |> List.mapi (fun i x -> 
            if (i = segmentIndex - 1 && (leftHorizontal <> 0 || i = 1 || i = (List.length segments) - 2)) then 
                {x with Length=x.Length+amount} 
            
            elif (i = segmentIndex + 1 && (rightHorizontal <> 0 || i = 1 || i = (List.length segments) - 2))then
                {x with Length=x.Length-amount} 
            else x 
            )
    let amount = yCoordinate - currentHeight
    {wire with Segments = moveHorizontalSegment wire.Segments amount}



let moveWire (channelOrientation: Orientation) (model: Model) (wireId: ConnectionId) (startSegment: int) (endSegment: int) (coordinateTarget: float): Wire = 
    let wire = Map.find wireId model.Wires

    let segmentCoordinates = 
        match channelOrientation with
        | Horizontal -> 
            YCoordinateEachSegment wire
        | Vertical -> 
            XCoordinateEachSegment wire
    
    let remainder = startSegment % 2
  
    [0..(List.length wire.Segments) - 1]
    |> List.zip segmentCoordinates[..(List.length segmentCoordinates)-2]
    |> List.fold (fun wire (segmentCoordinate, index) -> if ((index % 2 = remainder) && (index >= startSegment) && (index <= endSegment)) then (moveSegment2 segmentCoordinate coordinateTarget wire index) else wire) wire

let moveWires(channelOrientation: Orientation) (model: Model) (wiresToMove: List<WireMovementInfo>) (wireSpacings: List<float>) =
    let modelWires = model.Wires
    let newWires = 
        wiresToMove
        |> List.zip wireSpacings 
        |> List.fold (fun state (wireSpacing, wire) -> Map.add wire.Wire (moveWire channelOrientation model wire.Wire wire.StartSegment wire.EndSegment wireSpacing) state) model.Wires
        //|> Map.map (fun _ wire -> smartAutoroute model wire)
    {model with Wires = newWires}


let smartChannelRoute //spaces wires evenly 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =

    let wiresInChannel: WireMovementInfo list = getWiresInChannel channelOrientation channel model
    print "number of wires in channel"
    print (List.length wiresInChannel)
    let spacings = getWireSpacings channelOrientation channel wiresInChannel
    let sortedWires = getWireOrder channelOrientation model wiresInChannel

    moveWires channelOrientation model sortedWires spacings
  
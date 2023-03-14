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

type BoxSides = { 
//defines the left x coordinate, right x coordinate, top y coordinate, bottom y coordinate of a bounding box.
        Left: float;
        Right: float;
        Top: float;
        Bottom: float
    }


type MovementDirection = Left | Right 

let getWiresInChannel (model: Model) (channel: BoundingBox) : List<ConnectionId> = 
    //TO BE IMPLEMENTED
    //returns a list of all wire id's in a channel
    failwithf("not implemented yet")

let getInOutSegments (wire: Wire) (channel: BoundingBox) =
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
            match intersects with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Left) edges) then
                       Some (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        leftSeg tl
                | [] -> None

        let rec rightSeg (intersects: (LineSeg * Edge list) list) = 
            match intersectList with
                | (line, edges)::tl -> 
                    if Option.isSome (List.tryFind (fun (el: Edge) -> el = Edge.Right) edges) then
                       Some  (Map.findKey (fun _ v -> v =line) segMap)
                    else 
                        rightSeg tl
                | [] -> None 

        let finalLeft = leftSeg intersectList
        let finalRight = rightSeg intersectList


        finalLeft, finalRight

let straightenWire (model: Model) (wire: Wire) (startSegment: int) (endSegment: int) : Wire =
    let startStart, startEnd = getSegPositions wire startSegment
    let endStart, endEnd = getSegPositions wire endSegment

    if (startStart.Y = startEnd.Y) && (endStart.Y = endEnd.Y) then //if both segments are horizontal
        let segMove = endStart.Y - startStart.Y
        moveSegment model wire.Segments[startSegment] segMove
    else
        wire


let getWireOrder (model: Model) (wires: list<ConnectionId>) = 
    let getFinalHeight (wire: Wire) = 
        
     


// let straightenWire (model: Model) (wire: ConnectionId) (startSegment: int) (endSegment: int) : Wire = 
//     //TO BE IMPLEMENTED BY SHERIF
//     failwithf("not implemented yet")


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


    //for a horizontal channel, adjusts wires so that only one horizontal segment in a wire runs through the channel. 
    let straightenedModel = straightenHorizontalWires channel channelOrientation model sortedWireIdsInChannel
    
    //allocates a spacing for each wire. An x coordinate for vertical channels and a y coordinate for horizontal channels./
    let wireSpacings = wireSpacer channelOrientation channel sortedWireIdsInChannel

    if channelOrientation = Vertical then
        //creating a new order improves the wire ordering in the vertical channel. It deals with edge cases that the previous ordering algorithm
        //doesn't always deal with effectively
        let newOrder = improveWireOrder channel wireSpacings straightenedModel.Wires sortedWireIdsInChannel
        //moves wires to their associated spacing, or as close as possible to it if not possible
        {straightenedModel with Wires = moveWires channelOrientation straightenedModel.Wires wireSpacings newOrder}
    else    
        //moves wires to their associated spacing, or as close as pottible to it if not possible
        {straightenedModel with Wires = moveWires channelOrientation straightenedModel.Wires wireSpacings sortedWireIdsInChannel}
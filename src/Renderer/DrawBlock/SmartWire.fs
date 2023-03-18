module SmartWire
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

///HLP23: Sherif

type AddSegType = Previous | Next | Both | Neither

/// wire offset defines how close a wire can be to a component
let wireOffset = 10.


///this function converts a component to the boxLines format 
let ComponentToBox (comp: Component) =
    let topLeft = {X = comp.X; Y = comp.Y}
    {TopLeft = topLeft; W = comp.W; H = comp.H}

/// this is a variant of the boxUnion function, opting to have the second parameter as a list of boxes, finding the union of the whole set
let unionBoxLines (box1: BoundingBox) (newBoxes: BoundingBox list) = 
    let maxLeft = List.max (box1.Left.Start.X::(List.map (fun (x: BoundingBox) -> x.Left.Start.X) newBoxes))
    let maxRight = List.max ((box1.Top.Finish.X::(List.map (fun (x: BoundingBox) -> x.Top.Finish.X) newBoxes)))
    let maxTop = List.max (box1.Top.Start.Y::(List.map (fun (x: BoundingBox) -> x.Top.Start.Y) newBoxes))
    let maxBot = List.max (box1.Bottom.Start.Y::(List.map (fun (x: BoundingBox) -> x.Bottom.Start.Y) newBoxes))
    let topLeft = {X = maxLeft; Y = maxTop}
    let botLeft = {X = maxLeft; Y = maxBot}
    let topRight = {X = maxRight; Y = maxTop}
    let botRight = {X = maxRight; Y = maxBot}

    {
        TopLeft = topLeft;
        W = topRight.X-topLeft.X
        H = botLeft.Y - topLeft.Y
    }
/// converts a wire to a map of index and line segments, allowing a simple translation between segment index and its corresponding lineSegment
let WireToLineSegs (wire: Wire) = 
    segmentsToIssieVertices wire.Segments wire
    |> List.map (fun (x,y,_) -> {X = x; Y = y})
    |> List.pairwise
    |> List.map (fun (startpoint,endpoint) -> {Start = startpoint; Finish = endpoint})
    |> List.mapi (fun idx line -> (idx, line))
    //|> List.filter (fun (idx,_) -> idx = 0 )
    |> Map.ofList

///finds the intersection between two LineSegs if it exists -> could be made more accessible to others in group phase
/// by taking StartPoint and endPoint for both wires, no need right now though
let LineSegIntersect (l1: LineSeg) (l2: LineSeg) : XYPos Option = 
    let dx1 = l1.Finish.X - l1.Start.X 
    let dy1 = l1.Finish.Y - l1.Start.Y
    let dx2 = l2.Finish.X - l2.Start.X 
    let dy2 = l2.Finish.Y - l2.Start.Y

    if (dx1 = 0 && dx2 = 0) || (dy1 = 0 && dy2 = 0) then //if both horizontal or both vertical
        None
    else
        let h, v = if l1.Start.Y = l1.Finish.Y then (l1, l2) else (l2, l1)
        let minX = min h.Start.X h.Finish.X
        let maxX = max h.Start.X h.Finish.X
        let minY = min v.Start.Y v.Finish.Y
        let maxY = max v.Start.Y v.Finish.Y
        if v.Start.X >= minX && v.Start.X <= maxX && h.Start.Y >= minY && h.Start.Y <= maxY then
            Some { X = v.Start.X; Y = h.Start.Y}
        else
            None

/// calculates the distance a wiresegment needs to move in order to avoid it
let LineMove (box: BoundingBox) (line: LineSeg) =
    let distToCentre = segmentIntersectsBoundingBox box line.Start line.Finish
    if  Option.isNone distToCentre then
        0.
    else
        let dist = Option.get distToCentre
        match (getSegmentOrientation line.Start line.Finish) with
            | Horizontal -> 
                if (line.Start.Y <= box.Centre().Y) then
                    (box.TopLeft.Y - wireOffset) - box.Centre().Y + dist
                else 
                    (box.TopLeft.Y + box.H + wireOffset) - box.Centre().Y - dist
            | Vertical ->
                if (line.Start.X <= box.Centre().X) then
                    (box.TopLeft.X - wireOffset) - box.Centre().X + dist
                else 
                    (box.TopLeft.X + box.W + wireOffset) - box.Centre().X - dist

/// finds the intersection positions between a line segent and a box
/// note that only either (top or bottom) or (left or right) are needed
/// this is because the current version of the algorithm would do the same thing 
/// for left and right or top and bottom
let SegBoxIntersect (box: BoundingBox) (line: LineSeg) = 
    let topIntersect = LineSegIntersect box.Top line
    let leftIntersect = LineSegIntersect box.Left line
    let botIntersect = LineSegIntersect box.Bottom line
    let rightIntersect = LineSegIntersect box.Right line
    let topOut = 
        if Option.isSome topIntersect then
            [(Top, (Option.get topIntersect))]
        else 
            []
    let botOut = 
        if Option.isSome botIntersect then
            [(Bottom, (Option.get botIntersect))]
        else
            []
    let leftOut = 
        if Option.isSome leftIntersect then
            [(Left, Option.get (leftIntersect))]
        else
            []
    let rightOut = 
        if Option.isSome rightIntersect then
            [(Right, (Option.get rightIntersect))]
        else
            []
    topOut @ botOut @ leftOut @ rightOut



/// This function replaces a wire Segment with either 1, 3 or 5 segments dependent on the addType,
/// with the intention of making an otherwise immovable segment movable
let addFakeSegs (addType: AddSegType) (wire: Wire) (idx: int) (intersect: Intersect) = 
    let initStart, initFinish = getSegPositions wire idx
    let segments = wire.Segments
    let totalLen = segments[idx].Length
    let vertIntersect = getSegmentOrientation initStart initFinish = Vertical
    let prevLen, nextLen =
        if vertIntersect then
            let startToBottom = intersect.Box.Bottom.Start.Y+wireOffset-initStart.Y
            let startToTop = intersect.Box.Top.Start.Y-wireOffset-initStart.Y
            let bottomToFinish = initFinish.Y-intersect.Box.Bottom.Start.Y+wireOffset
            let topToFinish = initFinish.Y-intersect.Box.Top.Start.Y-wireOffset
            let lenCheck = (2.0 * (abs startToBottom)) < (abs totalLen)
            if lenCheck then
                startToBottom, topToFinish
            else
                startToTop, bottomToFinish
        else
            let startToLeft = intersect.Box.Left.Start.X-wireOffset-initStart.X
            let startToRight = intersect.Box.Right.Start.X+wireOffset-initStart.X
            let leftToFinish = initFinish.X-intersect.Box.Left.Start.Y-wireOffset
            let rightToFinish = initFinish.X-intersect.Box.Right.Start.Y+wireOffset
            let lenCheck = (2.0 * (abs startToLeft)) < (abs totalLen)
            if lenCheck then
                startToLeft, rightToFinish
            else
                startToRight, leftToFinish
    let boxSegLen = 
        let doubleOffset = (2.0*wireOffset)
        if vertIntersect then
            intersect.Box.H + doubleOffset
        else
            intersect.Box.W + doubleOffset

    let midLen = 
        if addType = Both then
            boxSegLen
        elif addType = Previous then
            boxSegLen + nextLen
        else 
            prevLen + boxSegLen
    let defaultSeg = {segments[idx-1] with Length = 1.; Mode = Auto}
    let newSegs = 
        if addType <> Neither then 
            segments[..(idx-1)] 
            @ if (addType = Previous || addType = Both) then [{defaultSeg with Length = prevLen}] @ [defaultSeg] else []
            @ [{segments[idx] with Length = midLen; Mode = Auto}] 
            @ if (addType = Next || addType = Both) then [defaultSeg] @ [{defaultSeg with Length = nextLen}] else []
            @ segments[(idx+1)..]
        else
            segments
    let mappedSegs = 
        newSegs
        |> List.mapi (fun idx seg -> {seg with Index = idx})
    let newIdx = 
        if addType = Both || addType = Previous then 
            idx + 2
        else
            idx
    
    {wire with Segments = mappedSegs}, newIdx


let smartAutoroute (model: Model) (wire: Wire) = 
    let destPos, startPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    let segMap = WireToLineSegs wire
    let getIntersects model  = 
 
        let componentBoxes = 
            getComponentInfo model
            |> Map.values
            |> List.ofSeq
            |> List.map ComponentToBox

        List.allPairs componentBoxes (List.ofSeq (Map.values segMap))
        |> List.collect (fun (box, line) -> (List.map (fun x -> box, line, x) (SegBoxIntersect box line)))
        |> List.map (fun (box', line', (intersect', position')) -> 
            {
                Box = box'; 
                Line = line'; 
                IntersectType = intersect'; 
                Position = position'; 
                //Index = segMap[line']
                Index = Map.findKey (fun idx line -> line' = line) segMap
            }
            )
        |> List.filter (fun (i: Intersect) -> 
            i.Position <> startPos && i.Position <> destPos)

    let initIntersects = getIntersects model

    /// recursive function to iterate through and handle intersects one at a time
    let rec wireRecursive currWire (intersects: Intersect list) (channelBoundingBoxes: BoundingBox list) = 
        let segMapUpdate = WireToLineSegs currWire
    

        match intersects with
            | intersect::tl ->
                // printfn "%A" intersect.intersectType
                let segIndex = 
                    intersect.Index
                let dist = LineMove intersect.Box intersect.Line

                // let addType = 
                //     if not contradiction then
                //         if (segments[segIndex-1].Mode = Manual && segments[segIndex+1].Mode = Manual) || segments[segIndex].Mode = Manual then
                //             Both
                //         elif segments[segIndex-1].Mode = Manual then
                //             Previous
                //         elif segments[segIndex+1].Mode = Manual then
                //             Next
                //         else Neither
                //     else
                //         Both
                //let longerWire, newSegIdx = addFakeSegs addType currWire segIndex intersect
                let newWire = 
                    // if newSegIdx = 0 then 
                    //     let tmpWire, tmpIdx = addFakeSegs Previous longerWire newSegIdx intersect
                    //     moveSegment model tmpWire.Segments[tmpIdx] dist //intersect
                    // elif newSegIdx = longerWire.Segments.Length-1 then
                    //     let tmpWire, tmpIdx = addFakeSegs Next longerWire newSegIdx intersect
                    //     moveSegment model tmpWire.Segments[tmpIdx] dist //intersect
                    // else
                    //     moveSegment model longerWire.Segments[newSegIdx] dist

                    moveSegment model currWire.Segments[segIndex] dist
                    //find the bounding box we are moving around
                    //direction the wire is moving to the side of the bounding box
                    //create a bounding box in on that side
                    //run smartChannels on this bounding box
                //let componentboundingBox = Intersect.Box)
                let directionOfMovement: Edge = 
                    if intersect.IntersectType = Top || intersect.IntersectType = Bottom then
                        if dist > 0 then
                            Right
                        else
                            Left
                    else
                        if dist > 0  then
                            Bottom
                        else
                            Top
                    
                let channelBoundingBox = 
                    match directionOfMovement with
                    | Top -> 
                        let topLeft = {intersect.Box.TopLeft with Y = intersect.Box.TopLeft.Y-50.0}
                        {TopLeft = topLeft; W = intersect.Box.W; H = 50.0}
                    | Bottom ->
                        let topLeft = {intersect.Box.TopLeft with Y = intersect.Box.TopLeft.Y + intersect.Box.H}
                        {TopLeft = topLeft; W = intersect.Box.W; H = 50.0}
                    | Left-> 
                        let topLeft = {intersect.Box.TopLeft with X = intersect.Box.TopLeft.X-50.0}
                        {TopLeft = topLeft; H = intersect.Box.H; W = 50.0}
                    | Right ->
                        let topLeft = {intersect.Box.TopLeft with X = intersect.Box.TopLeft.X + intersect.Box.W}
                        {TopLeft = topLeft; H = intersect.Box.H; W = 50.0}
                        

                let currIntersects = getIntersects model 
                let contradiction = ((List.length currIntersects) >= (List.length intersects)) && intersects <> currIntersects

                let newIntersect =
                    if contradiction then 
                        let newIntersects = listDifference currIntersects intersects
                        let unionBox = 
                            if not (List.isEmpty newIntersects) then
                                Some (unionBoxLines intersect.Box (List.map (fun x->x.Box ) newIntersects))
                            else
                                None
                        if Option.isSome unionBox then
                            {intersect with Box = (Option.get unionBox)}
                        else
                            intersect
                    else
                        intersect  
                if newIntersect <> intersect then 
                    wireRecursive newWire (newIntersect::tl) (channelBoundingBox::channelBoundingBoxes)
                else    
                    wireRecursive newWire tl (channelBoundingBox::channelBoundingBoxes)
            | [] -> (currWire, channelBoundingBoxes)
    
    if not (List.isEmpty initIntersects) && startPos = segMap[0].Start && destPos = segMap[(Map.count segMap) - 1].Finish then 
        wireRecursive wire initIntersects []
    else
        ((autoroute model wire), [])
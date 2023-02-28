module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

type LineSeg = 
    {
        start: XYPos
        finish: XYPos
    }
    member this.length = (this.finish.X-this.start.X) + (this.finish.Y-this.start.Y)

type boxLines =
    {
        top: LineSeg
        bottom: LineSeg
        left: LineSeg
        right: LineSeg
        W: float
        H: float
    }

//type IntersectType = Top | Bottom | Left | Right
type AddSegType = Previous | Next | Both | Neither

type Intersect = {
    box: boxLines
    line: LineSeg 
    intersectType: Edge
    position: XYPos
    }
let wireOffset = 10.

let ComponentToBox (comp: Component) =
    let topLeft = {X = comp.X; Y = comp.Y}
    let topRight = {X = comp.X + comp.W; Y = comp.Y}
    let botLeft = {X = comp.X; Y = comp.Y + comp.H}
    let botRight = {X = comp.X + comp.W; Y = comp.Y + comp.H}

    let t = {start = topLeft; finish = topRight}
    let b = {start = botLeft; finish = botRight}
    let l = {start = topLeft; finish = botLeft}
    let r = {start = topRight; finish = botRight}

    let outBox = {top = t; bottom = b; left = l; right = r; W = comp.W; H = comp.H}
    outBox

let unionBoxLines (box1: boxLines) (box2: boxLines) = 
    let maxLeft = max box1.left.start.X box2.left.start.X
    let maxRight = max box1.top.finish.X box2.top.finish.X
    let maxTop = max box1.top.start.Y box2.top.start.Y
    let maxBot = max box1.bottom.start.Y box2.bottom.start.Y

    let topLeft = {X = maxLeft; Y = maxTop}
    let botLeft = {X = maxLeft; Y = maxBot}
    let topRight = {X = maxRight; Y = maxTop}
    let botRight = {X = maxRight; Y = maxBot}

    {top = {start = topLeft; finish = topRight}; 
        bottom = {start = botLeft; finish = botRight}; 
        left = {start = topLeft; finish = botLeft}; 
        right = {start = topRight; finish = botRight};
        H = botLeft.Y - topLeft.Y;
        W = topRight.X - topLeft.X
    }

let WireToLineSegs (wire: Wire) = 
    segmentsToIssieVertices wire.Segments wire
    |> List.map (fun (x,y,_) -> {X = x; Y = y})
    |> List.pairwise
    |> List.map (fun (startpoint,endpoint) -> {start = startpoint; finish = endpoint})
    |> List.mapi (fun idx line -> (idx, line))
    //|> List.filter (fun (idx,_) -> idx = 0 )
    |> Map.ofList


let LineSegIntersect (l1: LineSeg) (l2: LineSeg) : XYPos Option = 
    let dx1 = l1.finish.X - l1.start.X 
    let dy1 = l1.finish.Y - l1.start.Y
    let dx2 = l2.finish.X - l2.start.X 
    let dy2 = l2.finish.Y - l2.start.Y

    if (dx1 = 0 && dx2 = 0) || (dy1 = 0 && dy2 = 0) then //if both horizontal or both vertical
        None
    else
        let h, v = if l1.start.Y = l1.finish.Y then (l1, l2) else (l2, l1)
        let minX = min h.start.X h.finish.X
        let maxX = max h.start.X h.finish.X
        let minY = min v.start.Y v.finish.Y
        let maxY = max v.start.Y v.finish.Y
        if v.start.X >= minX && v.start.X <= maxX && h.start.Y >= minY && h.start.Y <= maxY then
            Some { X = v.start.X; Y = h.start.Y}
        else
            None

let boundingToLines (box: BoundingBox) = 
    let topRight = {Y = box.TopLeft.Y; X = box.TopLeft.X + box.W}
    let botLeft = {X = box.TopLeft.X; Y = box.TopLeft.Y+box.H}
    let botRight = {X = box.TopLeft.X + box.W; Y = box.TopLeft.Y+box.H}
    let topLeft = box.TopLeft

    {top = {start = topLeft; finish = topRight}; 
        bottom = {start = botLeft; finish = botRight}; 
        left = {start = topLeft; finish = botLeft}; 
        right = {start = topRight; finish = botRight};
        H = botLeft.Y - topLeft.Y;
        W = topRight.X - topLeft.X
    }

let linesToBounding (box: boxLines) = 
    {TopLeft = box.top.start;
    H = box.left.finish.Y-box.left.start.Y;
    W = box.top.finish.X - box.top.start.X}

let LineMove (box: BoundingBox) (line: LineSeg) =
    let distToCentre = segmentIntersectsBoundingBox box line.start line.finish
    if  Option.isNone distToCentre then
        0.
    else
        let dist = Option.get distToCentre
        match (getSegmentOrientation line.start line.finish) with
            | Horizontal -> 
                if (line.start.Y <= box.Centre().Y) then
                    (box.TopLeft.Y - wireOffset) - box.Centre().Y + dist
                else 
                    (box.TopLeft.Y + box.H + wireOffset) - box.Centre().Y - dist
            | Vertical ->
                if (line.start.X <= box.Centre().X) then
                    (box.TopLeft.X - wireOffset) - box.Centre().X + dist
                else 
                    (box.TopLeft.X + box.W + wireOffset) - box.Centre().X - dist

let SegBoxIntersect (box: boxLines) (line: LineSeg) = 
    let topIntersect = LineSegIntersect box.top line
    let leftIntersect = LineSegIntersect box.left line
    let botIntersect = LineSegIntersect box.bottom line
    let rightIntersect = LineSegIntersect box.right line
    let topOut = 
        if Option.isSome topIntersect then
            [(Top, (Option.get topIntersect))]
        elif Option.isSome botIntersect then
            [(Bottom, (Option.get botIntersect))]
        else 
            []
    let leftOut = 
        if Option.isSome leftIntersect then
            [(Left, Option.get (leftIntersect))]
        elif Option.isSome rightIntersect then
            [(Right, (Option.get rightIntersect))]
        else
            []
    // elif Option.isSome rightIntersect then
        // (Right, rightIntersect)
    topOut @ leftOut

let getSegPositions wire idx = 
    let segMap = WireToLineSegs wire
    let segPositions = segMap[idx]

    segPositions.start, segPositions.finish



let addFakeSegs (addType: AddSegType) (wire: Wire) (idx: int) (intersect: Intersect) = 
    let initStart, initFinish = getSegPositions wire idx
    let segments = wire.Segments
    let totalLen = segments[idx].Length
    let vertIntersect = getSegmentOrientation initStart initFinish = Vertical

    let prevLen, nextLen =
        if vertIntersect then
            let startToBottom = intersect.box.bottom.start.Y+wireOffset-initStart.Y
            let startToTop = intersect.box.top.start.Y-wireOffset-initStart.Y
            let bottomToFinish = initFinish.Y-intersect.box.bottom.start.Y+wireOffset
            let topToFinish = initFinish.Y-intersect.box.top.start.Y-wireOffset
            let lenCheck = (2.0 * (abs startToBottom)) < (abs totalLen)
            if lenCheck then
                startToBottom, topToFinish
            else
                startToTop, bottomToFinish
        else
            let startToLeft = intersect.box.left.start.X-wireOffset-initStart.X
            let startToRight = intersect.box.right.start.X+wireOffset-initStart.X
            let leftToFinish = initFinish.X-intersect.box.left.start.Y-wireOffset
            let rightToFinish = initFinish.X-intersect.box.right.start.Y+wireOffset
            let lenCheck = (2.0 * (abs startToLeft)) < (abs totalLen)
            if lenCheck then
                startToLeft, rightToFinish
            else
                startToRight, leftToFinish
            
    let boxSegLen = 
        let doubleOffset = (2.0*wireOffset)
        if vertIntersect then
            intersect.box.H + doubleOffset
        else
            intersect.box.W + doubleOffset

    let midLen = 
        if addType = Both then
            boxSegLen
        elif addType = Previous then
            boxSegLen + nextLen
        else 
            prevLen + boxSegLen


    let defaultSeg = {segments[idx-1] with Length = 0.; Mode = Auto}
    let newSegs = 
        if addType <> Neither then 
            segments[..(idx-1)] 
            @ if (addType = Previous || addType = Both) then [{defaultSeg with Length = prevLen}] @ [defaultSeg] else []
            @ [{segments[idx] with Length = midLen}] 
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

let moveSeg2 (model:Model) (seg:Segment) (distance:float) (intersect: Intersect) = 
    let wire = model.Wires[seg.WireId]
    let segments = wire.Segments
    let idx = seg.Index

    if idx = 0 then // Should never happen
        printfn $"Trying to move wire segment {seg.Index}:{logSegmentId seg}, out of range in wire length {segments.Length}, adding segments next" |> ignore
        let longerWire, _ = addFakeSegs Previous wire idx intersect
        longerWire
    elif idx = segments.Length - 1 then
        printfn $"Trying to move wire segment {seg.Index}:{logSegmentId seg}, out of range in wire length {segments.Length}, adding segments previous"
        let longerWire, _ = addFakeSegs Next wire idx intersect
        longerWire
    elif idx < 0 || idx > segments.Length - 1 then 
        printfn "bad things" |> ignore
        wire
    else
        let safeDistance = getSafeDistanceForMove segments idx distance
        let prevSeg = segments[idx - 1]
        let nextSeg = segments[idx + 1]
        let movedSeg = segments[idx]

        let newPrevSeg = { prevSeg with Length = prevSeg.Length + safeDistance } 
        let newNextSeg = { nextSeg with Length = nextSeg.Length - safeDistance }
        let newMovedSeg = { movedSeg with Mode = Manual }
        let newSegments = 
            segments[.. idx - 2] @ [newPrevSeg; newMovedSeg; newNextSeg] @ segments[idx + 2 ..]

        { wire with Segments = newSegments }

let smartAutoroute (model: Model) (wire: Wire): Wire = 
    let segments = wire.Segments
    let destPos, startPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    //printfn $"start pos = {startPos}" |> ignore
    let segMap = WireToLineSegs wire
    let getIntersects model wire = 
        
        let componentBoxes = 
            SmartHelpers.getComponentInfo model
            |> Map.values
            |> List.ofSeq
            |> List.map ComponentToBox
    
    //let (intersects: Intersect list) =
        List.allPairs componentBoxes (List.ofSeq (Map.values segMap))
        |> List.collect (fun (box, line) -> (List.map (fun x -> box, line, x) (SegBoxIntersect box line)))
        //|> List.filter (fun (_, _, (intersect, position)) -> (intersect, position) <> (Top, None))
        |> List.map (fun (box', line', (intersect', position')) -> {box = box'; line = line'; intersectType = intersect'; position = position'})
        |> List.filter (fun (i: Intersect) -> 
            //printfn $"intersect pos = {i.position}"
            //printfn $"intersect pos = start pos {i.position = startPos}"
            i.position <> startPos && i.position <> destPos)

    let initIntersects = getIntersects model wire

    let intersectPrinter (intersect: Intersect): unit = 
        let segIndex = 
            Map.tryFindKey (fun _ l -> l = intersect.line) segMap
            |> Option.defaultValue 0
        printfn $"intersection at {intersect.intersectType} of component, segment = {segIndex}, linemove would be {LineMove (linesToBounding intersect.box) intersect.line}"

    initIntersects
    |> List.map intersectPrinter
    |> ignore

    let rec wireRecursive currWire (intersects: Intersect list) = 
        let currIntersects = getIntersects model currWire
        /// contradiction is true when the previous change resulted in a separate intersection 
        /// between a wire and a symbol
        let contradiction = ((List.length currIntersects) >= (List.length intersects)) && intersects <> currIntersects
        if List.isEmpty currIntersects then
            currWire
        else
            match intersects with
                | intersect::_ ->
                    let segIndex = 
                        Map.tryFindKey (fun _ l -> l = intersect.line) segMap
                        |> Option.defaultValue 0 //Shouldn't happen
                    //let dist = LineMove intersect.intersectType intersect.position intersect.box
                    let dist = LineMove (linesToBounding intersect.box) intersect.line

                    let addType = 
                        if not contradiction then
                            if (segments[segIndex-1].Mode = Manual && segments[segIndex+1].Mode = Manual) || segments[segIndex].Mode = Manual then
                                Both
                            elif segments[segIndex-1].Mode = Manual then
                                Previous
                            elif segments[segIndex+1].Mode = Manual then
                                Next
                            else Neither
                        else
                            Both
                    printfn $"alpha add type = {addType} need linemove of {dist}" |> ignore
                    let longerWire, newSegIdx = addFakeSegs addType currWire segIndex intersect
                    let newWire = 
                        //moveSegment model longerWire.Segments[newSegIdx] dist
                        moveSeg2 model longerWire.Segments[newSegIdx] dist intersect
                    let newIntersect =
                        if contradiction then 

                            let newIntersects = SmartHelpers.listDifference currIntersects intersects
                            printfn $"alpha newIntersects length {List.length newIntersects}" |> ignore

                            let oldIntersects = SmartHelpers.listDifference intersects currIntersects
                            printfn $"alpha oldIntersects length {List.length oldIntersects}"
                            printfn $"alpha contradiction with intersect {newIntersects[0].intersectType}" |> ignore
                            let unionBox = 
                                //if (List.length newIntersects = 1) && (List.length oldIntersects = 1) then
                                if not (List.isEmpty newIntersects) && (newIntersects[0].box <> intersect.box) then  //&& not (List.isEmpty oldIntersects) then
                                    Some (unionBoxLines newIntersects[0].box intersect.box)
                                else
                                    None
                            if Option.isSome unionBox then
                                printfn "new intersection added" |> ignore
                                {intersect with box = (Option.get unionBox)}
                            else
                                intersect
                        else
                            intersect  
                    if newIntersect <> intersect then 
                        wireRecursive newWire (newIntersect::(List.tail intersects))
                    else    
                        wireRecursive newWire (List.tail intersects)


                | [] -> currWire
    
    if not (List.isEmpty initIntersects) && startPos = segMap[0].start && destPos = segMap[(Map.count segMap) - 1].finish then 
        wireRecursive wire initIntersects
    else
        autoroute model wire
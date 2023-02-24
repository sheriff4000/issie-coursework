﻿module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators

(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)

/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
/// 
type LineSeg = 
    {
        start: XYPos
        finish: XYPos
    }
type boxAsLines =
    {
        top: LineSeg
        bottom: LineSeg
        left: LineSeg
        right: LineSeg
    }

type IntersectType = Top | Bottom | Left | Right

type Intersect = {
    box: boxAsLines
    line: LineSeg 
    intersectType: IntersectType 
    position: Option<XYPos>
    }

let ComponentToBox (comp: Component) =
    let topLeft = {X = comp.X; Y = comp.Y}
    let topRight = {X = comp.X + comp.W; Y = comp.Y}
    let botLeft = {X = comp.X; Y = comp.Y + comp.H}
    let botRight = {X = comp.X + comp.W; Y = comp.Y + comp.H}

    let t = {start = topLeft; finish = topRight}
    let b = {start = botLeft; finish = botRight}
    let l = {start = topLeft; finish = botLeft}
    let r = {start = topRight; finish = botRight}

    let outBox = {top = t; bottom = b; left = l; right = r}
    outBox

let WireToLineSegs (wire: Wire) = 
    let wireStart = wire.StartPos
    let segMap = Map.empty
    let folder initialOrientation (currPos: XYPos) (seg:Segment) =

        if initialOrientation = Horizontal then // evens are horizontal
            if seg.Index % 2 = 0 then
                {currPos with X = currPos.X + seg.Length}
            else
                {currPos with Y = currPos.Y + seg.Length}
        else // evens are vertical
            if seg.Index % 2 = 0 then
                {currPos with Y = currPos.Y + seg.Length}
            else
                {currPos with X = currPos.X + seg.Length}

    (wireStart, wire.Segments)
    ||> List.scan (folder wire.InitialOrientation)
    |> List.pairwise
    |> List.map (fun (startpoint,endpoint) -> {start = startpoint; finish = endpoint})
    |> List.mapi (fun idx line -> (idx, line))
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

let SegBoxIntersect (box: boxAsLines) (line: LineSeg) = 
    let topIntersect = LineSegIntersect box.top line
    let leftIntersect = LineSegIntersect box.left line
    let botIntersect = LineSegIntersect box.bottom line
    let rightIntersect = LineSegIntersect box.right line
    
    if Option.isSome topIntersect && Option.isSome botIntersect then
        (Top, topIntersect)
    elif Option.isSome leftIntersect && Option.isSome rightIntersect then
        (Left, leftIntersect)
    elif Option.isSome topIntersect && Option.isSome leftIntersect then
        (Top, topIntersect)
    elif Option.isSome botIntersect && Option.isSome rightIntersect then
        (Bottom, botIntersect)
    elif Option.isSome topIntersect && Option.isSome rightIntersect then
        (Top, topIntersect)
    elif Option.isSome botIntersect && Option.isSome leftIntersect then
        (Bottom, botIntersect)
    // elif Option.isSome botIntersect then
        // (Bottom, botIntersect)
    // elif Option.isSome rightIntersect then
        // (Right, rightIntersect)
    else 
        (Top, None)

let LineMove intersectType position (box: boxAsLines) =
    let wireOffset = 5.0
    if position <> None then 
        let (pos: XYPos) = Option.get position
        if intersectType = Top || intersectType = Bottom then
            let midpoint = ((box.top.finish.X - box.top.start.X) / 2.0) + box.top.start.X
            if pos.X < midpoint then
                box.top.start.X - pos.X - wireOffset
            else 
                box.top.finish.X - pos.X + wireOffset
        else 
            let midpoint = ((box.left.finish.Y - box.left.start.Y) / 2.0) + box.left.start.Y
            if pos.Y < midpoint then
                box.left.start.Y - pos.Y - wireOffset
            else 
                box.left.finish.Y - pos.Y + wireOffset
    else
        0.0

let moveSeg2 (model:Model) (seg:Segment) (distance:float) = 
    let wire = model.Wires[seg.WireId]
    let segments = wire.Segments
    let idx = seg.Index

    if idx <= 0 || idx >= segments.Length - 1 then // Should never happen
        printfn $"Trying to move wire segment {seg.Index}:{logSegmentId seg}, out of range in wire length {segments.Length}"
        wire
    else
        let prevSeg = segments[idx - 1]
        let nextSeg = segments[idx + 1]
        let movedSeg = segments[idx]

        let newPrevSeg = { prevSeg with Length = prevSeg.Length + distance }
        let newNextSeg = { nextSeg with Length = nextSeg.Length - distance }
        //let newMovedSeg = { movedSeg with Mode = Manual }
        let newSegments = 
            segments[.. idx - 2] @ [newPrevSeg; movedSeg; newNextSeg] @ segments[idx + 2 ..]

        { wire with Segments = newSegments }


let smartAutoroute (model: Model) (wire: Wire): Wire = 
    let segMap = WireToLineSegs wire
    let getIntersects model wire = 
        
        let componentBoxes = 
            SmartHelpers.getComponentInfo model
            |> Map.values
            |> List.ofSeq
            |> List.map ComponentToBox
    
    //let (intersects: Intersect list) =
        List.allPairs componentBoxes (List.ofSeq (Map.values segMap))
        |> List.map (fun (box, line) -> box, line, (SegBoxIntersect box line))
        |> List.filter (fun (_, _, (intersect, position)) -> (intersect, position) <> (Top, None))
        |> List.map (fun (box', line', (intersect', position')) -> {box = box'; line = line'; intersectType = intersect'; position = position'})

    let initIntersects = getIntersects model wire

    let intersectPrinter (intersect: Intersect): unit = 
        let segIndex = 
            Map.tryFindKey (fun _ l -> l = intersect.line) segMap
            |> Option.defaultValue 0
        printfn $"intersection at {intersect.intersectType} of component, segment = {segIndex}, linemove would be {LineMove intersect.intersectType intersect.position intersect.box}"

    initIntersects
    |> List.map intersectPrinter
    |> ignore

    // let wireChange currWire (intersect: Intersect) = 
    //     match intersect with
    //         | (box, line, ((intersectType: IntersectType), (position: XYPos Option))) ->
    //             let segIndex = 
    //                 Map.tryFindKey (fun _ l -> l = line) segMap
    //                 |> Option.defaultValue 0
    //             let dist = LineMove intersectType position box

                    
    //             moveSegment model currWire.Segments[segIndex] dist

    //let contradictingRoute = 
    let rec wireRecursive currWire (intersects: Intersect list) = 
        let currIntersects = getIntersects model currWire

        /// contradiction is true when the previous change resulted in a separate intersection 
        /// between a wire and a symbol
        let contradiction = (List.length currIntersects) = (List.length intersects)

        if List.isEmpty currIntersects then
            currWire
        else
            match intersects with
                | intersect::_ ->
                    let segIndex = 
                        Map.tryFindKey (fun _ l -> l = intersect.line) segMap
                        |> Option.defaultValue 0
                    let dist = LineMove intersect.intersectType intersect.position intersect.box
                    
                    let newWire = 
                        if contradiction then 
                            let newIntersect = SmartHelpers.listDifference currIntersects intersects
                            moveSegment model currWire.Segments[segIndex] dist
                        else
                             moveSegment model currWire.Segments[segIndex] dist

                    wireRecursive newWire (List.tail intersects)
                | _ -> currWire
    
    if not (List.isEmpty initIntersects) then 
        // (wire, intersects)
        // ||> List.fold wireChange
        wireRecursive wire initIntersects
    else
        autoroute model wire



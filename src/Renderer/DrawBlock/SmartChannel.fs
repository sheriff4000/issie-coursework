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
    (validX && validY)
    
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

// // YOURE WORKING ON SHIT BELOW HERE
// let moveVerticalSegment (segments: List<Segment>) (amount: float) = 
//     let moveLeft (segments: List<Segment>) (amount: float) = 
// //moves middle segment to the left for negative amount and to the right for positive amount
//     let totalAvailableLength = segments[2].Length + segments[4].Length
//     let left_or_right = amount > 0
    


// let moveWire (xCoordinate: float) (wire: Wire) = //moves middle segment of wire so that it is either the xcoordinate or as close as possible to it
//     let minPossibleX = wire.StartPos.X + wire.Segments[0].Length
//     let maxPossibleX = minPossibleX + wire.Segments[2].Length + wire.Segments[4].Length
//     match xCoordinate with
//     | xCoordinate when xCoordinate < minPossibleX -> 
        
//     | xCoordinate when xCoordinate > maxPossibleX -> 

// let moveWires (channel: BoundingBox) (ids: list<ConnectionId>) (wires: Map<ConnectionId, Wire>) = 
//     // returns new wires array in which wires are moved to try and be evenly spaced.

// //YOURE WORKING ON SHIT ABOVE HERE


    //let spaceVerticalWires (wires: Wire list) (leftSide: float) (width: float) = 
    
    // let numberOfWires = float (List.length wires)
    // let wirePercentageLocations = [1.0..numberOfWires] |> List.map (fun x ->  (100.0 / (numberOfWires + 1.0 )) * x)
    // let setWireHorizontalLength (leftSegmentLengthPercent: float) (wire: Wire) =  
    //     let segments = wire.Segments
    //     let totalHorizontalLength = segments[2].Length + segments[4].Length
    //     let newLeftSegmentLength = totalHorizontalLength * leftSegmentLengthPercent * 0.01
    //     let newRightSegmentLength = totalHorizontalLength - newLeftSegmentLength
    //     let newSegments = segments |> List.mapi (fun i x -> if i = 2 then {x with Length=newLeftSegmentLength} else if i = 4 then {x with Length=newRightSegmentLength} else x) 
    //     {wire with Segments = newSegments}
    // let partials = wirePercentageLocations |> List.map (fun x -> setWireHorizontalLength x )
    // wires |> List.mapi (fun i x -> partials[i] x)



///
/// HLP23: suggested initial smartChannel top-level function
/// to be tested, it must be given a channel in through which to route wires nicely
/// Normally the channel will come from symbol edges.
/// The function must identify all wires with segments going through the channel and space them
/// This function routes the middle segment of all 7 segment wires by moving them perpendicular to its direction.
/// It is expected that all wires provided are 7 segment with middle segment in the same direction
/// wires not so will be ignored.
/// The messiness of when to call it, what to do with different types of wire, which wires to call it with
/// could be left till later.
/// For this simple routing only one dimension of the channel is needed (determined by orientation).
/// The Wires going through the channel must be returned as an updated Wires map in model.
// HLP23: need to update XML doc comments when this function is fully worked out.


let smartChannelRoute //spaces wires evenly 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =
    //steps:
    //1. check which wires are within the box
        //functions needed: checkWireInChannel: checks if the vertical segment of wire goes through the channel
                            //getWiresInChannel: returns all the wires in the Channel
    //2. find order in which to have the wires, based on min and max place the wires can be
        //functions needed: orderWires: orders the wires (based on where their vertical segment will be, left to right)
    //3. try to space the wires evenly
        //functions needed: wireSpacer: decides where the vertical segment of each wire will be
        //                  moveWires: actually moves each wire so that it's spaced (moves as close as possible if not possible)
    //4. if can't: maximimse the minimum distance and get close to even spacing
        //functions needed: also wireSpacer and wireMover
    //5. extend to also do horizontal (in an elegant way)
    //6. extend to deal with hard cases
    let tr = channel.TopLeft
   
    //printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"
    let wiresInChannel = getWiresInChannel model.Wires channel

    print "number of wires in general: "
    print (List.length (Map.toList model.Wires))
    print "number of wires in channel:"
    print (List.length wiresInChannel)
    model
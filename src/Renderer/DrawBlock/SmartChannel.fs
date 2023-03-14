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

let getSidesOfChannel (channel: BoundingBox) = 
    //takes a bounding box and returns a BoxSides record 
    let right = channel.TopLeft.X + channel.W
    let top = channel.TopLeft.Y
    let left = channel.TopLeft.X
    let bottom = top + channel.H
    {
        Left = left; 
        Top = top; 
        Bottom = bottom; 
        Right = right
    }

let getWiresInChannel (model: Model) (channel: BoundingBox) : List<ConnectionId> = 
    let checkWireInChannel (channel: BoundingBox) (wire: Wire)  = // checks if a wire is in a channel (returns a bool)
        
        let boxSides = getSidesOfChannel channel
        //check whether the x values are valid
        let wireX = getXOfVerticalSegmentWire wire
        let validX = (wireX > boxSides.Left) && (wireX < boxSides.Right)
        //check whether the y values are valid
        let wireTopY, wireBottomY = getYOfVerticalSegmentWire wire
        let validY = (wireTopY < boxSides.Bottom) && (wireBottomY > boxSides.Top)
        //check if the whole wire segment is valid
        (validX && validY && ((List.length wire.Segments) = 7 ))

let getStartSegment (model: Model) (channel: BoundingBox) (wire: ConnectionId): int = 
    //TO BE IMPLEMENTED BY SHERIF
    //returns the start horizontal segment
    failwithf("not implemented yet")

let getEndSegment (model: Model) (channel: BoundingBox) (wire: ConnectionId): int = 
    //TO BE IMPLEMENTED BY SHERIF
    //returns the start horizontal segment
    failwithf("not implemented yet")

let straightenWire (model: Model) (wire: ConnectionId) (startSegment: int) (endSegment: int) : Wire = 
    //TO BE IMPLEMENTED BY SHERIF
    failwithf("not implemented yet")


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
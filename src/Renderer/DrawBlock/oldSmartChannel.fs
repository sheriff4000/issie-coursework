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


// ----------------------------  THESE FUNCTIONS ARE TO BE USED FOR WIRES WITH 7 SEGMENTS --------------------------- 
//                               THEY ACT AS HELPERS   
let getXOfVerticalSegmentWire (wire: Wire) = 
    wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length

let getYOfVerticalSegmentWire (wire: Wire) = 
    
    wire.StartPos.Y, (wire.StartPos.Y + wire.Segments[3].Length)

let getXOfLeftHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.X + wire.Segments[0].Length, wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length

let getYOfLeftHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.Y + wire.Segments[1].Length

let getXOfRightHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length, wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length + wire.Segments[4].Length

let getYOfRightHorizontalSegmentWire (wire: Wire) = 
    wire.StartPos.Y + wire.Segments[1].Length + wire.Segments[3].Length
//  ----------------------------------------------------------------------------------------------------------------------------



// ----------------------------  THESE FUNCTIONS ARE TO ACTUALLY MOVE WIRES SO THAT THEY ARE EQUALLY SPACED IN A CHANNEL --------------------------- 
let moveWireVerticalSegment (xCoordinate: float) (wire: Wire) = 
    
    let moveVerticalSegment (segments: List<Segment>) (amount: float) (direction: MovementDirection) = 
        //moves middle vertical segment of a 7 segment wire. Moves to the left when amount is negative, and to the right when amount is positive.
        let totalAvailableLength = segments[2].Length + segments[4].Length
        let possibleRightLength = max 0.0 (segments[4].Length - amount)
        let possibleLeftLength = max 0.0 (segments[2].Length - amount)
        match direction with
        | Left -> segments |> List.mapi (fun i x -> if i = 2 then {x with Length=possibleLeftLength} else if i = 4 then {x with Length=totalAvailableLength - possibleLeftLength} else x)
        | Right -> segments |> List.mapi (fun i x -> if i = 2 then {x with Length=totalAvailableLength - possibleRightLength} else if i = 4 then {x with Length=possibleRightLength} else x)
   
   //moves middle vertical segment of wire so that it is at either the xcoordinate given or as close as possible to it if impossible
    (xCoordinate - (getXOfVerticalSegmentWire wire))
    |> function
        | x when x < 0 -> {wire with Segments = moveVerticalSegment wire.Segments (-x) Left}
        | x when x > 0 -> {wire with Segments = moveVerticalSegment wire.Segments x Right}
        | _ -> wire




let moveWireHorizontalSegment (yCoordinate: float) (wire: Wire) = 
    let moveHorizontalSegment (segments: List<Segment>) (amount: float)= 
        segments
        |> List.mapi (fun i x -> if i = 3 then {x with Length=x.Length+amount} else if i = 5 then {x with Length=x.Length - amount} else x )

    let amount = yCoordinate - (wire.StartPos.Y + wire.Segments[3].Length)
    {wire with Segments = moveHorizontalSegment wire.Segments amount}

//------------------------------------------------------------------------------------------------------------------------


//------------------------------ THESE FUNCTIONS ARE USED TO FIND WHICH WIRES ACTUALLY RUN THROUGH A CHANNEL -----------------------
let checkHorizontalWireInChannel (channel: BoundingBox) (wire: Wire) =
    
    //checking whether the left horizontal segment of the wire lies within the channel
    let boxSides = getSidesOfChannel channel
    let leftSegmentY = getYOfLeftHorizontalSegmentWire wire
    let leftValidY = (leftSegmentY > boxSides.Top) && (leftSegmentY < boxSides.Bottom)
    let leftSegmentLeft, leftSegmentRight = getXOfLeftHorizontalSegmentWire wire
    let leftValidX = (leftSegmentLeft < boxSides.Right) && (leftSegmentRight > boxSides.Left)
    let leftSegmentValid = leftValidX && leftValidY

    //checking whether the right horizontal segment of the wire lies within the channel
    let rightSegmentY = getYOfRightHorizontalSegmentWire wire
    let rightValidY = (rightSegmentY > boxSides.Top) && (rightSegmentY < boxSides.Bottom)
    let rightSegmentLeft, rightSegmentRight = getXOfRightHorizontalSegmentWire wire
    let rightValidX = (rightSegmentLeft < boxSides.Right) && (rightSegmentRight > boxSides.Left)
  
    let rightSegmentValid = rightValidX && rightValidY 

    //returning true if either of the horizontal segments are within the channel
    (leftSegmentValid || rightSegmentValid) && ((List.length wire.Segments) = 7 )

let checkVerticalWireInChannel (channel: BoundingBox) (wire: Wire)  = // checks if a wire is in a channel (returns a bool)
    //define the leftmost, topmost, rightmost, and downmost of the bounding box
    let boxSides = getSidesOfChannel channel
    //check whether the x values are valid
    let wireX = getXOfVerticalSegmentWire wire
    let validX = (wireX > boxSides.Left) && (wireX < boxSides.Right)
    //check whether the y values are valid
    let wireTopY, wireBottomY = getYOfVerticalSegmentWire wire
    let validY = (wireTopY < boxSides.Bottom) && (wireBottomY > boxSides.Top)
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

//---------------------------------------------------------------------------------------------------------------------------------------------------    


//--------- DEALS WITH STRAIGHTENING WIRES IN HORIZONTAL CHANNELS SO THAT ONLY ONE HORIZONTAL SEGMENT IN THE WIRE RUNS THROUGH THE CHANNEL -----
let checkWireNeedsStraightening (channel: BoundingBox) (modelWires: Map<ConnectionId, Wire>) (id: ConnectionId)  = 
    let wire = Map.find id modelWires
    let boxSides = getSidesOfChannel channel
    let verticalX = getXOfVerticalSegmentWire wire
    let verticalTopY, verticalBottomY = getYOfVerticalSegmentWire wire
    (verticalX > boxSides.Left) && (verticalX < boxSides.Right) && (verticalTopY > boxSides.Top) && (verticalBottomY < boxSides.Bottom) && (wire.Segments[3].Length <> 0)

let straightenHorizontalWire (modelWires: Map<ConnectionId, Wire>) (id: ConnectionId) = 
    let wire = Map.find id modelWires
    let newWire = moveWireVerticalSegment (wire.StartPos.X) wire
    Map.add id newWire modelWires
    
let straightenHorizontalWires (channel: BoundingBox) (channelOrientation: Orientation) (model: Model) (ids: List<ConnectionId>) = 
    match channelOrientation with
        | Vertical -> model
        | Horizontal ->
            let modelWires = model.Wires
            let newModelWires = 
                ids 
                |> List.filter (fun x -> checkWireNeedsStraightening channel model.Wires x)
                |> List.fold (fun state x -> straightenHorizontalWire state x) modelWires
            {model with Wires = newModelWires}

let moveWires (channelOrientation: Orientation) (wires: Map<ConnectionId, Wire>) (wireSpacings: list<float>) (ids: list<ConnectionId>)  = 
    //moves each wire in the channel to its allocated space. (an x coordinate wires in a vertical channel, y for horizontal)
    let updateWire (channelOrientation: Orientation) (wires: Map<ConnectionId, Wire>) (wireSpacing, id)  = 
        //returns the horizontal wire move function for horizontal channels, and vertical for vertical
        let wire = Map.find id wires
        match channelOrientation with
            | Vertical ->
                let updatedWire = moveWireVerticalSegment wireSpacing wire
                wires |> Map.add id updatedWire
            | Horizontal ->
                let updatedWire = moveWireHorizontalSegment wireSpacing wire //finish this at some point you stoopid
                wires |> Map.add id updatedWire
    let updateWireFunction = updateWire channelOrientation
    
    List.zip wireSpacings ids
    |> List.fold (updateWireFunction) wires
    
//---------------------------------------------------------------------------------------------------------------------------------------------


//--------------------------------------------------- DECIDES THE SPACING OF EACH WIRE IN THE CHANNEL -------------------------------------------
let wireSpacer (channelOrientation: Orientation) (channel: BoundingBox) (ids: list<ConnectionId>) = 
    //returns an array of what X coordinate each vertical segment should be
    let numberOfWires = float (List.length ids)
    
    match channelOrientation with
        | Vertical ->
            [1.0..numberOfWires] |> List.map (fun x -> channel.TopLeft.X + ((channel.W / (numberOfWires + 1.0 )) * x))
        | Horizontal ->
            [1.0..numberOfWires] |> List.map (fun x -> channel.TopLeft.Y + ((channel.H / (numberOfWires + 1.0 )) * x))

let getActualWireSpacings (wires: Map<ConnectionId, Wire>) (wireSpacings: List<float>) (sortedIdsInChannel: List<ConnectionId>) =  

    let getActualWireSpacing (wireSpacing: float) (wire: Wire)  = 
        let minimum = wire.StartPos.X + wire.Segments[0].Length
        let maximum = wire.StartPos.X + wire.Segments[0].Length + wire.Segments[2].Length + wire.Segments[4].Length + wire.Segments[6].Length
        if wireSpacing < minimum then
            minimum
        elif wireSpacing > maximum then
            maximum
        else 
            wireSpacing
    
    List.map (getActualWireSpacing) wireSpacings
    |> List.mapi (fun i x -> x (Map.find sortedIdsInChannel[i] wires ))

//---------------------------------------------------------------------------------------------------------------------------------------


//------- orders the wires in a channel, from left to right in vertical channels and top to bottom in horizontal channels ---------------------
let orderWires (channelOrientation: Orientation) (modelWires: Map<ConnectionId, Wire>) (ids: List<ConnectionId>) = //takes list of id's of wires in the channel and sorts them left to right
    match channelOrientation with
        | Vertical ->
            let getWireHorizontalLength (wire: Wire) = //THIS CAN BE A HELPER FUNCTIOn
                wire.Segments[0].Length + wire.Segments[2].Length + wire.Segments[4].Length + wire.Segments[6].Length

            let getHorizontalMidpoint (wire: Wire) = //THIS CAN BE A HELPER FUNCTION
                wire.StartPos.X + ((getWireHorizontalLength wire) / 2.0)
            ids
            
            |> List.sortBy (fun id -> (Map.find id modelWires) |> getWireHorizontalLength)
            |> List.sortBy (fun id -> (Map.find id modelWires) |> getHorizontalMidpoint ) 
        
        | Horizontal ->
            let getFinalHeight (wire: Wire) = 
                wire.StartPos.Y + wire.Segments[3].Length
            ids
            |> List.sortBy (fun id -> (Map.find id modelWires) |> getFinalHeight ) 


let checkWiresSufficientlySpaced (channel: BoundingBox) (actualWireSpacings: list<float>) : Option<List<int*int>> =
    if List.length actualWireSpacings < 2 then
        None
    else
        let spacePerWire = channel.W / float((List.length actualWireSpacings) + 1)
        let pairedIndexes = List.pairwise [0..(List.length actualWireSpacings)-1]
        let wiresTooClose = 
            List.pairwise actualWireSpacings
            |> List.zip pairedIndexes
            |> List.filter (fun ((i, j),(x, y)) -> (y - x) < (spacePerWire / 2.0))
        if List.length wiresTooClose > 0 then
            Some (wiresTooClose 
                |> List.map (fun (x,y) -> x))
        else
            None

let getMinimumSpacing (actualWireSpacings: list<float>) = 
    actualWireSpacings
    |> List.pairwise
    |> List.map (fun (x,y) -> y-x)
    |> List.min


let improveWireOrder (channel: BoundingBox) (wireSpacings: list<float>) (wires: Map<ConnectionId, Wire>) (sortedIdsInChannel: list<ConnectionId>) = 
    
    let rec wireSwapper (sortedIdsInChannel: list<ConnectionId>)(wiresAlreadyDone: Set<ConnectionId*ConnectionId>) (wiresToSwap: list<ConnectionId * ConnectionId>) = 
        let actualWireSpacings = getActualWireSpacings wires wireSpacings sortedIdsInChannel
        if List.length wiresToSwap > 0 then
            let (hd,tl) = 
                match wiresToSwap with 
                    | hd::tl -> (hd,tl)
                    | [] -> failwithf "shouldn't ever happen"
        
            let (leftId, rightId) = hd
            let oldMinimum = getMinimumSpacing actualWireSpacings
            let leftIndex = List.findIndex (fun x -> x = leftId) sortedIdsInChannel
            let rightIndex = List.findIndex (fun x -> x = rightId) sortedIdsInChannel
            let newSortedIdsInChannel = 
                sortedIdsInChannel 
                |> List.mapi (fun i id -> if i = leftIndex then rightId elif i = rightIndex then leftId else id)
            let possibleNewWireSpacings = getActualWireSpacings wires wireSpacings newSortedIdsInChannel
            let newMinimum = getMinimumSpacing possibleNewWireSpacings
            if newMinimum > oldMinimum then
                wireSwapper newSortedIdsInChannel (wiresAlreadyDone.Add((leftId, rightId)).Add((rightId, leftId))) tl 
            else 
                wireSwapper sortedIdsInChannel (wiresAlreadyDone.Add(leftId, rightId).Add(rightId, leftId)) tl 

        else
            let wiresToSwap = checkWiresSufficientlySpaced channel actualWireSpacings
            match wiresToSwap with
                | None -> 
                    sortedIdsInChannel
                | Some x -> 
                    let actualWiresToSwap = 
                        x
                        |> List.map (fun (x,y) -> (sortedIdsInChannel[x], sortedIdsInChannel[y]))
                        |> List.filter (fun (x,y) -> not(Set.contains (x,y) wiresAlreadyDone ));
                    if List.length actualWiresToSwap < 1 then
                        sortedIdsInChannel
                    else
                        wireSwapper sortedIdsInChannel wiresAlreadyDone actualWiresToSwap
           
    wireSwapper sortedIdsInChannel Set.empty [] 
// ------------------------------------------------------------------------------------------------------


 




let smartChannelRoute //spaces wires evenly 
        (channelOrientation: Orientation) 
        (channel: BoundingBox) 
        (model:Model) 
            :Model =

    //gets a list of all the id's of wires running through the channel and sorts them in order of where they will be in the channel
    //(for vertical channels the order is left to right, for horizontal channels the order is top to bottom)  
    let sortedWireIdsInChannel = 
        getWiresInChannel channelOrientation model.Wires channel
        |> orderWires channelOrientation model.Wires

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
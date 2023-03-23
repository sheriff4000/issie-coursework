# Implemented Functionality

The following features are implemented:

## Autoroute
* Wires can avoid a component it crosses through, as long as there is room for the wire segment to move.
* Autorouted segments remain in place until made redundant by moving component.
* Segments with an adjacent fixed segment cannot be moved

## SmartResize

* Checks if the two selected components are custom components and are aligned adjacently
* Resized the second symbol selected to match the same port distances along the adjacent edge to the other symbol
* Moves the second symbol selected so that one of the wires is straight
* Moves the second symbol selected's label so be in the same position as it previously was in
* This can be used after SmartPortReorder to have all wires straight as the ports are in the correct order

## TestSmartChannel button
* When two comnponents are selected, it evenly spaces out the wires that travel between ghe two component
* When there are not two components selected, it identifies all the channels in the sheet to then space out the wires. It also checks for wires close to components, spacing them out (differentiates wires that have been autorouted similarly around a component)
* Can channel wires of any complexity

## SmartPortOrder

* Reorders 2 input non-custom components if they have crossing wires: MUX2, DEMUX2, ADD, OR, XOR, NAND, NOR, XNOR
* Reorders ports and port edge based on relative symbol position of symbolToOrder compared to otherSymbol
* Reorders ports with wires connected to additional symbols not selected, based on the additional symbol's poition
* Reorders ports for vertical and horizontal channels, including mixed inputs and outputs with most port arrangements
* Reorders adjacent connections so that ports are opposite for smart resizing

## TestAll button
* Implements SmartPortReorder, SmartResize and SmartChannel all at once
* It will lead to two selected symbols having straight connected wires with ports aligned and any wires in between the symbols will be spaced evenly

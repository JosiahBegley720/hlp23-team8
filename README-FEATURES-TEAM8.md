Team 8
# Implemented functionality
The following features are implemented:

## SmartChannel
* feature - Spaces 7-segment wires inside a specified channel (specified by selecting two components)
* feature - Works for horizontal and vertical channels, including auto and manually routed wires 
	* only routes horizontal and vertical wires in their respective channels to avoid unintended spacings
* feature - Called by selecting "SmartChannel" in the edit menu
* feature - Minimises overlaps where possible to improve visibility

## SmartPortReorder - Josiah Begley
* feature - All connections between components are now instantly sorted with one pass of Bi-directional sorting
* feature - Added support to reorder ports between custom components of different dimensions
* feature - Added support for all logic components
* feature - Custom components have ports automatically reordered depending on inputs and output connections
* feature - Simple omponents are flipped depending on catergorical heuristic:
	* heuristic 1: Strickly increasing port numbers
	* heuristic 2: If flipping a components will not result in removing all wire crossing, flipping is conducted instead to minimise wire crossing based on a penalty heuristic
* feature - After symbol adjustments, wires are updated using smartAutoRoute

## SmartRendering - Hannah Shewan
* All gates viewed in IEEE form
* User can input up to 4 bits for all logic gates, except NOT

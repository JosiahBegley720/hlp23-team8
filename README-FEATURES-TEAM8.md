Team 8
# Implemented functionality
The following features are implemented:

## SmartChannel - Harshil Shah
* feature - Spaces 7-segment wires inside a specified channel (specified by selecting two components)
* feature - Works for horizontal and vertical channels, including auto and manually routed wires 
	* only routes horizontal and vertical wires in their respective channels to avoid unintended spacings
* feature - Called by selecting "SmartChannel" in the edit menu
* feature - Minimises overlaps between similarly oriented wires where possible
	* Works for wires created in all directions (e.g. top to bottom and bottom to top, and left to right, and right to left)

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
* User can input up to 4 bits for all logic gates, except NOT. Port repositioning partially works.

## SmartAutoroute - Rahimi Ridzal
* feature - Implement solution for route round symbol problem
* feature - Implement solution for hugging parallel segments from the same output port
			Limitation: - there exists no solution result in the algorithm used thus it is limited to 5 attempts 
						- segment will be default after 5 attempts without best solution
						- to avoid infinite loop problem
* feature - Implement best path for long signals. If clean segment (segment with no intersection with Bounding Boxes) is not possible, the closest possible solution is returned
			Limitation: - limit is introduced to stop the function finding for optimal solution for cases where there is no solution


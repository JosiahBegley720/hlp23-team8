Changes Made 

1. Smart Rendering

All logic gates have an int Option as an input. This affects every file that matches with symbolType. If there are any bugs after merging, it is probably this!

[CatalogueView.fs](./src/Renderer/UI/CatalogueView.fs) contains the code which allows the user to input how many ports they'd like for each gate

[Update.fs](./src/Renderer/UI/Update.fs), [Renderer.fs](./src/Renderer/Renderer.fs), [ModelType.fs](./src/Renderer/UI/ModelType.fs), [MainView.fs](./src/Renderer/UI/MainView.fs) contain the code that stores the SymbolType (IEEE form or the newer 'box' format). This is a new type
and is processed as userdata. type of userdata.

[SymbolView.fs](./src/Renderer/DrawBlock/SymbolView.fs) [Symbol.fs](./src/Renderer/DrawBlock/Symbol.fs) and [DrawModelHelpers.fs](./src/Renderer/Common/DrawHelpers.fs) have the code to render the n bit input logic gates

Added an extra input parameter to AddPortPosToRender to allow us to offset the port positioning for IEEE gates. This is part of [Symbol.fs](./src/Renderer/DrawBlock/Symbol.fs)

2. SmartChannel

getChannel replaced with getOrientedChannel

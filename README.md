# ![fap](https://raw.githubusercontent.com/aornota/fap/master/src/assets/resources/fap-multi.png) | fap (η)

F# audio player using [Avalonia.FuncUI](https://github.com/fsprojects/Avalonia.FuncUI).

Adapted from [Angel Daniel Munoz Gonzalez's example](https://github.com/fsprojects/Avalonia.FuncUI/tree/master/src/Examples/Elmish%20Examples/Examples.Elmish.MusicPlayer) (see also [their original blog post](https://dev.to/tunaxor/cross-platform-music-player-with-avalonia-f-1igi)).

"Simulation" visuals adapted from [my _nebulous_ repository](https://github.com/aornota/nebulous).

#### Development prerequisites

- [Microsoft .NET 6.0 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/6.0): I'm currently using 6.0.306

##### Also recommended

- [Microsoft Visual Studio Code](https://code.visualstudio.com/download/) with the following extensions:
    - [C#](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.csharp)
    - [Ionide for F#](https://marketplace.visualstudio.com/items?itemName=ionide.ionide-fsharp)
    - [EditorConfig for VS Code](https://marketplace.visualstudio.com/items?itemName=editorconfig.editorconfig)
    - [Rainbow Brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets)

#### Running / building

- Before first running / building:
    - _dotnet tool restore_
    - _cd src_ then _dotnet restore_
- Run / build:
    - Run (debug): _dotnet run_
    - Build (debug): _dotnet build_
    - Build (release): _dotnet build -c release_

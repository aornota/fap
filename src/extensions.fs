[<AutoOpen>]
module Aornota.Fap.Extensions

open Avalonia
open Avalonia.Controls
open Avalonia.Media.Imaging
open Avalonia.Platform
open System

[<Literal>]
let private ASSETS_IMAGES = "avares://fap/assets/images/"

type Bitmap with

    static member FromImageAsset(name: string) : IBitmap =
        new Bitmap(
            AvaloniaLocator
                .Current
                .GetService<IAssetLoader>()
                .Open(Uri($"{ASSETS_IMAGES}{name}", UriKind.RelativeOrAbsolute))
        )

type Image with

    static member FromImageAsset(name) : Image =
        let img = Image()
        img.Source <- Bitmap.FromImageAsset name
        img

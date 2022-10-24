module Aornota.Fap.Domain

open Aornota.Fap.Literals
open Aornota.Fap.Utilities
open System
open System.IO

type TrackId =
    | TrackId of Guid

    static member Create() = TrackId(Guid.NewGuid())

type TrackData =
    { Id: TrackId
      Folder: string
      Name: string
      Duration: int64<millisecond> option }

let fileExtensions = [ FILE_EXTENSION_FLAC; FILE_EXTENSION_MP3; FILE_EXTENSION_WAV ]

let populateSongs (paths: string array) : TrackData array =
    paths
    |> Array.Parallel.map FileInfo
    |> Array.Parallel.map (fun fi ->
        { Id = TrackId(Guid.NewGuid())
          Folder = fi.DirectoryName
          Name = fi.Name
          Duration = None })

let populateFromDirectory (path: string) : TrackData array =
    let dottedFileExtensions = fileExtensions |> List.map (fun ext -> $".{ext}")

    match String.IsNullOrEmpty path with
    | true -> Array.empty
    | false ->
        (DirectoryInfo path).GetFiles()
        |> Array.filter (fun fi -> dottedFileExtensions |> List.contains fi.Extension)
        |> Array.Parallel.map (fun fi ->
            { Id = TrackId(Guid.NewGuid())
              Folder = fi.DirectoryName
              Name = fi.Name
              Duration = None })

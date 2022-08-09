module Aornota.Fap.Domain

open System
open System.IO

type TrackId = TrackId of Guid

type Track =
    { Id: TrackId
      Path: string
      Name: string }

let fileExtensions = [ "flac"; "mp3"; "wav" ]

let populateSongs (paths: string array) : Track array =
    paths
    |> Array.Parallel.map FileInfo
    |> Array.Parallel.map (fun fi ->
        { Id = TrackId(Guid.NewGuid())
          Path = fi.FullName
          Name = fi.Name })

let populateFromDirectory (path: string) : Track array =
    let dottedFileExtensions = fileExtensions |> List.map (fun ext -> $".{ext}")

    match String.IsNullOrEmpty path with
    | true -> Array.empty
    | false ->
        (DirectoryInfo path).GetFiles()
        |> Array.filter (fun fi -> dottedFileExtensions |> List.contains fi.Extension)
        |> Array.Parallel.map (fun fi ->
            { Id = TrackId(Guid.NewGuid())
              Path = fi.FullName
              Name = fi.Name })

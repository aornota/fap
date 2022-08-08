module Aornota.Fap.Domain

open System
open System.IO

type SongRecord =
    { id: Guid
      name: string
      path: string
      createdAt: DateTime }

let fileExtensions = [ "flac"; "mp3"; "wav" ]

let private dottedFileExtensions = fileExtensions |> List.map (fun ext -> $".{ext}")

let populateSongs (paths: string array) : SongRecord array =
    paths
    |> Array.Parallel.map FileInfo
    |> Array.Parallel.map (fun info -> info.Name, info.FullName)
    |> Array.Parallel.map (fun (name, path) ->
        { id = Guid.NewGuid()
          name = name
          path = path
          createdAt = DateTime.Now })

let populateFromDirectory (path: string) : SongRecord array =
    match String.IsNullOrEmpty path with
    | true -> Array.empty
    | false ->
        let dirinfo = DirectoryInfo path

        dirinfo.GetFiles()
        |> Array.filter (fun info -> dottedFileExtensions |> List.contains info.Extension)
        |> Array.Parallel.map (fun info -> info.Name, info.FullName)
        |> Array.Parallel.map (fun (name, path) ->
            { id = Guid.NewGuid()
              name = name
              path = path
              createdAt = DateTime.Now })

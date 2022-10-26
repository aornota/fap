module Aornota.Fap.Persistence

open Aornota.Fap.Literals
open System
open System.IO
open Thoth.Json.Net

type PersistenceType =
    | Preferences
    | Session
    | Playlist

type ReadError =
    | FileNotFound
    | Other of string

[<Literal>]
let private JSON_SPACE_COUNT = 4

// TODO-NMB: Is this actually needed?..
let private extraCoders =
    Extra.empty
    |> Extra.withDecimal
    |> Extra.withCustom (fun _ -> Encode.nil) (fun _ _ -> Ok())

let private persistenceRoot =
    let folder =
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyMusic), APPLICATION_NAME)

    if not (Directory.Exists(folder)) then
        Directory.CreateDirectory(folder)
    else
        DirectoryInfo(folder)

let private subFolder =
    function
    | Preferences -> None
    | Session -> Some FOLDER_SESSIONS
    | Playlist -> Some FOLDER_SESSIONS

let private folder persistenceType =
    match subFolder persistenceType with
    | Some subFolder ->
        let folder = Path.Combine(persistenceRoot.FullName, subFolder)

        if not (Directory.Exists(folder)) then
            Directory.CreateDirectory(folder)
        else
            DirectoryInfo(folder)
    | None -> persistenceRoot

let fileExtension =
    function
    | Preferences -> FILE_EXTENSION_PREFERENCES
    | Session -> FILE_EXTENSION_SESSION
    | Playlist -> FILE_EXTENSION_PLAYLIST

let write persistenceType name (data: 'a) =
    async {
        let file = Path.Combine((folder persistenceType).FullName, name)

        let json = Encode.Auto.toString<'a> (JSON_SPACE_COUNT, data, extra = extraCoders)

        try
            do! File.WriteAllTextAsync(file, json) |> Async.AwaitTask
            return Ok()
        with exn ->
            return Error $"Persistence.write -> {exn.Message}"
    }

let read<'a> persistenceType name =
    async {
        let file = Path.Combine((folder persistenceType).FullName, name)

        if not (File.Exists(file)) then
            return Error FileNotFound
        else
            try
                let! json = File.ReadAllTextAsync(file) |> Async.AwaitTask

                return
                    match Decode.Auto.fromString<'a> (json, extra = extraCoders) with
                    | Ok data -> Ok data
                    | Error error -> Error(Other error)
            with exn ->
                return Error(Other $"Persistence.read -> {exn.Message}")
    }

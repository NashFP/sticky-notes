open Newtonsoft.Json
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Files
open Suave.Http.RequestErrors
open Suave.Http.Successful
open Suave.Types
open Suave.Web
open System.Collections.Generic
open System.IO

type Note = {
    text: string
    color: string
    x: int
    y: int
    z: int
}

type Request = {
    action: string
    id: int
    doc: Note
}

type Entry = {
    id: int
    doc: Note
}

let notes = Dictionary<int, Entry>()

let mutable currentId = 0
let nextId() =
    currentId <- currentId + 1
    currentId

let processRequest request =
    match request.action with
    | "read_all" ->
        Seq.toList notes.Values
    | "create" ->
        let id = nextId()
        let entry = { id = id; doc = request.doc}
        notes.Add(id, entry)
        [entry]
    | "update" ->
        let entry = notes.[request.id]
        notes.[request.id] <- { entry with doc = request.doc}
        []
    | "delete" ->
        notes.Remove(request.id) |> ignore
        []
    | _ ->
        failwith "Invalid Request"

let mapJson mapping =
    context (fun ctx ->
        ctx.request.["json"]
        |> Option.get
        |> fun s -> JsonConvert.DeserializeObject<_> s
        |> mapping
        |> JsonConvert.SerializeObject
        |> OK)

let contentRoot = Path.GetFullPath("content")

let app =
    choose [
        POST >>= path "/notes" >>= mapJson processRequest
        GET >>= path "/" >>= browseFile contentRoot "index.html"
        GET >>= browse contentRoot
        NOT_FOUND "Not Found"
    ]

startWebServer defaultConfig app

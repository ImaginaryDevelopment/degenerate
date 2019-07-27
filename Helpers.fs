module Helpers
open System
let isValueString = String.IsNullOrWhiteSpace >> not
module Option =
  let ofValueString x =
    if isValueString x then Some x else None
let trim = Option.ofValueString >> Option.map(fun x -> x.Trim()) >> Option.defaultValue null
let trim1 (d:char) =
  function
  | null -> null
  | "" -> ""
  | x -> x.Trim(d)
let delimit (d:string) (x:string seq) =
  String.Join(d,values=Array.ofSeq x)
let replace d r (x:string) =
  x.Replace(oldValue=d,newValue=r)
let camel = Option.ofValueString >> Option.map(fun x -> x.[..0].ToLower() + x.[1..]) >> Option.defaultValue null
let (|ValueString|NonValueString|) =
  function
  | x when String.IsNullOrWhiteSpace x -> NonValueString
  | x -> ValueString x
let valueStringOrFail name =
  function
  | null -> raise <| ArgumentNullException name
  | "" -> raise <| ArgumentException name
  | _ -> ()
let tryAfter d =
  valueStringOrFail "d" d
  function
  | null | "" -> None
  | value ->
    let i = value.IndexOf d
    if i < 0 then None
    else Some value.[i+d.Length..]

let after d =
  valueStringOrFail "d" d
  fun value ->
    valueStringOrFail "value" value
    let i = value.IndexOf d
    value.[i+d.Length..]
let before d =
  valueStringOrFail "d" d
  fun value ->
    valueStringOrFail "value" value
    let i = value.IndexOf d
    value.[0..i - 1]

let (|After|_|) d =
  valueStringOrFail "d" d
  tryAfter d

module Seq =
  let filteri f items =
    items
    |> Seq.mapi(fun i x -> i,x)
    |> Seq.filter f
    |> Seq.map snd
  let exceptIndex i' items =
    items
    |> filteri (fun (i,_) ->
      i <> i'
    )

module FS =
  open System.IO
  let adaptPath =
    function
    | ValueString v ->
      if v.StartsWith "~" then
        let dp = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
        let result = Path.Combine(dp, after "~" v |> trim1 '/' |> trim1 '\\')
        printfn "Adapt result:%s" result
        result
      else v
    | x -> x
  let (|AdaptPath|) = adaptPath

  let (|ParentDir|_|) =
    adaptPath
    >>Option.ofValueString
    >> Option.bind (Path.GetDirectoryName>>Option.ofObj)


  let (|DirExists|_|) =
    adaptPath
    >> function
      | x when Directory.Exists x -> Some x
      | _ -> None
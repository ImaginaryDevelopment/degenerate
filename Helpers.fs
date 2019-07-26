module Helpers
open System
let delimit (d:string) (x:string seq) =
  String.Join(d,values=Array.ofSeq x)
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

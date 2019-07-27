module Degenerate.Domain
open System

open Helpers
open MetaGen

let genProp tt  (prop:Property) valueOpt =
  [
    match tt with
    | Interface _ -> yield "abstract"
    | _ -> ()
    match tt with
    | Interface _ ->
      yield! ["member";prop.Name]
    | Class ->
      if prop.FinalMutability = Immutable then
        yield! ["member";sprintf "__.%s" prop.Name]
      else yield![ "member val";prop.Name]
    | _ -> yield prop.Name
    match valueOpt with
    | None ->
      yield ":"
      yield prop.PropType
    |Some value ->
      yield sprintf "= r.%s" value
    match tt, prop.FinalMutability with
    | Class, Mutable
    | Interface false, Mutable ->
      yield "with get,set"
    | Class, Immutable ->
        ()
    | Interface _, Immutable
    | Interface true, _ ->
      yield "with get"
    | Record, _ -> ()
  ]
  |> delimit " "

let genDeclare tt name =
  let cons =
    match tt with
    |Class -> sprintf "(r:%s)" <| genName Record name
    | _ -> String.Empty
  let naming = genName tt name
  sprintf "type %s%s=" naming cons

// a single record, interface, or class
let genImplProp indent m (prop:Property):string list=
    match m, prop.FinalMutability with
    |Mutable, Mutable ->
      [
        sprintf "member this.%s" prop.Name
        indent 1 <| sprintf "with get()= this.%s" prop.Name
        indent 1 <| sprintf "and set v= this.%s <- v" prop.Name
      ]
    | _, Immutable
    |Immutable,_ ->
      sprintf "member this.%s= this.%s" prop.Name prop.Name
      |> List.singleton

type ToGenerate = {
  Name:string
  Properties:Property list
}
let genInterfaceImpl indent name m props =
  [
      let isImmutable = match m with |Immutable -> true | _ -> false
      yield sprintf "interface %s with" <| genName(Interface isImmutable) name
      let props' = props |> Seq.collect (genImplProp indent m >> List.map (indent 1))
      yield! props'
  ]
let genType (toGen:ToGenerate) tt indent =
  let props = toGen.Properties |> Seq.map (fun prop ->
        match tt with
        | Class ->

          genProp tt prop (Some prop.Name)
        | _ -> genProp tt prop None
      )
  [
    yield sprintf "/// %i Properties" toGen.Properties.Length
    yield genDeclare tt toGen.Name
    match tt with
    | Record -> yield indent 1 "{"
    | _ -> ()
    yield! props |> Seq.map (indent 2)
    match tt with
    | Record -> yield "}"
    | _ -> ()

    // consider inheriting I%sR interface
    // implementing interfaces
    let genI m i = genInterfaceImpl indent toGen.Name m toGen.Properties |> Seq.map(indent i)
    match tt with
    | Record ->
      yield indent 1 "with"
      yield! genI Immutable 2
    | Class ->
      yield! genI Mutable 2
      yield! genI Immutable 2
    | _ -> ()
  ]

let genAll toGen ns indent =
  [
    yield sprintf "namespace %s" ns
    yield String.Empty
    yield! genType toGen (Interface true) indent
    yield! genType toGen (Interface false) indent
    yield String.Empty
    yield! genType toGen Record indent
    yield String.Empty
    // yield sprintf "module %sMeta =" toGen.Name
    // yield! Degenerate.MetaGen.genPropMeta toGen.Properties |> Seq.map (indent 1)
    yield! genModule indent toGen.Name toGen.Properties
    yield String.Empty
    yield! genType toGen Class indent
  ]

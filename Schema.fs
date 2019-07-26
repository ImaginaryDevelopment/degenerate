module Schema
open System
open Helpers
// source opts: reflection, json, gen code(the way we've been doing it)
// focus on DTO:
// record, interfaces, and class
type Mutability =
  |Mutable
  |Immutable
type Property = {
  Name:string
  PropType:string
  // Is mutable at class level? (cascades down)
  FinalMutability:Mutability
}

type TypeType =
  |Class
  |Record
  |Interface of readonly:bool

type GenFile = {
  Namespace: string
  BaseName:string
  Properties: string list
  Interfaces: string list
  Class: string list
}


let genProp tt  (prop:Property) valueOpt =
  [
    match tt with
    | Interface _ -> yield "abstract"
    | _ -> ()
    match tt with
    | Interface _ ->
      yield "member"
    | Class ->
      if prop.FinalMutability = Immutable then
        yield "__.member"
      else yield "member val"
    | _ ->()
    yield prop.Name
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

let genName tt name =
    match tt with
    |Class -> name
    |Interface false -> sprintf "I%sR" name
    |Interface true -> sprintf "I%sRW" name
    |Record -> sprintf "%sRec" name
let genDeclare tt name =
  let cons =
    match tt with
    |Class -> sprintf "(r:%s)" <| genName Record name
    | _ -> String.Empty
  let naming = genName tt name
  sprintf "type %s%s=" naming cons

// let defaultGenFile = {
//   Namespace= null
//   BaseName= null
//   Properties= List.empty
//   Interfaces= List.empty
//   Class=List.empty
// }

// type GenFileBuilder() =
//   member __.Yield (_:'a):GenFile = defaultGenFile value
//   [<CustomOperation("id")>]
//   member __.Namespace(gf, x)= {gf with Namespace = x}
//   [<CustomOperation("basename")>]
//   member __.BaseName(gf, x)= {gf with BaseName= x}
//   [<CustomOperation("prop")>]
//   member __.Prop(gf,(prop,tt))= {gf with Properties=genProp tt prop::gf.Properties}
// a single record, interface, or class
let genImplProp indent m (prop:Property):string list=
    match m with
    |Mutable ->
      [
        sprintf "member this.%s" prop.Name
        indent 1 <| sprintf "with get()= this.%s" prop.Name
        indent 1 <| sprintf "and set v= this.%s <- this.%s" prop.Name prop.Name
      ]
    |Immutable ->
      sprintf "member this.%s= this.%s" prop.Name prop.Name
      |> List.singleton
type ToGenerate = {
  Name:string
  Properties:Property list
}
let genInterfaceImpl indent name m props =
  [
      let isImmutable = match m with |Immutable -> true | _ -> false
      yield sprintf "interface %s with" <| genName(TypeType.Interface isImmutable) name
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
    yield genDeclare tt toGen.Name
    match tt with
    | Record -> yield "{"
    | _ -> ()
    yield! props |> Seq.map (indent 1)
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
      yield! genI Immutable 1
      yield! genI Mutable 1
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
    yield! genType toGen Class indent
  ]
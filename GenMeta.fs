module Degenerate.MetaGen
open System

open Helpers

let genName tt name =
    match tt with
    |Class -> name
    |Interface true -> sprintf "I%sR" name
    |Interface false -> sprintf "I%sRW" name
    |Record -> sprintf "%sRec" name

module Impl =
  let genPropMeta (prop:Property) : string seq =
    upcast [
      sprintf "/// %s" prop.PropType
      sprintf "let %s= \"%s\"" prop.Name prop.Name]
  let genDictEntry (prop:Property) : string seq =
    upcast [
      sprintf "%s, box x.%s" prop.Name prop.Name
    ]
  let genDict indent (props:Property seq) : string seq =
    upcast [
      yield "let toDict x= dict ["
      yield! props |> Seq.collect genDictEntry |> Seq.map (indent 1)
      yield "]"
    ]
  let genPropSet indent name (props:Property seq): string seq =
    upcast [
      yield sprintf "let %s= Set[" name
      yield indent 2 (props |> Seq.map(fun p -> p.Name) |> delimit "; ")
      yield "]"
    ]

  let genPropsR indent (props:Property seq): string seq =
    genPropSet indent "propsR" props
  let genPropsW indent (props:Property seq)=
    props
    |> Seq.filter(fun prop -> match prop.FinalMutability with |Mutable -> true | _ -> false)
    |> genPropSet indent "propsRW"
  let genToRecord indent name (props:Property seq): string seq =
    upcast [
      let iname = genName (Interface true) name
      let camel = camel name
      yield sprintf "let inline toRecord (%s:%s) =" camel iname
      yield indent 1 "{"
      yield! props |> Seq.map(fun prop -> sprintf "%s= %s.%s" prop.Name camel prop.Name |> indent 2)
      yield indent 1 "}"
    ]
  let genSrtp indent name (props:Property seq): string seq=
    upcast[
      let cname= camel name
      yield sprintf "let inline toRecordSrtp(%s:^a):%s=" cname (genName Record name)
      yield indent 1 "{"
      yield! props |> Seq.map(fun p -> sprintf "%s= (^a: (member %s: _) %s)" p.Name p.Name cname |> indent 2)
      yield indent 1 "}"
    ]

open Impl

let genModule indent name props =
  let indents i = Seq.map(indent i)
  [
    yield sprintf "module %sMeta =" name
    yield! props |> Seq.collect genPropMeta |> indents 1

    yield String.Empty
    yield! genDict indent props |> indents 1

    yield String.Empty
    yield! genPropsR indent props |> indents 1
    yield! genPropsW indent props |> indents 1

    yield String.Empty
    yield! genToRecord indent name props |> indents 1

    yield String.Empty
    yield! genSrtp indent name props |> indents 1
  ]



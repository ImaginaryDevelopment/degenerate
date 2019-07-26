
let consume name args =
  args
  |> List.tryFindIndex(fun x -> x = name)
  |> function
    |None -> (None, args)
    |Some i -> (Some args.[i], args |> Seq.exceptIndex i |> List.ofSeq)

let consumepair key args =
  args
  |> List.tryFindIndex(fun x -> x = key)
  |> function
    |Some i when args.Length > i ->
      let rem = args |> Seq.exceptIndex i |> Seq.exceptIndex i |> List.ofSeq
      let v = args.[i+1]
      (Some v,rem)
    | _ -> (None,args)
type GetArgResult = {
  ArgValue:string
  // if it didn't use the next value, pull it back
  UnconsumedOpt : string option
}
// match up -key:value or -key value or - key "value"
let getArgValue (current:string) next =
  match next
  if current.Contains ":" then
    if current.Length > current.IndexOf ":" + 1 then
      {ArgValue=current |> after ":",Some next
  else ()

let consumeAnyPair (key:string) (args:string list) =
  args
  |> List.tryFindIndex(fun x -> x.StartsWith key)
  |> function
    | None -> (None,args)
    | Some i ->
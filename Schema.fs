namespace Degenerate

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
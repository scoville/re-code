// Type

@ocaml.doc("The underlying type of all the encoders.")
type t<'a> = 'a => Js.Json.t

// Primitives

@ocaml.doc("Takes a string value and returns a `Js.Json.t`.")
let string = Js.Json.string

@ocaml.doc("Takes an int value and returns a `Js.Json.t`.")
let int = x => Js.Json.number(Belt.Float.fromInt(x))

@ocaml.doc("Takes a float value and returns a `Js.Json.t`.")
let float_ = Js.Json.number

@ocaml.doc("Takes a bool value and returns a `Js.Json.t`.")
let bool = Js.Json.boolean

@ocaml.doc("Represents the `null` value.")
let null = Js.Json.null

// Collections

@ocaml.doc("Encodes a list with the provided encoder.

_Since this function converts the provided list into an array,
you should probably use the `array` function when possible._")
let list = (encoder, values) => values->Belt.List.toArray->Js.Array2.map(encoder)->Js.Json.array

@ocaml.doc("Encodes an array with the provided encoder.")
let array = (encoder, values) => values->Js.Array2.map(encoder)->Js.Json.array

// Data Structures

@ocaml.doc(
  "Encodes an option type. When the optional value is `None` the json value will be `null`."
)
let maybe = (encoder, value) =>
  switch value {
  | Some(value) => encoder(value)
  | None => Js.Json.null
  }

@ocaml.doc("Encodes a result type. When the result value is `Error(_)` the json value will be `null`.

_Since the error will automatically be discarded, you may want to handle the error first
and use the `maybe` function instead._")
let result = (encoder, value) =>
  switch value {
  | Ok(value) => encoder(value)
  | Error(_) => Js.Json.null
  }

@ocaml.doc("Encodes a date type. Uses the `Js.Date.toISOString` function under the hood.")
let date = date => Js.Json.string(Js.Date.toISOString(date))

// Objects

@ocaml.doc("Encodes an object.

This function, just like the `Js.Dict.fromArray` function will accept an array
of tuple-2 where the first element must be a string, and the second one a `Js.Json.t` value.")
let object = values => Js.Json.object_(Js.Dict.fromArray(values))

@ocaml.doc("Encodes a dynamic `Js.Dict.t` object.")
let dict = (encoder, values) => Js.Json.object_(Js.Dict.map((. value) => encoder(value), values))

@ocaml.doc("Encodes a `Belt.Map.String.t`.")
let stringMap = (encoder, values) =>
  Js.Json.object_(Js.Dict.fromArray(Belt.Map.String.toArray(Belt.Map.String.map(values, encoder))))

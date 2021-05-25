// Type

@ocaml.doc("The underlying type of all the decoders.")
type t<'data, 'error> = Js.Json.t => result<'data, 'error>

// Primitives

@ocaml.doc("Takes a value and always return it, ignores the provided JSON

Example:
```
\"true\"->decodeString(pure(42)) == Ok(42)
\"12\"->decodeString(pure(42)) == Ok(42)
\"invalid json\"->decodeString(pure(42)) == Error(ParseError)
```
")
let pure = (value): t<_, _> => _json => Ok(value)

@ocaml.doc("Makes the decoder fail, ignoring the provided JSON.
This is very convenient when you need to parse variant types for instance.

See `flatMap` for more.

Example:
```
\"true\"->decodeString(throw(\"An error occured\")) == Error(TypeError(\"An error occured\"))
\"invalid json\"->decodeString(throw(\"An error occured\")) == Error(ParseError)
```
")
let throw = (message): t<_, _> => _json => Error(message)

@ocaml.doc("Decodes a JSON boolean into a bool.

Example:
```
\"true\"->decodeString(bool) == Ok(true)
\"false\"->decodeString(bool) == Ok(false)
\"1\"->decodeString(bool) == Error(TypeError(...))
```
")
let bool = json =>
  switch Js.Json.decodeBoolean(json) {
  | Some(bool) => Ok(bool)
  | None =>
    Error(
      `Boolean expected, got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`,
    )
  }

@ocaml.doc("Decodes a JSON integer into an int.

Example:
```
\"1\"->decodeString(int) == Ok(1)
\"1.0\"->decodeString(int) == Ok(1)
\"1.1\"->decodeString(int) == Error(TypeError(...))
\"true\"->decodeString(int) == Error(TypeError(...))
```
")
let int = json =>
  switch Js.Json.decodeNumber(json) {
  | Some(number) if Js.Float.isFinite(number) && Js.Math.floor_float(number) == number =>
    Ok(number->Belt.Int.fromFloat)
  | Some(_) | None =>
    Error(
      `Integer expected, got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`,
    )
  }

@ocaml.doc("Decodes a JSON float into a float.

Example:
```
\"1\"->decodeString(float_) == Ok(1.0)
\"1.0\"->decodeString(float_) == Ok(1.0)
\"1.1\"->decodeString(float_) == Ok(1.1)
\"true\"->decodeString(float_) == Error(TypeError(...))
```
")
let float_ = json =>
  switch Js.Json.decodeNumber(json) {
  | Some(number) => Ok(number)
  | None =>
    Error(
      `Float expected, got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`,
    )
  }

@ocaml.doc("Decodes a JSON string into a string.

Example:
```
\"\\\"foo\\\"\"->decodeString(string) == Ok(\"foo\")
\"1\"->decodeString(string) == Error(TypeError(...))
```
")
let string = json =>
  switch Js.Json.decodeString(json) {
  | Some(str) => Ok(str)
  | None =>
    Error(
      `String expected, got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`,
    )
  }

@ocaml.doc("Returns the specified value if a JSON `null` value is found, returns an error otherwise.

Example:
```
\"null\"->decodeString(null(1)) == Ok(1)
\"null\"->decodeString(null(true)) == Ok(true)
\"1\"->decodeString(null(true)) == Error(TypeError(...))
```
")
let null = (value, json) =>
  switch Js.Json.decodeNull(json) {
  | Some(_) => Ok(value)
  | None =>
    Error(
      `null value expected got ${json
        ->Js.Json.stringifyAny
        ->Belt.Option.getWithDefault("unknown")}`,
    )
  }

// Object Primitives

@ocaml.doc("Will try to access the specified key in a JSON object.
Notice that a JSON object can have more than one attribute.

Check `apply` if you need to decode more than one attribute from the same object,
and the `DecodeExtra.Object.required` and `DecodeExtra.Object.optional` functions
for a shorter version.

Example:
```
\"{ \\\"x\\\": 42 }\"->decodeString(field(\"x\", int)) == Ok(42)
\"{ \\\"x\\\": 42, \\\"y\\\": 43 }\"->decodeString(field(\"x\", int)) == Ok(42)
\"{ \\\"x\\\": true }\"->decodeString(field(\"x\", int)) == Error(TypeError(...))
\"{ \\\"y\\\": 42 }\"->decodeString(field(\"x\", int)) == Error(TypeError(...))
```
")
let field = (key, decoder, json) =>
  switch Js.Json.decodeObject(json) {
  | Some(dict) =>
    switch dict->Js.Dict.get(key) {
    | Some(value) => decoder(value)
    | None => Error(`Object has no attribute ${key}`)
    }
  | None =>
    Error(
      `Object expected, got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`,
    )
  }

@ocaml.doc("Will try to access the specified index in a JSON array.

Example:
```
\"[\\\"foo\\\", \\\"bar\\\", \\\"baz\\\"]\"->decodeString(index(0, string)) == Ok(\"foo\")
\"[\\\"foo\\\", \\\"bar\\\", \\\"baz\\\"]\"->decodeString(index(2, string)) == Ok(\"baz\")
\"[\\\"foo\\\", \\\"bar\\\", \\\"baz\\\"]\"->decodeString(index(3, string)) == Error(TypeError(...))
```
")
let index = (index, decoder, json) =>
  switch Js.Json.decodeArray(json) {
  | Some(values) =>
    switch values->Belt.Array.get(index) {
    | Some(value) => decoder(value)
    | None => Error(`Index ${Belt.Int.toString(index)} out of bound`)
    }
  | None =>
    Error(
      `Array expected, got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`,
    )
  }

// Mapping

@ocaml.doc("Transforms the value of a decoder. Doesn't do anything if the decoder fails.

Example:
```
\"\\\"world\\\"\"->decodeString(string->map(value => `Hello ${value}`)) == Ok(\"Hello world\")
\"true\"->decodeString(string->map(value => `Hello ${value}`)) == Error(TypeError(...))
```
")
let map = (decoder, f, json) =>
  switch decoder(json) {
  | Error(_) as error => error
  | Ok(value) => Ok(f(value))
  }

@ocaml.doc("Transforms the value of a decoder by _replacing_ the decoder.
Especially useful to decode variant types or to validate data.

Doesn't do anything if the decoder fails.

Example:
```
\"10\"->decodeString(
  int->flatMap(value => value > 0 ? pure(\"Positive\") : throw(\"Negative\"))
) == Ok(\"Positive\")

\"-10\"->decodeString(
  int->flatMap(value => value > 0 ? pure(\"Positive\") : throw(\"Negative\"))
) == Error(TypeError(\"Negative\"))
```

A more complex example with a variant:
```
type role = Admin | User

let roleDecoder = string->flatMap(
  value => switch value {
  | \"admin\" => pure(Admin)
  | \"user\" => pure(User)
  | unknownRole => throw(`Invalid role: ${unknownRole}`)
  }
)

\"\\\"admin\\\"\"->decodeString(roleDecoder) == Ok(Admin)
\"\\\"user\\\"\"->decodeString(roleDecoder) == Ok(User)
\"\\\"unknown\\\"\"->decodeString(roleDecoder) == Error(TypeError(\"Invalid role: unknown\"))
```
")
let flatMap = (decoder, f, json) =>
  switch decoder(json) {
  | Error(_) as error => error
  | Ok(value) => f(value, json)
  }

@ocaml.doc("_Applies_ a decoder containing a function to a decoder of value.
Since `apply` can be used on function of arbitrary arity this function, when used with `pure`
is extremely convenient to decode complex objects.

Check `DecodeExtra.Object.required` and `DecodeExtra.Object.optional` for an even shorter syntax.

Example:
```
type user = {age: int, name: string, isLoggedIn: bool}

// Makes a user from the provided age, name, and logged in status
let makeUser = (age, name, isLoggedIn) => {age: age, name: name, isLoggedIn: isLoggedIn}

let userDecoder = pure(makeUser)
->apply(field(\"age\", int))
->apply(field(\"name\", string))
->apply(field(\"isLoggedIn\", bool))

\"{ \\\"age\\\": 20, \\\"name\\\": \\\"foo\\\", \\\"isLoggedIn\\\": false }\"->decodeString(userDecoder) ==
  Ok({age: 20, name: \"foo\", isLoggedIn: false})
```
")
let apply = (decoder1, decoder2, json) =>
  switch (decoder1(json), decoder2(json)) {
  | (Error(_) as error, _) | (_, Error(_) as error) => error
  | (Ok(f), Ok(value)) => Ok(f(value))
  }

// Data Structures

@ocaml.doc("Tries all the decoders, in order, until one succeeds. Returns an error otherwise.

Very useful when dealing with heterogeneous JSON values.

Example:
```
\"[1, \\\"foo\\\", 3]\"->decodeString(
  array(oneOf([string->map(value => #string(value)), int->map(value => #int(value))])),
) == Ok([#int(1), #string(\"foo\"), #int(3)])
```
")
let oneOf = (decoders, json) => decoders->Js.Array2.reduce((acc, decoder) =>
    switch (acc, decoder(json)) {
    | (Ok(_) as ok, _) | (Error(_), Ok(_) as ok) => ok
    | (Error(_) as error, Error(_)) => error
    }
  , Error("oneOf found no matching decoder"))

@ocaml.doc("Decodes a JSON value or _null_.

Example:
```
\"1\"->decodeString(nullable(int)) == Ok(Some(1))
\"null\"->decodeString(nullable(int)) == Ok(None)
\"\\\"foo\\\"\"->decodeString(nullable(int)) == Error(TypeError(...))
```
")
let nullable = decoder => oneOf([null(None), decoder->map(value => Some(value))])

@ocaml.doc("Decodes a JSON value successfully or returns `None`.

Used with the `field` function to decode attributes of objects that can be missing.

See `DecodeExtra.Object.optional` for a shorter alternative.

Example:
```
\"{ \\\"x\\\": 42, \\\"y\\\": 43 }\"->decodeString(maybe(field(\"x\", int))) == Ok(Some(42))
\"{ \\\"y\\\": 43 }\"->decodeString(maybe(field(\"x\", int))) == Ok(None)
\"true\"->decodeString(maybe(field(\"x\", int))) == Ok(None)
```
")
let maybe = decoder => oneOf([decoder->map(value => Some(value)), pure(None)])

@ocaml.doc("Using the `lazy_` decoder allows you to handle mutually recursive types.

Example:
```
type rec comment = {message: string, responses: responses}

and responses = Responses(array<comment>)

let makeComment = (message, responses) => {message: message, responses: responses}

let rec commentDecoder = lazy (
  pure(makeComment)
  ->apply(field(\"message\", string))
  ->apply(field(\"responses\", array(lazy_(commentDecoder))->map(responses => Responses(responses))))
)

// You can now use the decoder
```

_Not using the `lazy_` decoder would result in a `RangeError` in the above example._
")
let lazy_ = (decoder, json) => pure()->flatMap(_ => Lazy.force(decoder), json)

@ocaml.doc("Decodes a JSON array value into an array of value.

Example:
```
\"[1, 2, 3]\"->decodeString(array(int)) == Ok([1, 2, 3])
\"[1, 2, 3]\"->decodeString(array(string)) == Error(TypeError(...))
\"1\"->decodeString(array(int)) == Error(TypeError(...))
```
")
let array = (decoder, json) =>
  switch Js.Json.classify(json) {
  | JSONArray(values) => values->Js.Array2.reduce((acc, value) =>
      switch acc {
      | Error(_) as error => error
      | Ok(values) => decoder(value)->Belt.Result.map(value => values->Js.Array2.concat([value]))
      }
    , Ok([]))
  | _ =>
    Error(`Array expected got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`)
  }

@ocaml.doc("Decodes a JSON array value into a list of value.

_This decoder has an extra cost at runtime as it converts the JSON array to a list.
You should consider using the array decoder instead in general, especially for large arrays._

Example:
```
\"[1, 2, 3]\"->decodeString(list(int)) == Ok(list{1, 2, 3})
\"[1, 2, 3]\"->decodeString(list(string)) == Error(TypeError(...))
\"1\"->decodeString(list(int)) == Error(TypeError(...))
```
")
let list = (decoder, json) =>
  switch Js.Json.classify(json) {
  | JSONArray(values) => values->Js.Array2.reduce((acc, value) =>
      switch acc {
      | Error(_) as error => error
      | Ok(values) => decoder(value)->Belt.Result.map(value => values->Js.Array2.concat([value]))
      }
    , Ok([]))->Belt.Result.map(Belt.List.fromArray)
  | _ =>
    Error(`List expected got ${json->Js.Json.stringifyAny->Belt.Option.getWithDefault("unknown")}`)
  }

// Run Decoders

@ocaml.doc("The errors that can be returned by the decoder runners.

`ParseError` means the provided JSON value (or the computed one) is not valid and cannot be parsed.
`TypeError` is returned when the JSON is valid but not compliant with the provided decoder,
a string message is attached to let you know what went wrong.
")
type error = ParseError | TypeError(string)

@ocaml.doc("Takes a JSON value and a decoder and tries to decode the JSON accordingly.

Returns an `error` when something goes wrong.
")
let decodeJson = (json, decoder: t<_, _>) => {
  switch decoder(json) {
  | Ok(_) as ok => ok
  | Error(error) => Error(TypeError(error))
  }
}

@ocaml.doc("Takes any value and a decoder, stringifies and parses the value as JSON, then tries to decode the JSON accordingly.

Returns an `error` when something goes wrong.

_This function is unsafe, and you should probably not use it to validate ReScript values directly._
")
let decodeAnyUnsafe = (value, decoder) => {
  try value->Js.Json.stringifyAny->Belt.Option.getExn->Js.Json.parseExn->decodeJson(decoder) catch {
  | _ => Error(ParseError)
  }
}

@ocaml.doc("Takes a string value and a decoder, parses the string as JSON, then tries to decode the JSON accordingly.

Returns an `error` when something goes wrong.
")
let decodeString = (str, decoder) => {
  try str->Js.Json.parseExn->decodeJson(decoder) catch {
  | _ => Error(ParseError)
  }
}

module Role = {
  type t = Admin | Client | User

  let decoder = {
    open Decode

    string->flatMap(value =>
      switch value {
      | "admin" => pure(Admin)
      | "client" => pure(Client)
      | "user" => pure(User)
      | role => throw(`Unknown role ${role}`)
      }
    )
  }
}

module User = {
  type t = {
    age: int,
    name: string,
    score: float,
    loggedIn: bool,
    phoneNumber: option<string>,
    address: option<string>,
    hobbies: array<string>,
    hobbies2: list<string>,
    role: Role.t,
    firstHobby: string,
    email: string,
    dateOfBirth: Js.Date.t,
  }

  let make = (
    age,
    name,
    score,
    loggedIn,
    phoneNumber,
    address,
    hobbies,
    hobbies2,
    role,
    firstHobby,
    email,
    dateOfBirth,
  ) => {
    age: age,
    name: name,
    score: score,
    loggedIn: loggedIn,
    phoneNumber: phoneNumber,
    address: address,
    hobbies: hobbies,
    hobbies2: hobbies2,
    role: role,
    firstHobby: firstHobby,
    email: email,
    dateOfBirth: dateOfBirth,
  }

  let decoder = {
    open Decode
    open DecodeExtra

    pure(make)
    ->Object.required("age", int->Int.min(18, ~or="Too young"))
    ->Object.required("name", string->String.required->map(name => `Hello ${name}`))
    ->Object.required("score", float_->Float.positive)
    ->Object.required("loggedIn", bool)
    ->Object.required("phoneNumber", nullable(string))
    ->Object.optional("address", string)
    ->Object.required("hobbies", array(string)->Array.notEmpty)
    ->Object.required("hobbies", list(string)->List.notEmpty)
    ->Object.required("role", Role.decoder)
    ->Object.required("hobbies", index(0, string))
    ->Object.required("email", string->String.email)
    ->Object.required("dateOfBirth", Date.iso)
  }
}

let payload = `{
  "age": 20,
  "name": "Toto",
  "score": 5.2,
  "loggedIn": true,
  "phoneNumber": null,
  "hobbies": ["games", "books"],
  "role": "admin",
  "email": "foo@bar.com",
  "dateOfBirth": "2001-02-15T08:58:23.041Z"
}`

let expectedUser = {
  User.age: 20,
  name: "Hello Toto",
  score: 5.2,
  loggedIn: true,
  phoneNumber: None,
  address: None,
  hobbies: ["games", "books"],
  hobbies2: list{"games", "books"},
  role: Admin,
  firstHobby: "games",
  email: "foo@bar.com",
  dateOfBirth: Js.Date.fromString("2001-02-15T08:58:23.041Z"),
}

assert (payload->Decode.decodeString(User.decoder) == Ok(expectedUser))

open Decode

assert ("true"->decodeString(pure(42)) == Ok(42))
assert ("12"->decodeString(pure(42)) == Ok(42))
assert ("invalid json"->decodeString(pure(42)) == Error(ParseError))

assert ("true"->decodeString(throw("An error occured")) == Error(TypeError("An error occured")))
assert ("invalid json"->decodeString(throw("An error occured")) == Error(ParseError))

assert ("true"->decodeString(bool) == Ok(true))
assert ("false"->decodeString(bool) == Ok(false))
assert ("1"->decodeString(bool) == Error(TypeError("Boolean expected, got 1")))

assert ("1"->decodeString(int) == Ok(1))
assert ("1.0"->decodeString(int) == Ok(1))
assert ("1.1"->decodeString(int) == Error(TypeError("Integer expected, got 1.1")))
assert ("true"->decodeString(int) == Error(TypeError("Integer expected, got true")))

assert ("1"->decodeString(float_) == Ok(1.0))
assert ("1.0"->decodeString(float_) == Ok(1.0))
assert ("1.1"->decodeString(float_) == Ok(1.1))
assert ("true"->decodeString(float_) == Error(TypeError("Float expected, got true")))

assert ("\"foo\""->decodeString(string) == Ok("foo"))
assert ("1"->decodeString(string) == Error(TypeError("String expected, got 1")))

assert ("null"->decodeString(null(1)) == Ok(1))
assert ("null"->decodeString(null(true)) == Ok(true))
assert ("1"->decodeString(null(true)) == Error(TypeError("null value expected got 1")))

assert ("{ \"x\": 42 }"->decodeString(field("x", int)) == Ok(42))
assert ("{ \"x\": 42, \"y\": 43 }"->decodeString(field("x", int)) == Ok(42))
assert (
  "{ \"x\": true }"->decodeString(field("x", int)) == Error(TypeError("Integer expected, got true"))
)
assert (
  "{ \"y\": 42 }"->decodeString(field("x", int)) == Error(TypeError("Object has no attribute x"))
)

assert ("[\"foo\", \"bar\", \"baz\"]"->decodeString(index(0, string)) == Ok("foo"))
assert ("[\"foo\", \"bar\", \"baz\"]"->decodeString(index(2, string)) == Ok("baz"))
assert (
  "[\"foo\", \"bar\", \"baz\"]"->decodeString(index(3, string)) ==
    Error(TypeError("Index 3 out of bound"))
)

assert ("\"world\""->decodeString(string->map(value => `Hello ${value}`)) == Ok("Hello world"))
assert (
  "true"->decodeString(string->map(value => `Hello ${value}`)) ==
    Error(TypeError("String expected, got true"))
)

assert (
  "10"->decodeString(int->flatMap(value => value > 0 ? pure("Positive") : throw("Negative"))) ==
    Ok("Positive")
)

assert (
  "-10"->decodeString(int->flatMap(value => value > 0 ? pure("Positive") : throw("Negative"))) ==
    Error(TypeError("Negative"))
)

type role = Admin | User

let roleDecoder = string->flatMap(value =>
  switch value {
  | "admin" => pure(Admin)
  | "user" => pure(User)
  | unknownRole => throw(`Invalid role: ${unknownRole}`)
  }
)

assert ("\"admin\""->decodeString(roleDecoder) == Ok(Admin))
assert ("\"user\""->decodeString(roleDecoder) == Ok(User))
assert ("\"unknown\""->decodeString(roleDecoder) == Error(TypeError("Invalid role: unknown")))

type user = {age: int, name: string, isLoggedIn: bool}

// Makes a user from the provided age, name, and logged in status
let makeUser = (age, name, isLoggedIn) => {age: age, name: name, isLoggedIn: isLoggedIn}

let userDecoder =
  pure(makeUser)
  ->apply(field("age", int))
  ->apply(field("name", string))
  ->apply(field("isLoggedIn", bool))

assert (
  "{ \"age\": 20, \"name\": \"foo\", \"isLoggedIn\": false }"->decodeString(userDecoder) ==
    Ok({age: 20, name: "foo", isLoggedIn: false})
)

assert (
  "[1, \"foo\", 3]"->decodeString(
    array(oneOf([string->map(value => #string(value)), int->map(value => #int(value))])),
  ) == Ok([#int(1), #string("foo"), #int(3)])
)

assert ("1"->decodeString(nullable(int)) == Ok(Some(1)))
assert ("null"->decodeString(nullable(int)) == Ok(None))
assert (
  "\"foo\""->decodeString(nullable(int)) == Error(TypeError("oneOf found no matching decoder"))
)

assert ("{ \"x\": 42, \"y\": 43 }"->decodeString(maybe(field("x", int))) == Ok(Some(42)))
assert ("{ \"y\": 43 }"->decodeString(maybe(field("x", int))) == Ok(None))
assert ("true"->decodeString(maybe(field("x", int))) == Ok(None))

assert ("[1, 2, 3]"->decodeString(array(int)) == Ok([1, 2, 3]))
assert ("[1, 2, 3]"->decodeString(array(string)) == Error(TypeError("String expected, got 1")))
assert ("1"->decodeString(array(int)) == Error(TypeError("Array expected got 1")))

assert ("[1, 2, 3]"->decodeString(list(int)) == Ok(list{1, 2, 3}))
assert ("[1, 2, 3]"->decodeString(list(string)) == Error(TypeError("String expected, got 1")))
assert ("1"->decodeString(list(int)) == Error(TypeError("List expected got 1")))

type rec comment = {message: string, responses: responses}

and responses = Responses(array<comment>)

let makeComment = (message, responses) => {message: message, responses: responses}

let rec commentDecoder = lazy (
  pure(makeComment)
  ->apply(field("message", string))
  ->apply(field("responses", array(lazy_(commentDecoder))->map(responses => Responses(responses))))
)

assert (
  "{\"message\": \"Hello\", \"responses\": [{\"message\": \"Hello\", \"responses\": [{\"message\": \"Hello\", \"responses\": [{\"message\": \"Hello\", \"responses\": [{\"message\": \"Hello\", \"responses\": [{\"message\": \"Hello\", \"responses\": []}]}]}]}]}]}"
  ->decodeString(Lazy.force(commentDecoder))
  ->Belt.Result.isOk
)

open DecodeExtra

assert (
  "\"2001-02-15T08:58:23.041Z\""->decodeString(Date.iso) ==
    Ok(Js.Date.fromString("2001-02-15T08:58:23.041Z"))
)

assert ("1"->decodeString(int->Int.max(10)) == Ok(1))
assert ("11"->decodeString(int->Int.max(10)) == Error(TypeError("Value should be less than 10")))
assert (
  "11"->decodeString(int->Int.max(10, ~or="Should be less than 10!")) ==
    Error(TypeError("Should be less than 10!"))
)

assert ("11"->decodeString(int->Int.min(10)) == Ok(11))
assert ("9"->decodeString(int->Int.min(10)) == Error(TypeError("Value should be greater than 10")))
assert (
  "9"->decodeString(int->Int.min(10, ~or="Value should be above 10!")) ==
    Error(TypeError("Value should be above 10!"))
)

assert ("1.5"->decodeString(float_->Float.max(10.0)) == Ok(1.5))
assert (
  "10.1"->decodeString(float_->Float.max(10.0)) == Error(TypeError("Value should be less than 10"))
)
assert (
  "10.1"->decodeString(float_->Float.max(10.0, ~or="Should be less than 10!")) ==
    Error(TypeError("Should be less than 10!"))
)

assert ("10.1"->decodeString(float_->Float.min(10.0)) == Ok(10.1))
assert (
  "9.9"->decodeString(float_->Float.min(10.0)) ==
    Error(TypeError("Value should be greater than 10"))
)
assert (
  "9.9"->decodeString(float_->Float.min(10.0, ~or="Value should be above 10!")) ==
    Error(TypeError("Value should be above 10!"))
)

assert ("\"foobar\""->decodeString(string->String.matches(%re("/oo/"))) == Ok("foobar"))

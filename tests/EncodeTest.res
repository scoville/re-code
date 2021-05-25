module User = {
  type t = {
    age: int,
    name: string,
    email: option<string>,
    loggedIn: bool,
    phoneNumber: option<string>,
    hobbies: array<string>,
    dateOfBirth: Js.Date.t,
  }
}

let userEncoder = user => {
  open Encode

  object([
    ("age", int(user.User.age)),
    ("name", string(user.name)),
    ("email", maybe(string, user.email)),
    ("loggedIn", bool(user.loggedIn)),
    ("phoneNumber", maybe(string, user.phoneNumber)),
    ("hobbies", array(string, user.hobbies)),
    ("dateOfBirth", date(user.dateOfBirth)),
  ])
}

let user = {
  User.age: 20,
  name: "Hello Toto",
  email: Some("foo@bar.com"),
  loggedIn: true,
  phoneNumber: None,
  hobbies: ["games", "books"],
  dateOfBirth: Js.Date.fromString("2001-02-15T08:58:23.041Z"),
}

assert (
  Js.Json.stringifyWithSpace(userEncoder(user), 2) == `{
  "age": 20,
  "name": "Hello Toto",
  "email": "foo@bar.com",
  "loggedIn": true,
  "phoneNumber": null,
  "hobbies": [
    "games",
    "books"
  ],
  "dateOfBirth": "2001-02-15T08:58:23.041Z"
}`
)

open Encode

assert (Js.Json.stringify(string("foo")) == `"foo"`)
assert (Js.Json.stringify(int(42)) == "42")
assert (Js.Json.stringify(float_(42.5)) == "42.5")
assert (Js.Json.stringify(bool(false)) == "false")

assert (Js.Json.stringify(list(string, list{"foo", "bar"})) == `["foo","bar"]`)
assert (Js.Json.stringify(array(string, ["foo", "bar"])) == `["foo","bar"]`)

assert (Js.Json.stringify(maybe(string, None)) == "null")
assert (Js.Json.stringify(maybe(string, Some("foo"))) == `"foo"`)
assert (Js.Json.stringify(result(string, Error("Will get discarded"))) == "null")
assert (Js.Json.stringify(result(string, Ok("foo"))) == `"foo"`)

assert (
  Js.Json.stringify(
    date(Js.Date.fromString("2001-02-15T08:58:23.041Z")),
  ) == `"2001-02-15T08:58:23.041Z"`
)

assert (
  Js.Json.stringify(dict(int, Js.Dict.fromArray([("foo", 1), ("bar", 2)]))) == `{"foo":1,"bar":2}`
)

// Order for Maps is not guaranteed
assert (
  Js.Json.stringify(
    stringMap(int, Belt.Map.String.fromArray([("foo", 1), ("bar", 2)])),
  ) == `{"bar":2,"foo":1}`
)

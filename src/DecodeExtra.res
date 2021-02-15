@ocaml.doc("Adds extra decoder to handle JSON Objects")
module Object = {
  let required = (decoder1, key, decoder2) => decoder1->Decode.apply(Decode.field(key, decoder2))

  let optional = (decoder1, key, decoder2) =>
    decoder1->Decode.apply(Decode.maybe(Decode.field(key, decoder2)))
}

@ocaml.doc("Adds extra decoder to handle JSON Integers")
module Int = {
  @ocaml.doc("Only accepts integers below the given value.
Optionally, a custom message can be provided.

Example:
```
\"1\"->decodeString(int->Int.max(10)) == Ok(1)
\"11\"->decodeString(int->Int.max(10)) == Error(TypeError(...))
\"11\"->decodeString(int->Int.max(10, ~or=\"Should be less than 10!\")) == Error(TypeError(\"Should be less than 10!\"))
```
")
  let max = (
    decoder,
    value,
    ~or as error=`Value should be less than ${Belt.Int.toString(value)}`,
  ) => decoder->Decode.flatMap(value' => value' < value ? Decode.pure(value') : Decode.throw(error))

  @ocaml.doc("Only accepts integers above the given value.
Optionally, a custom message can be provided.

Example:
```
\"11\"->decodeString(int->Int.min(10)) == Ok(11)
\"9\"->decodeString(int->Int.min(10)) == Error(TypeError(...))
\"9\"->decodeString(int->Int.min(10, ~or=\"Value should be above 10!\")) == Error(TypeError(\"Value should be above 10!\"))
```
")
  let min = (
    decoder,
    value,
    ~or as error=`Value should be greater than ${Belt.Int.toString(value)}`,
  ) => decoder->Decode.flatMap(value' => value' > value ? Decode.pure(value') : Decode.throw(error))

  @ocaml.doc("Only accepts strictly positive integers.
Optionally, a custom message can be provided.")
  let positive = (decoder, ~or as error="Value should be positive") =>
    decoder->Decode.flatMap(value => value > 0 ? Decode.pure(value) : Decode.throw(error))

  @ocaml.doc("Only accepts strictly negative integers.
Optionally, a custom message can be provided.")
  let negative = (decoder, ~or as error="Value should be negative") =>
    decoder->Decode.flatMap(value => value < 0 ? Decode.pure(value) : Decode.throw(error))
}

@ocaml.doc("Adds extra decoder to handle JSON Floats")
module Float = {
  @ocaml.doc("Only accepts floats below the given value.
Optionally, a custom message can be provided.

Example:
```
\"1.5\"->decodeString(float_->Float.max(10.0)) == Ok(1.5)
\"10.1\"->decodeString(float_->Float.max(10.0)) == Error(TypeError(...))
\"10.1\"->decodeString(float_->Float.max(10.0, ~or=\"Should be less than 10.0!\")) == Error(TypeError(\"Should be less than 10.0!\"))
```
")
  let max = (
    decoder,
    value,
    ~or as error=`Value should be less than ${Belt.Float.toString(value)}`,
  ) => decoder->Decode.flatMap(value' => value' < value ? Decode.pure(value') : Decode.throw(error))

  @ocaml.doc("Only accepts floats above the given value.
Optionally, a custom message can be provided.

Example:
```
\"10.1\"->decodeString(float_->Float.min(10.0)) == Ok(10.1)
\"9.9\"->decodeString(float_->Float.min(10.0)) == Error(TypeError(...))
\"9.9\"->decodeString(float_->Float.min(10.0, ~or=\"Value should be above 10.0!\")) == Error(TypeError(\"Value should be above 10.0!\"))
```
")
  let min = (
    decoder,
    value,
    ~or as error=`Value should be greater than ${Belt.Float.toString(value)}`,
  ) => decoder->Decode.flatMap(value' => value' > value ? Decode.pure(value') : Decode.throw(error))

  @ocaml.doc("Only accepts strictly positive floats.
Optionally, a custom message can be provided.")
  let positive = (decoder, ~or as error="Value should be positive") =>
    decoder->Decode.flatMap(value => value > 0.0 ? Decode.pure(value) : Decode.throw(error))

  @ocaml.doc("Only accepts strictly negative floats.
Optionally, a custom message can be provided.")
  let negative = (decoder, ~or as error="Value should be negative") =>
    decoder->Decode.flatMap(value => value < 0.0 ? Decode.pure(value) : Decode.throw(error))
}

@ocaml.doc("Adds extra decoder to handle JSON Strings")
module String = {
  @ocaml.doc("Only accepts non empty strings.
Optionally, a custom message can be provided.")
  let required = (decoder, ~or as error="String should not be empty") =>
    decoder->Decode.flatMap(value => value == "" ? Decode.throw(error) : Decode.pure(value))

  @ocaml.doc("Only accepts strings with specified length.
Optionally, a custom message can be provided.")
  let length = (
    decoder,
    length,
    ~or as error=`String should have length ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->Js.String2.length == length ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts strings which length is greater than the provided value.
Optionally, a custom message can be provided.")
  let min = (
    decoder,
    length,
    ~or as error=`String should have length greater than ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->Js.String2.length >= length ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts strings which length is less than the provided value.
Optionally, a custom message can be provided.")
  let max = (
    decoder,
    length,
    ~or as error=`String should have length less than ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->Js.String2.length <= length ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts strings that match the provided regex.
Optionally, a custom message can be provided.

Example:
\"\\\"foobar\\\"\"->decodeString(string->String.matches(%re(\"/oo/\"))) == Ok(\"foobar\")
")
  let matches = (decoder, regex, ~or as error=`String should match /${Js.Re.source(regex)}/`) =>
    decoder->Decode.flatMap(value =>
      regex->Js.Re.test_(value) ? Decode.pure(value) : Decode.throw(error)
    )

  %%private(
    // Uses the same regex as Yup https://github.com/jquense/yup/blob/b53e5f23ced4c2df7636203e1300b9938fd33b3a/src/string.ts#L9
    let emailRegex = %re(
      "/^((([a-z]|\\d|[!#\$%&'\\*\\+\\-\\/=\\?\\^_`{\\|}~]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])+(\\.([a-z]|\\d|[!#\\$%&'\\*\\+\\-\\/=\\?\\^_`{\\|}~]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])+)*)|((\\x22)((((\\x20|\\x09)*(\\x0d\\x0a))?(\\x20|\\x09)+)?(([\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x7f]|\\x21|[\\x23-\\x5b]|[\\x5d-\\x7e]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(\\\\([\\x01-\\x09\\x0b\\x0c\\x0d-\\x7f]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF]))))*(((\\x20|\\x09)*(\\x0d\\x0a))?(\\x20|\\x09)+)?(\\x22)))@((([a-z]|\\d|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(([a-z]|\\d|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])*([a-z]|\\d|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])))\\.)+(([a-z]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(([a-z]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])*([a-z]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])))$/i"
    )
  )

  @ocaml.doc("Only accepts email strings.
Optionally, a custom message can be provided.")
  let email = (decoder, ~or as error="String should be an email") =>
    decoder->Decode.flatMap(value =>
      emailRegex->Js.Re.test_(value) ? Decode.pure(value) : Decode.throw(error)
    )

  %%private(
    // Uses the same regex as Yup https://github.com/jquense/yup/blob/b53e5f23ced4c2df7636203e1300b9938fd33b3a/src/string.ts#L11
    let urlRegex = %re(
      "/^((https?|ftp):)?\\/\\/(((([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(%[\\da-f]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:)*@)?(((\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5]))|((([a-z]|\\d|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(([a-z]|\\d|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])*([a-z]|\\d|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])))\\.)+(([a-z]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(([a-z]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])*([a-z]|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])))\\.?)(:\\d*)?)(\\/((([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(%[\\da-f]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)+(\\/(([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(%[\\da-f]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)*)*)?)?(\\?((([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(%[\\da-f]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)|[\\uE000-\\uF8FF]|\\/|\\?)*)?(\\#((([a-z]|\\d|-|\\.|_|~|[\\u00A0-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFEF])|(%[\\da-f]{2})|[!\\$&'\\(\\)\\*\\+,;=]|:|@)|\\/|\\?)*)?$/i"
    )
  )

  @ocaml.doc("Only accepts url strings.
Optionally, a custom message can be provided.")
  let url = (decoder, ~or as error="String should be an url") =>
    decoder->Decode.flatMap(value =>
      urlRegex->Js.Re.test_(value) ? Decode.pure(value) : Decode.throw(error)
    )
}

@ocaml.doc("Adds extra decoder to handle JSON Arrays")
module Array = {
  @ocaml.doc("Only accepts non empty arrays.
Optionally, a custom message can be provided.")
  let notEmpty = (decoder, ~or as error=`Array should not be empty`) =>
    decoder->Decode.flatMap(value =>
      value->Js.Array2.length > 0 ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts arrays with specified length.
Optionally, a custom message can be provided.")
  let length = (
    decoder,
    length,
    ~or as error=`Array should have length ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->Js.Array2.length == length ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts arrays which length is greater than the provided value.
Optionally, a custom message can be provided.")
  let min = (
    decoder,
    length,
    ~or as error=`Array should have length greater than ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->Js.Array2.length >= length ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts arrays which length is less than the provided value.
Optionally, a custom message can be provided.")
  let max = (
    decoder,
    length,
    ~or as error=`Array should have length less than ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->Js.Array2.length <= length ? Decode.pure(value) : Decode.throw(error)
    )
}

@ocaml.doc("Adds extra decoder to handle JSON Arrays as Lists.

_You should probably consider using arrays instead._")
module List = {
  @ocaml.doc("Only accepts non empty lists.
Optionally, a custom message can be provided.")
  let notEmpty = (decoder, ~or as error=`List should not be empty`) =>
    decoder->Decode.flatMap(value =>
      value->List.length > 0 ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts strings with specified length.
Optionally, a custom message can be provided.")
  let length = (
    decoder,
    length,
    ~or as error=`List should have length ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->List.length == length ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts lists which length is greater than the provided value.
Optionally, a custom message can be provided.")
  let min = (
    decoder,
    length,
    ~or as error=`List should have length greater than ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->List.length >= length ? Decode.pure(value) : Decode.throw(error)
    )

  @ocaml.doc("Only accepts lists which length is less than the provided value.
Optionally, a custom message can be provided.")
  let max = (
    decoder,
    length,
    ~or as error=`List should have length less than ${Belt.Int.toString(length)}`,
  ) =>
    decoder->Decode.flatMap(value =>
      value->List.length <= length ? Decode.pure(value) : Decode.throw(error)
    )
}

@ocaml.doc("Adds decoders to handle JSON Dates")
module Date = {
  @ocaml.doc("Automatically parses a JSON string value as a valid date object.

As per the doc, the date should be ISO 8601 formatted:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse

Example:
```
\"\\\"2001-02-15T08:58:23.041Z\\\"\"->decodeString(Date.iso) == Ok(Js.Date.fromString(\"2001-02-15T08:58:23.041Z\"))
```
")
  let iso = Decode.string->Decode.flatMap(value =>
    switch Js.Date.fromString(value) {
    | date if !(date->Js.Date.getTime->Js.Float.isNaN) => Decode.pure(date)
    | exception _ => Decode.throw("Not a valid ISO date")
    | _ => Decode.throw("Not a valid ISO date")
    }
  )
}

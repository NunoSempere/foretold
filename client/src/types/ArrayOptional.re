open Rationale;

let concatSomes = (optionals: Js.Array.t(option('a))) : Js.Array.t('a) =>
  optionals
  |> Js.Array.filter(Option.isSome)
  |> Js.Array.map(Option.toExn("Warning: This should not have happened"));

let concatSome = (optionals: array(option('a))) : array('a) =>
  optionals
  |> Js.Array.filter(Option.isSome)
  |> Js.Array.map(Option.toExn("Warning: This should not have happened"));
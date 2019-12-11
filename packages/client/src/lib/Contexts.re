type pair;

[@bs.get] external provider: pair => ReasonReact.reactClass = "Provider";
[@bs.get] external consumer: pair => ReasonReact.reactClass = "Consumer";

[@bs.module "react"] external createContext: 'a => pair = "";

module MakePair = (Config: {
                     type t;
                     let defaultValue: t;
                   }) => {
  let _pair = createContext(Config.defaultValue);

  module Provider = {
    [@react.component]
    let make = (~value: Config.t, ~children=<Null />) =>
      ReasonReact.wrapJsForReason(
        ~reactClass=provider(_pair),
        ~props={"value": value},
        children,
      )
      |> ReasonReact.element;
  };

  module Consumer = {
    [@react.component]
    let make = (~children: Config.t => ReasonReact.reactElement) =>
      ReasonReact.wrapJsForReason(
        ~reactClass=consumer(_pair),
        ~props=Js.Obj.empty(),
        children,
      )
      |> ReasonReact.element;
  };
};
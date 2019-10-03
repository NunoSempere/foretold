let botDefault = "data:image/png;base64,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";

module Link = {
  let component = ReasonReact.statelessComponent(__MODULE__ ++ " link");
  let make =
    FC__Link.make(
      ~className=
        Css.(
          style([
            display(`inlineBlock),
            color(FC__Settings.textDark),
            hover([color(FC__Settings.darkLink)]),
          ])
        ),
      ~isDisabled=false,
    );
};

module Styles = {
  open Css;
  let small = style([fontSize(`em(0.8)), marginTop(`em(0.1))]);
  let by =
    style([
      marginRight(`em(0.4)),
      color(FC__Settings.accentBlue),
      fontSize(`em(0.8)),
    ]);
  let imageCropper =
    style([
      width(`em(1.)),
      height(`em(1.)),
      marginRight(`em(0.4)),
      marginTop(`em(0.1)),
      overflow(`hidden),
      position(`relative),
      borderRadius(`percent(20.)),
      display(`inlineBlock),
    ]);
  let image =
    style([
      display(`inline),
      margin2(~v=`zero, ~h=`auto),
      height(`auto),
      width(`percent(100.)),
    ]);
};

module Agent = {
  type user = {
    name: string,
    image: option(string),
    onClick: ReactEvent.Mouse.t => unit,
  }
  and bot = {
    name: string,
    image: option(string),
    onClick: ReactEvent.Mouse.t => unit,
    owner: option(t),
  }
  and t =
    | User(user)
    | Bot(bot);

  let onClick = agent =>
    switch (agent) {
    | User(u) => u.onClick
    | Bot(u) => u.onClick
    };

  let name = agent =>
    switch (agent) {
    | User(u) => u.name
    | Bot(u) => u.name
    };

  let image = agent =>
    switch (agent) {
    | User(u) => u.image
    | Bot(u) => u.image
    };

  let owner = agent =>
    switch (agent) {
    | User(_) => None
    | Bot(b) => b.owner
    };

  let makeUser = (~name: string, ~onClick=_ => (), ~image=?, ()): t =>
    User({name, image, onClick});

  let makeBot = (~name: string, ~onClick=_ => (), ~image=?, ~owner=?, ()): t =>
    Bot({name, image, onClick, owner});
};

module SubItem = {
  let component = ReasonReact.statelessComponent(__MODULE__ ++ "-subitem");

  let make = (~agent: Agent.t, _) => {
    ...component,
    render: _self => {
      <Link onClick={Agent.onClick(agent)}>
        <div className=Styles.imageCropper>
          <img
            src={Agent.image(agent) |> Rationale.Option.default(botDefault)}
            className=Styles.image
          />
        </div>
        {Agent.name(agent) |> ReasonReact.string}
      </Link>;
    },
  };
};

let component = ReasonReact.statelessComponent(__MODULE__);
let make = (~agent: Agent.t, _) => {
  ...component,
  render: _self => {
    FC__Base.(
      switch (Agent.owner(agent)) {
      | Some(owner) =>
        <Div flexDirection=`column>
          <Div flex={`num(1.0)}> <SubItem agent /> </Div>
          <Div flex={`num(1.0)} className=Styles.small>
            <span className=Styles.by> {"by " |> ReasonReact.string} </span>
            <SubItem agent=owner />
          </Div>
        </Div>
      | None => <SubItem agent />
      }
    );
  },
};
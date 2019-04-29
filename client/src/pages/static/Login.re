open Utils;

let component = ReasonReact.statelessComponent("Home");

let make = _children => {
  ...component,
  render: _ =>
    <div className=StaticStyles.body>
      <div className=StaticStyles.title> {"Foretold" |> ste} </div>
      <div className=StaticStyles.description>
        <Antd.Button
          _type=`primary
          onClick={_e => Context.Auth.Auth0Client.triggerLoginScreen()}>
          {"Login" |> ste}
        </Antd.Button>
      </div>
    </div>,
};
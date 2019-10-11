open Utils;

let component = ReasonReact.statelessComponent("Login");

let make = _children => {
  ...component,
  render: _ =>
    <div className=StaticStyles.body>
      <div className=StaticStyles.title> {"Foretold" |> ste} </div>
      <div className=StaticStyles.description>
        <Antd.Button
          _type=`primary onClick={_e => Auth0Client.triggerLoginScreen()}>
          {"Login" |> ste}
        </Antd.Button>
        <Antd.Button _type=`primary onClick={_e => Auth0Client.checkSession()}>
          {"Check Session" |> ste}
        </Antd.Button>
      </div>
    </div>,
};

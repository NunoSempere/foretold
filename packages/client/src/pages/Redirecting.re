let component = ReasonReact.statelessComponent("Redirecting");

// @todo: fix this duplicated ligic(#rederectings)
// @todo: use only didMount (or samilar)
let make = (~loggedInUser: option(Types.user), _children) => {
  ...component,
  render: _ =>
    switch (loggedInUser) {
    | Some(userData) =>
      let user = userData;
      let agentId = user.agent |> E.O.fmap((e: Primary.Agent.t) => e.id);
      let name = user.name;

      switch (name, agentId) {
      | ("", _) => Routing.Url.push(Profile)
      | (_, Some(id)) =>
        Routing.Url.push(Agent({agentId: id, subPage: AgentMeasurements}))
      | _ => ()
      };

      <>
        {"Redirecting..." |> Utils.ste |> E.React.inH1}
        {"If you are not redirected shortly, try refreshing the page or contacting Ozzie."
         |> Utils.ste
         |> E.React.inP}
      </>;
    | _ =>
      Routing.Url.push(Home);
      <div />;
    },
};
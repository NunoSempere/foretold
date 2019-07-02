/* TODO: Finish me */
open Utils;

module Component = {
  let name = (page: Context.Routing.AgentPage.SubPage.t) =>
    switch (page) {
    | AgentMeasurements => "Predictions"
    | AgentMeasurables => "Created Questions"
    | AgentBots => "Bots"
    };

  let tab = (agentId, currentPage, selectedPage) => {
    let isActive = currentPage == selectedPage;
    <FC.Tab
      isActive
      onClick={C.Link.LinkType.onClick(
        Internal(Agent({agentId, subPage: selectedPage})),
      )}>
      {name(selectedPage) |> ste}
    </FC.Tab>;
  };

  let tabs =
      (
        page: Context.Routing.AgentPage.t,
        agent: Foretold__GraphQL.Queries.Agent.agent,
      ) => {
    let agentId = page.agentId;
    let subPage = page.subPage;

    let agentType =
      switch (agent) {
      | {bot: Some(_)} => `BOT
      | _ => `USER
      };

    agentType == `USER
      ? <>
          {tab(agentId, subPage, AgentMeasurements)}
          {tab(agentId, subPage, AgentMeasurables)}
          {tab(agentId, subPage, AgentBots)}
        </>
      : <>
          {tab(agentId, subPage, AgentMeasurements)}
          {tab(agentId, subPage, AgentMeasurables)}
        </>;
  };
};
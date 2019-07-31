let component = ReasonReact.statelessComponent("AgentCommunities");
let make = (~agentId, ~layout=SLayout.FullPage.makeWithEl, _children) => {
  ...component,
  render: _ => {
    let body =
      <FC.PageCard.Body>
        <ChannelTable.Jsx2 agentId={Some(agentId)} />
      </FC.PageCard.Body>;

    let head =
      <div>
        <FC.Base.Div float=`left>
          <FC.PageCard.HeaderRow.Title>
            {"Agent Communities" |> ReasonReact.string}
          </FC.PageCard.HeaderRow.Title>
        </FC.Base.Div>
      </div>;

    SLayout.LayoutConfig.make(~head, ~body) |> layout;
  },
};
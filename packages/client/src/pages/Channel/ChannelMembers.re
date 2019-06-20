open Utils;
open Foretold__GraphQL;

let ste = ReasonReact.string;

let component = ReasonReact.statelessComponent("ChannelMembers");

let changeRoleAction = (agentId, channelId, role, text) =>
  Foretold__GraphQL.Mutations.ChannelMembershipRoleUpdate.Mutation.make(
    (mutation, _) =>
    <Foretold__Components__Link
      linkType={
        Action(
          _ =>
            Foretold__GraphQL.Mutations.ChannelMembershipRoleUpdate.mutate(
              mutation,
              ~agentId,
              ~channelId,
              ~role,
            ),
        )
      }>
      {text |> ste}
    </Foretold__Components__Link>
  )
  |> E.React.el;

let removeFromChannel = (agentId, channelId) =>
  Foretold__GraphQL.Mutations.ChannelMembershipDelete.Mutation.make(
    (mutation, _) =>
    <Foretold__Components__Link
      linkType={
        Action(
          _ =>
            Foretold__GraphQL.Mutations.ChannelMembershipDelete.mutate(
              mutation,
              agentId,
              channelId,
            ),
        )
      }>
      {"Remove" |> ste}
    </Foretold__Components__Link>
  )
  |> E.React.el;

module Columns = {
  type column = Table.column(Context.Primary.Types.channelMembership);
  let canX = (x, record: Context.Primary.Types.channelMembership) =>
    record.permissions
    |> E.O.fmap(r => Context.Primary.Permissions.canX(x, r))
    |> E.O.default(false);

  let agentColumn: column = {
    name: "Agent" |> ste,
    render: m =>
      m.agent
      |> E.O.fmap((r: Context.Primary.Types.agent) =>
           <Foretold__Components__Link
             linkType={
               Internal(Agent({agentId: r.id, subPage: AgentMeasurements}))
             }>
             {r.name |> E.O.default("Anonymous") |> ste}
           </Foretold__Components__Link>
         )
      |> E.O.React.defaultNull,
    flex: 1,
  };

  let roleColumn: column = {
    name: "Role" |> ste,
    render: m =>
      switch (m.role) {
      | `ADMIN =>
        <div className="ant-tag ant-tag-blue"> {"Admin" |> ste} </div>
      | `VIEWER =>
        <div className="ant-tag ant-tag-green"> {"Viewer" |> ste} </div>
      },
    flex: 1,
  };

  let roleChangeColumn: string => column =
    channelId => {
      name: "Change Role" |> ste,
      render: m =>
        <div>
          {switch (m.role, m.agent) {
           | (`VIEWER, Some(agent)) =>
             E.React.showIf(
               canX(`CHANNEL_MEMBERSHIP_ROLE_UPDATE, m),
               changeRoleAction(
                 agent.id,
                 channelId,
                 `ADMIN,
                 "Change to Admin",
               ),
             )
           | (`ADMIN, Some(agent)) =>
             E.React.showIf(
               canX(`CHANNEL_MEMBERSHIP_ROLE_UPDATE, m),
               changeRoleAction(
                 agent.id,
                 channelId,
                 `VIEWER,
                 "Change to Viewer",
               ),
             )
           | _ => <div />
           }}
        </div>,
      flex: 1,
    };

  let removeFromChannelColumn: string => column =
    channelId => {
      name: "Remove" |> ste,
      render: m =>
        switch (m.agent, canX(`CHANNEL_MEMBERSHIP_DELETE, m)) {
        | (Some(agent), true) => removeFromChannel(agent.id, channelId)
        | _ => ReasonReact.null
        },
      flex: 1,
    };

  let all = channelId => [|
    agentColumn,
    roleColumn,
    roleChangeColumn(channelId),
    removeFromChannelColumn(channelId),
  |];
};

let make =
    (~channelId: string, ~layout=SLayout.FullPage.makeWithEl, _children) => {
  ...component,
  render: _ => {
    let table =
      Queries.ChannelMemberships.component(~id=channelId, memberships =>
        memberships
        |> E.HttpResponse.fmap(memberships =>
             Table.fromColumns(Columns.all(channelId), memberships)
           )
        |> E.HttpResponse.withReactDefaults
      );

    SLayout.LayoutConfig.make(
      ~head=
        <>
          <FC.Base.Div float=`left>
            <FC.PageCard.HeaderRow.Title>
              {"Channel Members" |> ste}
            </FC.PageCard.HeaderRow.Title>
          </FC.Base.Div>
          <FC.Base.Div
            float=`right
            className={Css.style([
              FC.PageCard.HeaderRow.Styles.itemTopPadding,
              FC.PageCard.HeaderRow.Styles.itemBottomPadding,
            ])}>
            <FC.Base.Button
              variant=Primary
              onClick={e =>
                Foretold__Components__Link.LinkType.onClick(
                  Internal(ChannelInvite(channelId)),
                  e,
                )
              }>
              {"Add Members" |> ste}
            </FC.Base.Button>
          </FC.Base.Div>
        </>,
      ~body=<FC.PageCard.Body> table </FC.PageCard.Body>,
    )
    |> layout;
  },
};
open Utils;
open Style.Grid;

module SimpleHeader = {
  module Styles = {
    open Css;
    let header =
      style([
        borderBottom(`px(1), `solid, `hex("eee")),
        paddingLeft(`px(10)),
        paddingBottom(`em(0.8)),
        paddingRight(`em(0.4)),
        paddingTop(`px(10)),
        float(`left),
        width(`percent(100.)),
      ]);
  };

  let buttonr = (str, action: Context.Routing.Url.t) =>
    <Div float=`right>
      <Antd.Button onClick={_ => Context.Routing.Url.push(action)}>
        {str |> ste}
      </Antd.Button>
    </Div>;

  let newMeasurable = channelId =>
    buttonr("New Measurable", MeasurableNew(channelId));

  let editChannel = channelId =>
    buttonr("Edit Measurable", ChannelEdit(channelId));

  let newSeries = channelId => buttonr("New Series", SeriesNew(channelId));

  let inviteToChannel = channelId =>
    buttonr("Invite Members", ChannelInvite(channelId));

  let leaveChannel = (channelId, agentId) =>
    Foretold__GraphQL.Mutations.ChannelLeave.Mutation.make((mutation, _) =>
      <Div float=`right>
        <Antd.Button
          onClick={
            _ =>
              Foretold__GraphQL.Mutations.ChannelLeave.mutate(
                mutation,
                agentId,
                channelId,
              )
          }>
          {"Leave Channel" |> ste}
        </Antd.Button>
      </Div>
    )
    |> E.React.el;

  let joinChannel = (channelId, agentId) =>
    Foretold__GraphQL.Mutations.ChannelJoin.Mutation.make((mutation, _) =>
      <Div float=`right>
        <Antd.Button
          onClick={
            _ =>
              Foretold__GraphQL.Mutations.ChannelJoin.mutate(
                mutation,
                agentId,
                channelId,
              )
          }>
          {"Join Channel" |> ste}
        </Antd.Button>
      </Div>
    )
    |> E.React.el;

  let members = (channel: Context.Primary.Channel.t) =>
    channel.membershipCount
    |> E.O.React.fmapOrNull(c =>
         <Div float=`right>
           <Antd.Button
             onClick={
               _ => Context.Routing.Url.push(ChannelMembers(channel.id))
             }>
             <Icon.Icon icon="PEOPLE" />
             {c |> string_of_int |> ste}
           </Antd.Button>
         </Div>
       );
};
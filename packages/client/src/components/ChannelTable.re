module Columns = {
  type record = Types.channel;
  type column = Table.column(Types.channel);

  let nameDescription =
    Table.Column.make(
      ~name="Name & Description" |> Utils.ste,
      ~render=
        (channel: record) =>
          <div>
            <Link linkType={Internal(ChannelShow(channel.id))}>
              {channel.name |> Utils.ste}
            </Link>
            {channel.description
             |> E.O.React.fmapOrNull(description =>
                  <ReactMarkdown source=description />
                )}
          </div>,
      ~flex=4,
      (),
    );

  let memberCount =
    Table.Column.make(
      ~name="Members" |> Utils.ste,
      ~render=
        (channel: record) =>
          channel.membershipCount
          |> E.O.fmap(string_of_int)
          |> E.O.default("")
          |> Utils.ste,
      ~flex=1,
      (),
    );

  let openedCount =
    Table.Column.make(
      ~name="Open Questions" |> Utils.ste,
      ~render=
        (channel: record) =>
          channel.openedMeasurablesCount
          |> E.O.fmap(string_of_int)
          |> E.O.default("")
          |> Utils.ste,
      ~flex=1,
      (),
    );

  let labels =
    Table.Column.make(
      ~name="Curation" |> Utils.ste,
      ~render=
        (channel: record) => <> <Curated channel /> <Curated channel /> </>,
      ~show=(channel: record) => channel.isCurated || channel.isArchived,
      ~flex=1,
      (),
    );

  let all = [|nameDescription, memberCount, openedCount, labels|];
};

[@react.component]
let make = (~agentId=None, ~isArchived=?) =>
  ChannelsGet.component(
    ~channelMemberId=?agentId,
    ~isArchived?,
    ~sortFn=ChannelsGet.sortCurated,
    channels =>
    Table.fromColumns(Columns.all, channels, ())
  );

module Jsx2 = {
  let make = (~agentId=None, ~isArchived=?, children) =>
    ReasonReactCompat.wrapReactForReasonReact(
      make,
      makeProps(~agentId, ~isArchived?, ()),
      children,
    );
};
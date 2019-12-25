module ReducerConfig = {
  type itemType = Types.agentMeasurable;
  type callFnParams = (
    option(string),
    option(string),
    option(Types.finalComparisonMeasurement),
  );

  let getId = (e: itemType) => e.id;
  let callFn =
      ((channelId, measurableId, finalComparisonMeasurement): callFnParams) =>
    AgentMeasurablesGet.component(
      ~channelId,
      ~measurableId,
      ~finalComparisonMeasurement,
      (),
    );

  let isEqual = (a: itemType, b: itemType) => {
    a.id == b.id;
  };
};

module Reducer = PaginationFunctor.Make(ReducerConfig);

[@react.component]
let make =
    (
      ~channelId=None,
      ~measurableId=None,
      ~head=Leaderboard.head(~subTab=ByMeasurable),
      ~columns=LeaderboardTable.Columns.measurables,
      ~finalComparisonMeasurement=None,
      ~container=`fluid,
    ) => {
  let subComponent = (reducerParams: Reducer.Types.reducerParams) => {
    let items =
      switch (reducerParams.response) {
      | Success(connection) => connection.edges
      | _ => [||]
      };

    let items =
      items |> E.A.fmap(Primary.LeaderboardItem.fromAgentMeasurable);

    let body =
      switch (reducerParams.response) {
      | Success(_) =>
        Array.length(items) > 0
          ? <FC.PageCard.Body>
              <LeaderboardTable items columns />
            </FC.PageCard.Body>
          : <NothingToShow />
      | _ => <Spin />
      };

    let head =
      head(
        ~channelId,
        ~paginationPage=Reducer.Components.paginationPage(reducerParams),
        (),
      );

    <SLayout head container> body </SLayout>;
  };

  <Reducer
    callFnParams=(channelId, measurableId, finalComparisonMeasurement)
    subComponent
  />;
};
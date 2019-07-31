open Utils;

module Query = [%graphql
  {|
    query getChannel($id: String!) {
      channel:
      channel(id: $id){
        id
        name
        description
        isArchived
        isPublic
        membershipCount
        myRole
      }
    }
  |}
];

module QueryComponent = ReasonApollo.CreateQuery(Query);

let toChannel = (channel): Primary.Channel.t =>
  Primary.Channel.make(
    ~id=channel##id,
    ~name=channel##name,
    ~description=channel##description,
    ~isArchived=channel##isArchived,
    ~isPublic=channel##isPublic,
    ~myRole=Some(channel##myRole),
    ~membershipCount=Some(channel##membershipCount),
    (),
  );

let component = (~id, fn) => {
  let query = Query.make(~id, ());
  QueryComponent.make(~variables=query##variables, ({result}) =>
    result
    |> ApolloUtils.apolloResponseToResult
    |> E.R.bind(_, e =>
         switch (e##channel |> E.O.fmap(toChannel)) {
         | Some(r) => Ok(r)
         | None => Error("Community Not Found" |> ste)
         }
       )
    |> E.R.fmap(fn)
    |> E.R.id
  )
  |> E.React.el;
};

let getChannelByIdAsComponent = (~id, innerFn) => {
  let query = Query.make(~id, ());
  QueryComponent.make(~variables=query##variables, ({result}) =>
    result
    |> HttpResponse.fromApollo
    |> HttpResponse.fmap(e => e##channel |> E.O.fmap(toChannel))
    |> HttpResponse.optionalToMissing
    |> innerFn
  )
  |> ReasonReact.element;
};

let component2 = (~id, innerFn) => {
  switch (id) {
  | "" => HttpResponse.Success(Primary.Channel.getGlobalChannel()) |> innerFn
  | _ => getChannelByIdAsComponent(~id, innerFn)
  };
};
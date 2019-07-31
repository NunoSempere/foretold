module EditPreference = [%graphql
  {|
    mutation preferenceUpdate(
        $id: String!
        $input: PreferenceUpdateInput!
    ) {
        preferenceUpdate(
            id: $id
            input: $input
        ) {
            id
        }
    }
 |}
];

module EditPreferenceMutation = ReasonApollo.CreateMutation(EditPreference);

let mutate =
    (
      mutation: EditPreferenceMutation.apolloMutation,
      stopAllEmails: bool,
      id: string,
    ) => {
  let mutate =
    EditPreference.make(
      ~id,
      ~input={"stopAllEmails": Some(stopAllEmails)},
      (),
    );
  mutation(~variables=mutate##variables, ~refetchQueries=[||], ()) |> ignore;
};

let withUserQuery =
    (auth0Id, innerComponentFn: 'a => ReasonReact.reactElement) => {
  let query = UserGet.Query.make(~auth0Id, ());
  UserGet.QueryComponent.make(~variables=query##variables, ({result}) =>
    result
    |> ApolloUtils.apolloResponseToResult
    |> E.R.fmap(innerComponentFn)
    |> E.R.id
  )
  |> E.React.el;
};

let withPreferenceMutation = innerComponentFn =>
  EditPreferenceMutation.make(
    ~onError=e => Js.log2("Graphql Error:", e),
    innerComponentFn,
  )
  |> E.React.el;
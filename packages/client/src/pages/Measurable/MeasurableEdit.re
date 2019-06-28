open Foretold__GraphQL;

module WithEditMutation = {
  module GraphQL = [%graphql
    {|
             mutation measurableUpdate($id: String!, $input: MeasurableUpdateInput!) {
                 measurableUpdate(id: $id, input: $input) {
                   id
                 }
             }
     |}
  ];

  module Mutation = ReasonApollo.CreateMutation(GraphQL);

  let mutate =
      (
        mutation: Mutation.apolloMutation,
        id: string,
        name: string,
        labelCustom: string,
        expectedResolutionDate: string,
        resolutionEndpoint: string,
        labelSubject: string,
        labelOnDate: string,
        showDescriptionDate: string,
        labelProperty: string,
        valueType: string,
      ) => {
    let date = showDescriptionDate == "TRUE" ? labelOnDate : "";
    let m =
      GraphQL.make(
        ~id,
        ~input={
          "name": name,
          "labelCustom": labelCustom |> Rationale.Option.some,
          "labelProperty": labelProperty |> Rationale.Option.some,
          "labelOnDate": date |> Js.Json.string |> Rationale.Option.some,
          "expectedResolutionDate":
            expectedResolutionDate |> Js.Json.string |> Rationale.Option.some,
          "resolutionEndpoint": resolutionEndpoint |> Rationale.Option.some,
          "labelSubject": labelSubject |> Rationale.Option.some,
          "valueType": valueType |> Context.Primary.Measurable.valueTypeToEnum,
        },
        (),
      );

    mutation(~variables=m##variables, ~refetchQueries=[|"getAgent"|], ())
    |> ignore;
  };
};

module CMutationForm =
  MutationForm.Make({
    type queryType = WithEditMutation.GraphQL.t;
  });

let formCreation = (id, m) => {
  let measurable = Queries.Measurable.toMeasurable(m);

  WithEditMutation.Mutation.make((mutation, data) =>
    MeasurableForm.SignUpForm.make(
      ~onSubmit=
        ({values}) =>
          WithEditMutation.mutate(
            mutation,
            id,
            values.name,
            values.labelCustom,
            values.expectedResolutionDate,
            values.resolutionEndpoint,
            values.labelSubject,
            values.labelOnDate,
            values.showDescriptionDate,
            values.labelProperty,
            values.valueType,
          ),
      ~initialState={
        name: measurable.name,
        labelOnDate:
          measurable.labelOnDate
          |> E.O.default(MomentRe.momentNow())
          |> MeasurableForm.formatDate,
        showDescriptionDate:
          measurable.labelOnDate |> E.O.isSome |> (e => e ? "TRUE" : "FALSE"),
        labelSubject: measurable.labelSubject |> E.O.default(""),
        labelCustom: measurable.labelCustom |> E.O.default(""),
        expectedResolutionDate:
          measurable.expectedResolutionDate
          |> E.O.default(MomentRe.momentNow())
          |> MeasurableForm.formatDate,
        resolutionEndpoint: measurable.resolutionEndpoint |> E.O.default(""),
        showDescriptionProperty: measurable.name == "" ? "TRUE" : "FALSE",
        labelProperty: measurable.labelProperty |> E.O.default(""),
        valueType:
          measurable.valueType |> Context.Primary.Measurable.valueTypeToStr,
      },
      ~schema=[(`name, Custom(_ => None))],
      ({handleSubmit, handleChange, form, _}) =>
        CMutationForm.showWithLoading(
          ~result=data.result,
          ~form=MeasurableForm.showForm(~form, ~handleSubmit, ~handleChange),
          ~successMessage="Question updated successfully.",
          (),
        ),
    )
    |> E.React.el
  )
  |> E.React.el;
};

let component = ReasonReact.statelessComponent("MeasurableEdit");

let make =
    (
      ~pageParams: PageConfig.LoggedInPage.pageParams,
      ~layout=SLayout.FullPage.makeWithEl,
      _children,
    ) => {
  ...component,
  render: _self =>
    SLayout.LayoutConfig.make(
      ~head=SLayout.Header.textDiv("Edit Question"),
      ~body=
        <FC.PageCard.BodyPadding>
          {Queries.Measurable.component(~id=pageParams.id, m =>
             formCreation(pageParams.id, m)
           )}
        </FC.PageCard.BodyPadding>,
    )
    |> layout,
};
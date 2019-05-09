open Utils;

type creator = {
  id: string,
  name: option(string),
};
type measurable = {
  id: string,
  name: string,
  valueType: Context.Primary.valueType,
  labelCustom: option(string),
  resolutionEndpoint: option(string),
  measurementCount: option(int),
  measurerCount: option(int),
  createdAt: MomentRe.Moment.t,
  updatedAt: MomentRe.Moment.t,
  expectedResolutionDate: option(MomentRe.Moment.t),
  state: Context.Primary.MeasurableState.t,
  stateUpdatedAt: option(MomentRe.Moment.t),
  creator: option(creator),
  labelSubject: option(string),
  labelOnDate: option(MomentRe.Moment.t),
  labelProperty: option(string),
};

let toMeasurable = (m: measurable): Context.Primary.Measurable.t =>
  Context.Primary.Measurable.make(
    ~id=m.id,
    ~name=m.name,
    ~valueType=m.valueType,
    ~channel=None,
    ~labelCustom=m.labelCustom,
    ~resolutionEndpoint=m.resolutionEndpoint,
    ~measurementCount=m.measurementCount,
    ~measurerCount=m.measurerCount,
    ~createdAt=Some(m.createdAt),
    ~updatedAt=Some(m.updatedAt),
    ~expectedResolutionDate=m.expectedResolutionDate,
    ~state=Some(m.state),
    ~stateUpdatedAt=m.stateUpdatedAt,
    ~labelSubject=m.labelSubject,
    ~labelOnDate=m.labelOnDate,
    ~labelProperty=m.labelProperty,
    (),
  );

module Query = [%graphql
  {|
      query getMeasurable ($id: String!) {
          measurable:
            measurable(id: $id) @bsRecord{
           id
           name
           labelCustom
           resolutionEndpoint
           valueType
           measurementCount
           measurerCount
           labelSubject
           labelProperty
           labelOnDate @bsDecoder(fn: "E.J.O.toMoment")
           state @bsDecoder(fn: "Context.Primary.MeasurableState.fromEnum")
           stateUpdatedAt @bsDecoder(fn: "E.J.O.toMoment")
           expectedResolutionDate @bsDecoder(fn: "E.J.O.toMoment")
           createdAt @bsDecoder(fn: "E.J.toMoment")
           updatedAt @bsDecoder(fn: "E.J.toMoment")
           creator @bsRecord{
             id
             name
           }
          }
      }
    |}
];
module QueryComponent = ReasonApollo.CreateQuery(Query);
let component = (~id, fn) => {
  let query = Query.make(~id, ());
  QueryComponent.make(~variables=query##variables, ({result}) =>
    result
    |> ApolloUtils.apolloResponseToResult
    |> E.R.bind(_, e =>
         e##measurable |> filterOptionalResult("Measurable not found" |> ste)
       )
    |> E.R.fmap(fn)
    |> E.R.id
  )
  |> E.React.el;
};
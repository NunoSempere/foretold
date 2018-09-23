open Rationale.Option.Infix;
open Rationale.Function.Infix;
open MomentRe;

let toMoment = Queries.jsonToString ||> moment;

/* let toOptionalMoment: option(Js.Json.t) => MomentRe.Moment.t = x => x <$> jsonToString |> Option.default("") |> moment */
let toOptionalMoment: option(Js.Json.t) => MomentRe.Moment.t =
  e =>
    (
      switch (e) {
      | Some(f) => f |> Queries.jsonToString
      | None => ""
      }
    )
    |> moment;

type user = {
  id: string,
  name: string,
};

type competitorType = [ | `AGGREGATION | `COMPETITIVE | `OBJECTIVE];
type bot = {
  competitorType,
  description: option(string),
  id: string,
  name: option(string),
};

type measurable = {
  id: string,
  name: string,
  isLocked: bool,
}

type measurement = {
  id: string,
  relevantAt: MomentRe.Moment.t,
  competitorType,
  createdAt: MomentRe.Moment.t,
  taggedMeasurementId: option(string),
  measurable: option(measurable),
};

type agent = {
  user: option(user),
  bot: option(bot),
  measurements: array(option(measurement)),
};

module GetAgent = [%graphql
  {|
    query getAgent ($id: String!) {
        agent:
        agent(id: $id) @bsRecord{
        user: User @bsRecord{
          id
          name
        }
        bot: Bot @bsRecord{
          id
          name
          description
          competitorType
        }
        measurements: Measurements @bsRecord{
            id
            createdAt @bsDecoder(fn: "toMoment")
            relevantAt @bsDecoder(fn: "toOptionalMoment")
            competitorType
            taggedMeasurementId
            measurable: Measurable @bsRecord{
              id
              name
              isLocked
            }
        }
        }
    }
  |}
];

module GetAgentQuery = ReasonApollo.CreateQuery(GetAgent);
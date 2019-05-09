open MomentRe;
let component = ReasonReact.statelessComponent("MeasurableChart");

module Styles = {
  open Css;
  let plot = style([maxWidth(px(800))]);
};

type competitorType = [ | `AGGREGATION | `COMPETITIVE | `OBJECTIVE];

type measurement = {
  createdAt: MomentRe.Moment.t,
  competitorType,
  low: float,
  median: float,
  high: float,
};

let formatDate = E.M.format(E.M.format_standard);
let make = (~measurements: array(measurement), _children) => {
  ...component,
  render: _ => {
    let sorted = measurements;

    let yMax =
      sorted
      |> E.A.fmap(e => e.high)
      |> E.A.fold_left((a, b) => a > b ? a : b, min_float);

    let yMin =
      sorted
      |> E.A.fmap(e => e.low)
      |> E.A.fold_left((a, b) => a < b ? a : b, max_float);

    let xMax =
      sorted
      |> E.A.fmap(e => e.createdAt)
      |> E.A.fold_left(
           (a, b) => Moment.isAfter(a, b) ? a : b,
           "Jan 3, 1970" |> moment,
         )
      |> formatDate
      |> E.JsDate.fromString;

    let xMin =
      sorted
      |> E.A.fmap(e => e.createdAt)
      |> E.A.fold_left(
           (a, b) => Moment.isBefore(a, b) ? a : b,
           "Jan 3, 2070" |> moment,
         )
      |> formatDate
      |> E.JsDate.fromString;

    let aggregatePercentiles =
      sorted
      |> Js.Array.filter(e => e.competitorType == `AGGREGATION)
      |> E.A.fmap(e =>
           {
             "y0": e.low,
             "y": e.high,
             "x": e.createdAt |> formatDate |> E.JsDate.fromString,
           }
         );
    let competitives =
      sorted
      |> Js.Array.filter(e => e.competitorType == `COMPETITIVE)
      |> E.A.fmap(e =>
           {
             "x": e.createdAt |> formatDate |> E.JsDate.fromString,
             "y1": e.low,
             "y2": e.median,
             "y3": e.high,
           }
         );

    let objectives =
      sorted
      |> Js.Array.filter(e => e.competitorType == `OBJECTIVE)
      |> E.A.fmap(e =>
           {
             "x": e.createdAt |> formatDate |> E.JsDate.fromString,
             "y1": e.low,
             "y2": e.median,
             "y3": e.high,
           }
         );

    let aggregateMedians =
      sorted
      |> Js.Array.filter(e => e.competitorType == `AGGREGATION)
      |> E.A.fmap(e =>
           {
             "x": e.createdAt |> formatDate |> E.JsDate.fromString,
             "y": e.median,
           }
         );

    Victory.(
      <div className=Styles.plot>
        <VictoryChart
          scale={"x": "time"}
          padding={"top": 10, "bottom": 25, "right": 10, "left": 30}
          maxDomain={"y": yMax, "x": xMax}
          minDomain={"y": yMin, "x": xMin}>
          <VictoryArea
            data=aggregatePercentiles
            style={
              "data": {
                "fill": "f6f6f6",
              },
            }
          />
          <VictoryLine
            data=aggregateMedians
            style={
              "data": {
                "stroke": "#ddd",
                "strokeWidth": "1",
                "strokeDasharray": "4 4 4 4",
              },
            }
          />
          {
            competitives
            |> E.A.fmapi((i, e) =>
                 <VictoryMeasurement
                   point=e
                   key={string_of_int(i)}
                   color="GRAY"
                 />
               )
            |> ReasonReact.array
          }
          {
            objectives
            |> E.A.fmapi((i, e) =>
                 <VictoryMeasurement
                   point=e
                   key={string_of_int(i)}
                   color="BLUE"
                 />
               )
            |> ReasonReact.array
          }
        </VictoryChart>
      </div>
    );
  },
};
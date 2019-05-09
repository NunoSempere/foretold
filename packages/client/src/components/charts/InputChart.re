open Utils;

let component = ReasonReact.statelessComponent("Chart");

let make = (~data, _children) => {
  ...component,
  render: _ =>
    Victory.(
      <VictoryChart
        padding={"top": 10, "bottom": 20, "right": 10, "left": 10} height=100>
        <VictoryAxis
          style={
            "axis": {
              "stroke": "#aaa",
            },
            "tickLabels": {
              "fontSize": "7",
              "padding": "2",
            },
          }
        />
        <VictoryArea data style={
                            "data": {
                              "fill": "rgb(133, 167, 179)",
                            },
                          } />
      </VictoryChart>
    ),
};
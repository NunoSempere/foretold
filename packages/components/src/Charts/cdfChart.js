import React from 'react';
import { useEffect } from 'react';
import { useSize } from 'react-use';

import chart from './cdfChartd3';

/**
 * @param min
 * @param max
 * @returns {number}
 */
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

/**
 * Example input:
 * {
 * xs: [50,100,300,400,500,600],
 * ys: [0.1, 0.4, 0.6, 0.7,0.8, 0.9]}
 * }
 */
function CdfChart(props) {
  const id = "chart-" + getRandomInt(0, 100000);
  const [sized, { width }] = useSize(() => <div/>, { width: props.width });

  useEffect(() => {
    const _chart = chart()
      .svgWidth(width)
      .svgHeight(props.height)
      .maxX(props.maxX)
      .minX(props.minX)
      .marginBottom(props.marginBottom || 15)
      .marginLeft(5)
      .marginRight(5)
      .marginTop(5)
      .showDistributionLines(props.showDistributionLines)
      .verticalLine(props.verticalLine)
      .showVerticalLine(props.showVerticalLine)
      .container("#" + id);

    _chart.data({ primary: props.primaryDistribution }).render();
  });

  const style = !!props.width ? { width: props.width + "px" } : {};

  return <div style={{ paddingLeft: "10px", paddingRight: "10px" }}>
    {sized}
    <div id={id} style={style}/>
  </div>;
}

export default CdfChart;
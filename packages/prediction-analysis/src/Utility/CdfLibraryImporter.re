module JS = {
  [@bs.deriving abstract]
  type distJs = {
    xs: array(float),
    ys: array(float),
  };

  let distToJs = (d: Types.distribution) => distJs(~xs=d.xs, ~ys=d.ys);

  let jsToDist = (d: distJs): Types.distribution => {
    xs: xsGet(d),
    ys: ysGet(d),
  };

  let doAsDist = (f, d: Types.distribution) => d |> distToJs |> f |> jsToDist;

  [@bs.module "./CdfLibraryImporter.js"]
  external cdfToPdf: distJs => distJs = "cdfToPdf";

  [@bs.module "./CdfLibraryImporter.js"]
  external findY: (float, distJs) => float = "findY";

  [@bs.module "./CdfLibraryImporter.js"]
  external findX: (float, distJs) => float = "findX";

  [@bs.module "./CdfLibraryImporter.js"]
  external integral: distJs => float = "integral";

  [@bs.module "./CdfLibraryImporter.js"]
  external differentialEntropy: (int, distJs) => distJs =
    "differentialEntropy";

  [@bs.module "./CdfLibraryImporter.js"]
  external scoreMarketCdfCdf: (int, distJs, distJs, distJs) => distJs =
    "scoreMarketCdfCdf";

  [@bs.module "./CdfLibraryImporter.js"]
  external scoreNonMarketCdfCdf: (int, distJs, distJs) => distJs =
    "scoreNonMarketCdfCdf";
};

module Distribution = {
  let toPdf = dist => dist |> JS.doAsDist(JS.cdfToPdf);
  let findX = (y, dist) => dist |> JS.distToJs |> JS.findX(y);
  let findY = (x, dist) => dist |> JS.distToJs |> JS.findY(x);
  let integral = dist => dist |> JS.distToJs |> JS.integral;
  let differentialEntropy = (maxCalculationLength, dist) =>
    dist
    |> JS.doAsDist(JS.differentialEntropy(maxCalculationLength))
    |> integral;
};

module PredictionResolutionGroup = {
  let logScoreNonMarketCdfCdf = (~sampleCount, ~agentPrediction, ~resolution) => {
    JS.scoreNonMarketCdfCdf(
      sampleCount,
      JS.distToJs(agentPrediction),
      JS.distToJs(resolution),
    )
    |> JS.jsToDist
    |> Distribution.integral;
  };
};
/* Untyped file generated by genType. */

import * as Curry from 'bs-platform/lib/es6/curry.js';

import * as LogErrorBS from './LogError.bs';

export const predictionGroupError = function (Arg1, Arg2) {
  const result = Curry._3(LogErrorBS.predictionGroupError, Arg1.scoringCombination.tag==="MarketScore"
    ? [/* MarketScore */-870794282, Arg1.scoringCombination.value]
    : [/* NonMarketScore */463448073, Arg1.scoringCombination.value], Arg1.sampleCount, Arg2);
  return result
};

export const differentialEntropy = function (Arg1, Arg2) {
  const result = Curry._2(LogErrorBS.differentialEntropy, Arg1, Arg2);
  return result
};

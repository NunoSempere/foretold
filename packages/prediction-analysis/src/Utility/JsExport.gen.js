/* Untyped file generated by genType. */

const Curry = require('bs-platform/lib/js/curry.js');

const JsExportBS = require('./JsExport.bs');

const foo = JsExportBS.foo;;
exports.foo = foo

const JsResult_fromResult = function (Arg1) {
  const result = JsExportBS.JsResult[0](Arg1);
  return {data:result[0], error:result[1]}
};;
exports.JsResult_fromResult = JsResult_fromResult

const toMeasurementValue = function (Arg1) {
  const result = JsExportBS.toMeasurementValue(Arg1);
  return {data:result[0], error:result[1]}
};;
exports.toMeasurementValue = toMeasurementValue

const toPredictionResolutionGroup = function (Arg1, Arg2, Arg3) {
  const result = Curry._3(JsExportBS.toPredictionResolutionGroup, Arg1, (Arg2 == null ? undefined : Arg2), Arg3);
  return result
};;
exports.toPredictionResolutionGroup = toPredictionResolutionGroup

const score = function (Arg1, Arg2, Arg3) {
  const result = Curry._3(JsExportBS.score, Arg1, (Arg2 == null ? undefined : Arg2), Arg3);
  return {data:result[0], error:result[1]}
};;
exports.score = score

const fromOption = function (Arg1) {
  const result = JsExportBS.fromOption((Arg1 == null ? undefined : Arg1));
  return result
};;
exports.fromOption = fromOption

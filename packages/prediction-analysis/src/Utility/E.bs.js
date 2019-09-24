// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Option$Rationale = require("rationale/src/Option.js");
var Result$Rationale = require("rationale/src/Result.js");

function listFlatten(results) {
  var match = List.for_all(Belt_Result.isOk, results);
  if (match) {
    return /* Ok */Block.__(0, [List.map(Belt_Result.getExn, results)]);
  } else {
    return /* Error */Block.__(1, [List.map(Belt_Option.getExn, List.filter(Option$Rationale.isSome)(List.map(Result$Rationale.getError, results)))]);
  }
}

function arrayFlatten(results) {
  var match = Belt_Array.some(results, Belt_Result.isError);
  if (match) {
    var __x = $$Array.map(Result$Rationale.getError, results);
    return /* Error */Block.__(1, [$$Array.map(Belt_Option.getExn, Belt_Array.keep(__x, Option$Rationale.isSome))]);
  } else {
    return /* Ok */Block.__(0, [$$Array.map(Belt_Result.getExn, results)]);
  }
}

var Result = /* module */[
  /* listFlatten */listFlatten,
  /* arrayFlatten */arrayFlatten
];

function concatSome(optionals) {
  return optionals.filter(Option$Rationale.isSome).map((function (param) {
                return Option$Rationale.toExn("Warning: This should not have happened", param);
              }));
}

function defaultEmpty(o) {
  if (o !== undefined) {
    return o;
  } else {
    return /* array */[];
  }
}

function toRanges(a) {
  var n = a.length;
  if (n === 0 || n === 1) {
    return /* Error */Block.__(1, ["Must be at least 2 elements"]);
  } else {
    var __x = Belt_Array.makeBy(n - 1 | 0, (function (r) {
            return r;
          }));
    return Curry._1(Result$Rationale.$$return, Belt_Array.map(__x, (function (index) {
                      return /* tuple */[
                              a[index],
                              a[index + 1 | 0]
                            ];
                    })));
  }
}

var $$Array$1 = /* module */[
  /* concatSome */concatSome,
  /* defaultEmpty */defaultEmpty,
  /* toRanges */toRanges,
  /* fold_left */$$Array.fold_left,
  /* fold_right */$$Array.fold_right
];

function min(r) {
  return $$Array.fold_left((function (a, b) {
                var match = a < b;
                if (match) {
                  return a;
                } else {
                  return b;
                }
              }), Number.MAX_VALUE, r);
}

function max(r) {
  return $$Array.fold_left((function (a, b) {
                var match = a > b;
                if (match) {
                  return a;
                } else {
                  return b;
                }
              }), Number.MIN_VALUE, r);
}

var FloatArray = /* module */[
  /* min */min,
  /* max */max
];

exports.Result = Result;
exports.$$Array = $$Array$1;
exports.FloatArray = FloatArray;
/* Option-Rationale Not a pure module */

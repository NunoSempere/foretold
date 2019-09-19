'use strict';

function _interopDefault (ex) { return (ex && (typeof ex === 'object') && 'default' in ex) ? ex['default'] : ex; }

var _ = _interopDefault(require('lodash'));
var math = _interopDefault(require('mathjs'));
var jstat = _interopDefault(require('jstat'));
var Sampling = require('discrete-sampling');
var Sampling__default = _interopDefault(Sampling);

const SUFFIXES = {
  '%': -2,
  'K': 3,
  'M': 6,
  'B': 9,
  'T': 12,
};

const parenthesize = str => `(?:${str})`;
//TODO
const escSpecialChars = str => str.replace(/\$|\{|\}|\_/g, e => `\\${e}`);
const isPresent = e => (!!e && !_.isEmpty(e)) || (typeof e === 'number') || (e === true);
const toSource = re => re instanceof RegExp ? re.source : escSpecialChars(re);
function or(res) {
  const strParts = res.map(toSource).filter(isPresent).map(parenthesize);
  const lengthSorted = _.sortBy(strParts, prt => -prt.length); // To avoid partial replacements.
  return new RegExp(parenthesize(lengthSorted.join('|')), 'g')
}

const spaceSep = res => new RegExp(res.filter(re => !!re).map(re => `(?:${re.source})`).join('\\s*'));
const padded = res => spaceSep([/^/, ...res, /$/]);

const SUFFIX_REGEX = new RegExp(Object.keys(SUFFIXES).join('|'));
const INTEGER_REGEX = /(?:(?:\d+)|(?:\d{1,3}(?:,\d{3})*))(?!\.[^\.])/;
const DECIMAL_REGEX = /\d*\.\d+/;
const NUMBER_REGEX = new RegExp(`(-?${or([INTEGER_REGEX, DECIMAL_REGEX]).source})\\s?(${SUFFIX_REGEX.source})?`);

const POINT_REGEX = padded([NUMBER_REGEX]);
const rangeRegex = (sep, left, right) => padded([left, NUMBER_REGEX, sep, NUMBER_REGEX, right]);

const getMult = suffix => Math.pow(10,SUFFIXES[suffix]);
const parseNumber = (num, suffix) => parseFloat(num.replace(',', '')) * (!!suffix ? getMult(suffix) : 1);

const rangeErrorFn = ([low, high]) => low > high ? {type: 1, subType: 2} : {};

// We assume that if the user started at 0 or tried a negative number,
// they intended for this to be normal.
function getGuesstimateType(guesstimateType, [low]) {
  switch (guesstimateType) {
    case 'UNIFORM': return guesstimateType
    case 'NORMAL': return guesstimateType
    default: return low <= 0 ? 'NORMAL' : 'LOGNORMAL'
  }
}

function regexBasedFormatter(re, guesstimateTypeFn = getGuesstimateType, errorFn = rangeErrorFn) {
  return {
    matches({text}) { return re.test(text) },
    error({text}) { return errorFn(this._numbers(text)) },

    format({guesstimateType, text}) {
      const params = this._numbers(text);
      return {guesstimateType: guesstimateTypeFn(guesstimateType, params), params}
    },

    _numbers(text) { return _.chunk(text.match(re).slice(1), 2).map(([num, suffix]) => parseNumber(num, suffix)) },
  }
}

const item = {
  formatterName: 'DISTRIBUTION_NORMAL_TEXT_UPTO',
  ...regexBasedFormatter(rangeRegex(/to|\.\.|->|:/)),
};

const item$1 = {
  formatterName: 'DISTRIBUTION_NORMAL_TEXT_UPTO',
  ...regexBasedFormatter(rangeRegex(/,\s?/, /\[/, /\]/)),
};

const item$2 = {
  formatterName: 'DISTRIBUTION_PROPORTIONALITY',
  ...regexBasedFormatter(rangeRegex(/of|in/), () => 'BETA'),
};

// We use an empty object here instead of a more meaningful object to play well with mathjs under the hood.
const SAMPLE_FILTERED = {filtered: true};

const filterLessThan = (val, min) => val < min ? SAMPLE_FILTERED : val;
const filterGreaterThan = (val, max) => val > max ? SAMPLE_FILTERED : val;
const filterBetween = (val, min, max) => val < min || val > max ? SAMPLE_FILTERED : val;

const Filters = {
  filterLessThan,
  filterGreaterThan,
  filterBetween,
};

const scenarios = (values, inputProbabilities) => {
  const total = _.sum(inputProbabilities);
  const probabilities = inputProbabilities.map(e => e/total);

  const bound = Math.min(values.length, probabilities.length);
  const testStat = Math.random();

  let running = 0;
  for (var i = 0; i < bound; i++) {
    running += probabilities[i];
    if (testStat < running) {
      return values[i]
    }
  }

  return Math.Nan
};

const ImpureConstructs = {
  scenarios: scenarios,
};

const bernoulli = p => Sampling__default.Bernoulli(p).draw();
const binomial = (n, p) => Sampling__default.Binomial(n,p).draw();
const poisson = (lambda) => Sampling__default.Poisson(lambda).draw();
const negBinomial = (r, p) => Sampling__default.NegBinomial(r, p).draw();

function triangular(min, max, mode = (min + max)/2) {
  const u = Math.random();
  if (u < (mode-min)/(max-min)) {
    return min + Math.sqrt(u*(max-min)*(mode-min))
  } else {
    return max - Math.sqrt((1-u)*(max-min)*(max-mode))
  }
}

// Source:
// http://www.mhnederlof.nl/doubletriangular.html
function doubleTriangular(min, max, mode = (min + max)/2) {
  const u = Math.random();
  if (u <= 0.5) {
    return min + (mode - min)*Math.sqrt(2*u)
  } else {
    return max - (max - mode)*Math.sqrt(2*(1-u))
  }
}

// Source:
// https://en.wikipedia.org/wiki/Beta_distribution#Transformations
function PERT(min, max, mode = (min + max)/2, lambda = 4) {
  const width = max - min;
  const a = 1 + lambda * ((mode - min)/width);
  const b = 1 + lambda * ((max - mode)/width);
  const p = jstat.beta.sample(a, b);
  return min + p*width
}

const Distributions = {
  beta: jstat.beta.sample,
  centralF: jstat.centralF.sample,
  cauchy: jstat.cauchy.sample,
  chisquare: jstat.chisquare.sample,
  exponential: jstat.exponential.sample,
  invgamma: jstat.invgamma.sample,
  lognormal: jstat.lognormal.sample,
  logn: jstat.lognormal.sample,
  normal: jstat.normal.sample,
  n: jstat.normal.sample,
  studentt: jstat.studentt.sample,
  weibull: jstat.weibull.sample,
  uniform: jstat.uniform.sample,
  gamma: jstat.gamma.sample,
  triangular,
  doubleTriangular,
  PERT,
  bernoulli: bernoulli,
  if: bernoulli,
  test: bernoulli,
  binomial: binomial,
  poisson: poisson,
  negBinomial: negBinomial
};

function bimodal(d1, d2, w=math.matrix([0.5])) {
    // A handy shorthand without the need to add square brackets
    if (!isNaN(w)) { //Check if weights specified as numbers, and if so, turn them into a matrix
        var array_ws = Array.prototype.slice.call(arguments,2);
        w = math.matrix(array_ws);
    }
    return multimodal(d1, d2, w)
}

function multimodal() {
    // Input should take the form: ( d1, d2, d3, ... math.matrix([w1, w2, w3, ...])  where
    // dn are numbers/functions which evaluate to numbers when called, and wn are weights.
    // Weights are optional, and if none are specified, the distribution will be sampled from uniformly.
    // Weights can be specified using:
    //  1) probability of being sampled (e.g. [0.2, 0.4, 0.4]), or
    //  2) any other positive number (e.g. [2, 4, 4] or [1, 2, 2]).
    // However, if only some weights are specified, it will be assumed notation of kind 1) is used, and otherwise
    // an error is thrown.
    // (The math.matrix is there instead of an array as mathjs compiles the expression "[]" that way. Might fix later.)
    var args = Array.from(arguments);
    let l = args.length;

    // check whether user submited an array of weights
    if (math.typeof(args[l-1]) == 'Matrix') {
        // separate distributions and weights into separate arrays (and extract array from mathjs matrix)
        var weights = args.pop()._data;

        // Check that all arguments are numbers
        if (args.some(isNaN)) throw new TypeError("Input must be coercible to numbers (except weight array)")

        // add implicit weights, uniform over the remaining probability mass
        let diff = args.length - weights.length;
        let sum_w = weights.reduce((a, b) => a + b, 0);
        let uniform_weights = Array(diff).fill( (1-sum_w)/diff );

        weights = weights.concat(uniform_weights);

        // check for error
        if (weights.some(v => v < 0)) throw new RangeError(`Some negative weights after normalisation.
        Did you maybe use custom weights > 1 and forget to specify all of them?`)
    }
    else {
      //FIX there should be some kind of TypeError check her as well, but want it DRY
        var weights = Array(l).fill(1/l);
    }

    // Sample index and get corresponding distribution
    let probabilities = Sampling.Discrete(weights);
    return args[probabilities.draw()]
}

const Multimodals = {
    bimodal,
    bi: bimodal,
    multimodal,
    mm: multimodal,
};

var Finance = require('financejs');

const finance = new Finance();

const financeFunctions = {
  PV: finance.PV,
  FV: finance.FV,
  NPV: finance.NPV,
  //IRR: finance.IRR, Too slow.
  PP: finance.PP,
  ROI: finance.ROI,
  AM: finance.AM,
  PI: finance.PI,
  DF: finance.DF,
  CI: finance.CI,
  CAGR: finance.CAGR,
  LR: finance.LR,
  R72: finance.R72,
  WACC: finance.WACC
};

// Distributions:
math.import(Distributions, {override: true});
// Financial functions:
math.import(financeFunctions, {override: true});
// Guesstimate constructs:
math.import(ImpureConstructs, {override: true, wrap: true});
// Filters
math.import(Filters, {override: true});
// Multimodals
math.import(Multimodals, {override: true});

const testVar = 30;

module.exports = testVar;

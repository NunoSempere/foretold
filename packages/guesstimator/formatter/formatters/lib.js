import _ from "lodash";
import math from 'mathjs';  
import {distributionUpToIntoLognormal} from "../../lib/distributionMath.js";

const SUFFIXES = {
  '%': -2,
  'K': 3,
  'M': 6,
  'B': 9,
  'T': 12,
}

const parenthesize = str => `(?:${str})`
//TODO
const escSpecialChars = str => str.replace(/\$|\{|\}|\_/g, e => `\\${e}`)
export const isPresent = e => (!!e && !_.isEmpty(e)) || (typeof e === 'number') || (e === true)
const toSource = re => re instanceof RegExp ? re.source : escSpecialChars(re)
function or(res) {
  const strParts = res.map(toSource).filter(isPresent).map(parenthesize)
  const lengthSorted = _.sortBy(strParts, prt => -prt.length) // To avoid partial replacements.
  return new RegExp(parenthesize(lengthSorted.join('|')), 'g')
}

const spaceSep = res => new RegExp(res.filter(re => !!re).map(re => `(?:${re.source})`).join('\\s*'))
const padded = res => spaceSep([/^/, ...res, /$/])

const SUFFIX_REGEX = new RegExp(Object.keys(SUFFIXES).join('|'))
const INTEGER_REGEX = /(?:(?:\d+)|(?:\d{1,3}(?:,\d{3})*))(?!\.[^\.])/
const DECIMAL_REGEX = /\d*\.\d+/
const NUMBER_REGEX = new RegExp(`(-?${or([INTEGER_REGEX, DECIMAL_REGEX]).source})\\s?(${SUFFIX_REGEX.source})?`)
export const DISTRIBUTION_RANGE_REGEX = /to|\.\.|->|:/

export const POINT_REGEX = padded([NUMBER_REGEX])
export const rangeRegex = (sep, left, right) => padded([left, NUMBER_REGEX, sep, NUMBER_REGEX, right])

const getMult = suffix => Math.pow(10,SUFFIXES[suffix])
const parseNumber = (num, suffix) => parseFloat(num.replace(',', '')) * (!!suffix ? getMult(suffix) : 1)

const rangeErrorFn = ([low, high]) => low > high ? {type: 1, subType: 2} : {}

// We assume that if the user started at 0 or tried a negative number,
// they intended for this to be normal.
function getGuesstimateType(guesstimateType, [low]) {
  switch (guesstimateType) {
    case 'UNIFORM': return guesstimateType
    case 'NORMAL': return guesstimateType
    default: return low <= 0 ? 'NORMAL' : 'LOGNORMAL'
  }
}

function getNumbers(text, regEx){
  return _.chunk(text.match(regEx).slice(1), 2).map(([num, suffix]) => parseNumber(num, suffix))
}

export function regexBasedFormatter(re, guesstimateTypeFn = getGuesstimateType, errorFn = rangeErrorFn) {
  return {
    matches({text}) { return re.test(text) },
    error({text}) { return errorFn(this._numbers(text)) },

    format({guesstimateType, text}) {
      const params = this._numbers(text)
      return {guesstimateType: guesstimateTypeFn(guesstimateType, params), params}
    },

    _numbers(text) { getNumbers(text, re) },
  }
}

// Transforms "=mm(normal(10,5), 1 to 100)) -> "=mm(normal(10,5), lognormal(50.1, 1.4))
export function shorthandIntoLognormalFormattingStep(text){
    function shorthandIntoLognormalReplacer(string){
        let rangeRegexIndividual = rangeRegex(DISTRIBUTION_RANGE_REGEX)
        let arrayLowHigh = getNumbers(string, rangeRegexIndividual)
      
        let low = arrayLowHigh[0]
        let high= arrayLowHigh[1]

        return distributionUpToIntoLognormal(low, high)
    } 
    
    let rangeRegexMultiple = (sep, left, right) => spaceSep([left, NUMBER_REGEX, sep, NUMBER_REGEX, right])
    let shorthandIntoLognormalRegex= new RegExp(rangeRegexMultiple(DISTRIBUTION_RANGE_REGEX), "g")
  
    return text.replace(shorthandIntoLognormalRegex, shorthandIntoLognormalReplacer)
}

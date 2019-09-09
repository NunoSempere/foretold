const { interpolate, range, min, max } = require('./functions');
const _ = require('lodash');

class ContinuousDistribution {
  /**
   * @param {number[]} xs
   * @param {number[]} ys
   */
  constructor(xs, ys) {
    if (!_.isArray(xs)) {
      throw new Error('XS should be an array.');
    }
    if (!_.isArray(ys)) {
      throw new Error('YS should be an array.');
    }
    if (!this.validateHasLength(xs)) {
      throw new Error('You need at least one element.');
    }
    if (!this.validateSize(xs, ys)) {
      throw new Error('Arrays of "xs" and "ys" have different sizes.');
    }

    const sorted = this.order(xs, ys);
    this.xs = sorted.xs;
    this.ys = sorted.ys;
  }

  /**
   * Order them to make sure that xs are increasing
   * @param {number[]} xs
   * @param {number[]} ys
   * @return {{ys: number[], xs: number[]}}
   */
  order(xs, ys) {
    const xsYs = xs.map((v, i) => ({ ys: ys[i], xs: v }));
    const sorted = xsYs.sort((a, b) => {
      if (a.xs > b.xs) return 1;
      if (a.xs < b.xs) return -1;
      return 0;
    });

    const XS = sorted.map(v => v.xs);
    const YS = sorted.map(v => v.ys);

    return { xs: XS, ys: YS };
  }

  /**
   * @param {number[]} xs
   * @param {number[]} ys
   * @return {boolean}
   */
  validateSize(xs, ys) {
    return xs.length === ys.length;
  }

  validateHasLength(xs) {
    return xs.length > 0;
  }

  minX(){
    return this.xs[0]
  }

  maxX(){
    return this.xs[this.xs.length - 1]
  }

  /**
   * If xs=[1,2,3], and ys=[5,6,7],
   * then findY(1) = 5, findY(3) = 7, findY(1.5) = 5.5
   * @param {number} x
   * @return {number}
   */
  findY(x) {
    let firstHigherIndex = this.xs.findIndex(X => X >= x);
    if (firstHigherIndex < 0) return this.ys[this.ys.length - 1];
    if (firstHigherIndex === 0) return this.ys[0];
    let lowerOrEqualIndex = firstHigherIndex - 1;
    if (lowerOrEqualIndex < 0) lowerOrEqualIndex = 0;
    let needsInterpolation = this.xs[lowerOrEqualIndex] !== x;
    if (needsInterpolation) {
      return interpolate(
        this.xs[lowerOrEqualIndex],
        this.xs[firstHigherIndex],
        this.ys[lowerOrEqualIndex],
        this.ys[firstHigherIndex],
        x
      );
    } else {
      return this.ys[lowerOrEqualIndex];
    }
  }

  /**
   * If xs=[1,2,3], and ys=[5,6,7],
   * then findX(5) = 1, findX(7) = 3, findY(5.5) = 1.5
   * This should do the same thing as `findY`, but for Y.
   * @param {number} y
   * @return {number}
   */
  findX(y) {
    let firstHigherIndex = this.ys.findIndex(Y => Y >= y);
    if (firstHigherIndex < 0) return this.xs[this.xs.length - 1];
    if (firstHigherIndex === 0) return this.xs[0];
    let lowerOrEqualIndex = firstHigherIndex - 1;
    if (lowerOrEqualIndex < 0) lowerOrEqualIndex = 0;
    let needsInterpolation = this.ys[lowerOrEqualIndex] !== y;
    if (needsInterpolation) {
      return interpolate(
        this.ys[lowerOrEqualIndex],
        this.ys[firstHigherIndex],
        this.xs[lowerOrEqualIndex],
        this.xs[firstHigherIndex],
        y
      );
    } else {
      return this.xs[lowerOrEqualIndex];
    }
  }

  /**
   * @param {number[]} xs
   * @return {ContinuousDistribution}
   */
  convertWithAlternativeXs(xs) {
    const ys = xs.map(x => this.findY(x));
    return new ContinuousDistribution(xs, ys);
  }

  /**
   * @param {number} newLength
   * @return {ContinuousDistribution}
   */
  convertToNewLength(newLength) {
    const _range = range(min(this.xs), max(this.xs), newLength);
    return this.convertWithAlternativeXs(_range);
  }

  /**
   * @return {number}
   */
  sampleSingle() {
    const y = Math.random();
    return this.findX(y);
  }

  /**
   * Poduce n samples, using ``sampleSingle`` for each.
   * @param size
   * @return {number[]}
   */
  sample(size) {
    return Array.from(Array(size), () => this.sampleSingle());
  }

  /**
   * Finds the integral. Takes the average Y value between points, treating them like a triangle.
   * @return {number[]}
   */
  integral(){
    let integral = 0
    for (let i = 1; i < this.ys.length; i++) {
      let thisY = this.ys[i];
      let lastY = this.ys[i-1];
      let thisX = this.xs[i];
      let lastX = this.xs[i-1];
      if (_.isFinite(thisY) && _.isFinite(lastY) && _.isFinite(thisX) && _.isFinite(lastX)){
        let sectionInterval = ((thisY+lastY)/2)*(thisX-lastX);
        integral = integral + sectionInterval
      }
    }
    return integral
  }
}

module.exports = {
  ContinuousDistribution,
};

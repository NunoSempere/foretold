const _ = require('lodash');

const utils = require('../../lib/utils');

/**
 * See "filter.js" comments.
 */
class Pagination {
  /**
   * @public
   * @param {object} [options]
   * @param {number} [options.last]
   * @param {number} [options.first]
   * @param {string} [options.after]
   * @param {string} [options.before]
   * @param {number} [options.limit]
   * @param {number} [options.offset]
   * @param {Layers.orderList} [options.order]
   */
  constructor(options = {}) {
    if (_.has(options, 'last')) {
      this.last = _.get(options, 'last', 0);
    }
    if (_.has(options, 'first')) {
      this.first = _.get(options, 'first', 0);
    }

    if (_.has(options, 'after')) {
      this.after = _.get(options, 'after', 0) * 1;
    }
    if (_.has(options, 'before')) {
      this.before = _.get(options, 'before', 0) * 1;
    }

    if (_.has(options, 'limit')) {
      this.limit = _.get(options, 'limit');
    }
    if (_.has(options, 'offset')) {
      this.offset = _.get(options, 'offset');
    }

    if (_.has(options, 'order')) {
      this.order = this._getOrder(options);
    }
  }

  /**
   * @param {object} options
   * @param {Layers.orderList} options.order
   * @returns {Layers.orderList}
   * @private
   */
  _getOrder(options) {
    const orderInput = _.get(options, 'order');
    const orderArray = _.isArray(orderInput) ? orderInput : [];
    return orderArray
      .filter(item => _.has(item, 'field'))
      .filter(item => _.has(item, 'direction'))
      .map((item) => {
        return {
          field: _.get(item, 'field'),
          direction: _.get(item, 'direction'),
        };
      });
  }

  /**
    * @returns {boolean}
   */
  isOrderSet() {
    return !!this.order && this.order.length > 0;
  }

  /**
   * @returns {{field: any, direction: any}[]}
   */
  getOrder() {
    return this.order;
  }

  /**
   * @public
   * @param {number} total
   * @return {{offset: number, limit: number }}
   */
  getPagination(total = 0) {
    this.before = Math.abs(this.before) || total;
    this.after = Math.abs(this.after) || 0;

    this.last = Math.abs(this.last) || 0;
    this.first = Math.abs(this.first) || 0;

    let offset, limit;
    if (this.first) limit = this.first;
    if (this.after) offset = this.after + 1;

    if (!offset && !limit) {
      if (this.last) {
        limit = this.last;
        offset = this.before - this.last;
      } else if (this.before !== total) {
        limit = this.before;
      }
    }

    offset = offset || 0;
    if (limit > total) limit = total;
    if (offset < 0) {
      limit += offset;
      offset = 0;
    }
    if (limit < 0) limit = 0;

    return { limit, offset };
  }

  inspect() {
    utils.inspect(this);
  }
}

module.exports = {
  Pagination,
};

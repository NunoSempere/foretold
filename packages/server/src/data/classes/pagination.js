const _ = require('lodash');

/**
 * See "filter.js" comments.
 */
class Pagination {
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
  }
}

module.exports = {
  Pagination,
};
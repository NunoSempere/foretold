const assert = require('assert');
const _ = require('lodash');

class FeedItemTemplate {
  /**
   * @param {object} options
   * @param {string} options.item
   * @param {string} options.description
   */
  constructor(options = {}) {
    assert(!!_.has(options, 'item'), 'Item is required');
    assert(!!_.has(options, 'description'), 'Description is required');
    assert(_.isString(options.item), 'Item should be a string');
    assert(_.isString(options.description), 'Item should be a string');

    this.item = options.item;
    this.description = options.description;
  }

  getItem() {
    return this.item;
  }

  getDescription() {
    return this.description;
  }
}

module.exports = {
  FeedItemTemplate,
};

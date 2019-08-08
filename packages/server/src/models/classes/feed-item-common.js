const assert = require('assert');
const _ = require('lodash');
const Mustache = require('mustache');

const { FeedItem } = require('./feed-item');
const { FEED_ITEM_BODY } = require('../enums/feed-item-body');

class FeedItemCommon extends FeedItem {

  /**
   * @param {object} options
   * @param {string} options.item
   * @param {string} options.description
   */
  constructor(options) {
    super(options);
    assert(!!_.has(options, 'item'), 'Item is required');
    assert(!!_.has(options, 'description'), 'Description is required');
    assert(_.isString(options.item), 'Item should be a string');
    assert(_.isString(options.description), 'Item should be a string');

    this.item = options.item;
    this.description = options.description;
  }

  /**
   * @public
   * @return {string}
   */
  getName() {
    return FEED_ITEM_BODY.common;
  }

  /**
   * @public
   * @return {string}
   */
  getItem() {
    return this.item;
  }

  /**
   * @public
   * @return {string}
   */
  getDescription() {
    return this.description;
  }

  /**
   * @public
   * @param {object} replacements
   * @return {FeedItem}
   */
  mutate(replacements) {
    const item = Mustache.render(this.item, replacements);
    const description = Mustache.render(this.description, replacements);

    return new FeedItemCommon({
      item,
      description,
    });
  }
}

module.exports = {
  FeedItemCommon,
};

const _ = require('lodash');
const data = require('../data');

/**
 * @param {*} root
 * @param {object} args
 * @param {object} args.input
 * @param {Schema.Context} options
 * @param {object} info
 * @returns {Promise<Models.User>}
 */
async function create(root, args, options, info) {
  const datas = {
    ...args.input,
    // required
    userId: _.get(options, 'user.id'),
  };
  return await data.bots.createOne(datas);
}

/**
 * @param {*} root
 * @param {object} args
 * @param {string} args.id
 * @param {object} args.input
 * @param {Schema.Context} options
 * @param {object} info
 * @returns {Promise<Models.User>}
 */
async function update(root, args, options, info) {
  return await data.bots.updateOne({ id: args.id }, args.input);
}

/**
 * @param {*} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<*|Array<Model>>}
 */
async function all(root, args, context, info) {
  const datas = { ...args };
  return await data.bots.getAll(datas);
}

/**
 * @param {*} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<*|Array<Model>>}
 */
async function one(root, args, context, info) {
  const id = _.get(args, 'id');
  return await data.bots.getOne({ id });
}

module.exports = {
  all,
  one,
  update,
  create,
};

const _ = require('lodash');

const data = require('../data');

/**
 * @param {object | null} root
 * @param {object} args
 * @param {object} args.id
 * @param {Schema.Context} context
 * @param {object} info
 * @return {Promise<void>}
 */
async function bot(root, args, context, info) {
  const id = _.get(args, 'id');
  console.log('\x1b[36m ---> \x1b[0m Middleware (bot)', { id });
  context.bot = id ? await data.bots.getOne({ id }) : null;
}

module.exports = {
  bot,
};

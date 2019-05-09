const _ = require('lodash');

/**
 * @param {*} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<boolean>}
 */
async function iAmOwner(root, args, context, info) {
  const creatorId = _.get(root, 'creatorId') || _.get(root, 'agentId');
  const currentAgentId = _.get(context, 'agent.id');
  return !!creatorId && creatorId === currentAgentId;
}

/**
 * @todo: remove then
 * @param {*} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<boolean>}
 */
async function iAmOwnerByUserId(root, args, context, info) {
  const creatorId = _.get(root, 'userId');
  const currentCreatorId = _.get(context, 'user.id');
  return !!creatorId && creatorId === currentCreatorId;
}

/**
 * @param {*} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<boolean>}
 */
async function isMe(root, args, context, info) {
  const agentId = _.get(root, 'agentId') || _.get(root, 'id');
  const currentAgentId = _.get(context, 'agent.id');
  return !!agentId && agentId === currentAgentId;
}

module.exports = {
  isMe,
  iAmOwner,
  iAmOwnerByUserId,
};

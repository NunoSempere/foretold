const authorizers = require('../authorizers');

/**
 * @param {object | null} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<*>}
 */
async function all(root, args, context, info) {
  return authorizers.availableAll(root, args, context, info);
}

/**
 * @param {object | null} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<*>}
 */
async function channelPermissions(root, args, context, info) {
  return authorizers.availableChannelPermissions(root, args, context, info);
}

/**
 * @param {object | null} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<*>}
 */
async function channelMembershipsPermissions(root, args, context, info) {
  return authorizers.availableChannelMembershipsPermissions(root, args, context, info);
}

/**
 * @param {object | null} root
 * @param {object} args
 * @param {Schema.Context} context
 * @param {object} info
 * @returns {Promise<*>}
 */
async function measurablesPermissions(root, args, context, info) {
  return authorizers.availableMeasurablesPermissions(root, args, context, info);
}

module.exports = {
  all,
  channelPermissions,
  measurablesPermissions,
  channelMembershipsPermissions,
};

const data = require('../data');

/**
 * @param {Model} channel
 * @returns {Promise<Model>}
 */
async function channelAgents(channel) {
  return await data.channelsData.getAgentsByChannelId(channel.id);
}

/**
 * @param {Model} channel
 * @returns {Promise<Model>}
 */
async function channelCreator(channel) {
  return await data.channelsData.getCreatorByChannelId(channel.id);
}

module.exports = {
  channelAgents,
  channelCreator,
};

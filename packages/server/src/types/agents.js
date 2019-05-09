const graphql = require("graphql");
const { resolver } = require("graphql-sequelize");

const resolvers = require('../resolvers');
const channelMemberships = require('./channel-memberhips');
const models = require("../models");

const agentType = new graphql.GraphQLEnumType({
  name: 'AgentType',
  values: {
    USER: {
      value: 'USER',
    },
    BOT: {
      value: 'BOT',
    },
  },
});

const agent = new graphql.GraphQLObjectType({
  name: 'Agent',
  fields: () => ({
    id: { type: graphql.GraphQLNonNull(graphql.GraphQLString) },
    type: { type: graphql.GraphQLNonNull(agentType) },
    name: { type: graphql.GraphQLString },
    measurementCount: { type: graphql.GraphQLNonNull(graphql.GraphQLInt) },
    channelMemberships: {
      type: graphql.GraphQLNonNull(graphql.GraphQLList(channelMemberships.channelsMembership)),
      resolve: resolvers.channelMemberships.allByAgentId,
    },
    isMe: require('./common').isMe,

    User: {
      type: require('./users').user,
      resolve: resolver(models.Agent.User)
    },

    Bot: {
      type: require('./bots').bot,
      resolve: resolver(models.Agent.Bot)
    },

    Measurements: {
      type: require('../connections').agentMeasurementsConnection.connectionType,
      args: require('../connections').agentMeasurementsConnection.connectionArgs,
      resolve: require('../connections').agentMeasurementsConnection.resolve
    },

    Measurables: {
      type: graphql.GraphQLNonNull(require('./measurables').measurable),
      resolve: resolver(models.Agent.Measurables)
    },

    Channels: {
      type: graphql.GraphQLNonNull(graphql.GraphQLList(require('./channels').channel)),
      resolve: resolver(models.Agent.Channels)
    }
  })
});

module.exports = {
  agent,
  agentType,
};

const graphql = require('graphql');
const { resolver, DateType } = require('graphql-sequelize');

const models = require('../../models/definitions');
const resolvers = require('../resolvers');

const seriesCreateInput = new graphql.GraphQLInputObjectType({
  name: 'SeriesCreateInput',
  fields: () => ({
    name: { type: graphql.GraphQLString },
    description: { type: graphql.GraphQLString },
    channelId: { type: graphql.GraphQLNonNull(graphql.GraphQLString) },
    subjects: { type: graphql.GraphQLList(graphql.GraphQLString) },
    properties: { type: graphql.GraphQLList(graphql.GraphQLString) },
    dates: { type: graphql.GraphQLList(DateType.default) },
  }),
});

const series = new graphql.GraphQLObjectType({
  name: 'Series',
  fields: () => ({
    id: { type: graphql.GraphQLNonNull(graphql.GraphQLString) },
    name: { type: graphql.GraphQLString },
    description: { type: graphql.GraphQLString },
    subjects: { type: graphql.GraphQLList(graphql.GraphQLString) },
    properties: { type: graphql.GraphQLList(graphql.GraphQLString) },
    dates: { type: graphql.GraphQLList(DateType.default) },
    channelId: { type: graphql.GraphQLNonNull(graphql.GraphQLString) },

    measurableCount: {
      type: graphql.GraphQLInt,
      resolve: resolvers.measurables.measurableCount,
    },

    createdAt: { type: graphql.GraphQLNonNull(DateType.default) },
    updatedAt: { type: graphql.GraphQLNonNull(DateType.default) },
    creatorId: { type: graphql.GraphQLString },
    iAmOwner: require('./common').iAmOwner,

    // @todo: security?
    // @todo: Do not use resolver. Use common interfaces of Data layer.
    creator: {
      type: require('./agents').agent,
      resolve: resolver(models.Series.Creator),
    },

    // @todo: security?
    // @todo: Do not use resolver. Use common interfaces of Data layer.
    channel: {
      type: require('./channels').channel,
      resolve: resolver(models.Series.Channel),
    },
  }),
});

module.exports = {
  series,
  seriesCreateInput,
};

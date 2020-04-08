const graphql = require('graphql');

const { CHANNEL_MEMBERSHIP_ROLES } = require('../../../enums');

const channelMembershipRoles = new graphql.GraphQLEnumType({
  name: 'ChannelMembershipRoles',
  values: {
    [CHANNEL_MEMBERSHIP_ROLES.ADMIN]: {
      value: CHANNEL_MEMBERSHIP_ROLES.ADMIN,
    },
    [CHANNEL_MEMBERSHIP_ROLES.VIEWER]: {
      value: CHANNEL_MEMBERSHIP_ROLES.VIEWER,
    },
    [CHANNEL_MEMBERSHIP_ROLES.VERIFIED]: {
      value: CHANNEL_MEMBERSHIP_ROLES.VERIFIED,
    },
  },
});

module.exports = {
  channelMembershipRoles,
};

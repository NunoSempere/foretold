const graphql = require("graphql");

const mutations = new graphql.GraphQLEnumType({
  name: 'mutations',
  values: {
    MEASUREMENT_CREATE: { value: "MEASUREMENT_CREATE" },
    MEASURABLE_CREATE: { value: "MEASURABLE_CREATE" },
    SERIES_CREATE: { value: "SERIES_CREATE" },
    MEASURABLE_ARCHIVE: { value: "MEASURABLE_ARCHIVE" },
    MEASURABLE_UNARCHIVE: { value: "MEASURABLE_UNARCHIVE" },
    MEASURABLE_UPDATE: { value: "MEASURABLE_UPDATE" },
    USER_UPDATE: { value: "USER_UPDATE" },
    CHANNEL_UPDATE: { value: "CHANNEL_UPDATE" },
    CHANNEL_CREATE: { value: "CHANNEL_CREATE" },
    CHANNEL_MEMBERSHIP_CREATE: { value: "CHANNEL_MEMBERSHIP_CREATE" },
    CHANNEL_MEMBERSHIP_DELETE: { value: "CHANNEL_MEMBERSHIP_DELETE" },
    CHANNEL_MEMBERSHIP_ROLE_UPDATE: { value: "CHANNEL_MEMBERSHIP_ROLE_UPDATE" },
    LEAVE_CHANNEL: { value: "LEAVE_CHANNEL" },
    JOIN_CHANNEL: { value: "JOIN_CHANNEL" },
    BOT_CREATE: { value: "BOT_CREATE" },
    BOT_UPDATE: { value: "BOT_UPDATE" },
  }
});

module.exports = {
  mutations,
};

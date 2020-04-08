const MARKET_TYPE = {
  MARKET: 'MARKET',
  NON_MARKET: 'NON_MARKET',
};

const START_AT = {
  QUESTION_CREATION_TIME: 'QUESTION_CREATION_TIME',
  AGENT_MEASUREMENT_CREATION_TIME: 'AGENT_MEASUREMENT_CREATION_TIME',
};

const FINAL_COMPARISON_MEASUREMENT = {
  LAST_OBJECTIVE_MEASUREMENT: 'LAST_OBJECTIVE_MEASUREMENT',
  LAST_AGGREGATE_MEASUREMENT: 'LAST_AGGREGATE_MEASUREMENT',
};

const AGENT_TYPE = {
  USER: 'USER',
  BOT: 'BOT',
};

/**
 * @todo: He is a small issue. If we have "viewers"
 * @todo: so then why then can add predictions?
 * @todo: The better to give them another
 * @todo: group name.
 *
 * @todo: But here is other thing. If we have
 * @todo: "requireVerification" then this is ok.
 */

const CHANNEL_MEMBERSHIP_ROLES = {
  ADMIN: 'ADMIN',
  VIEWER: 'VIEWER',
  NONE: 'NONE',
};

const CHANNEL_MEMBERSHIP_TYPE = {
  ADDED_IN_APP_BY_ADMIN: 'ADDED_IN_APP_BY_ADMIN',
  AGENT_JOINED_DIRECTLY: 'AGENT_JOINED_DIRECTLY',
  ADDED_BY_EMAIL_BY_ADMIN: 'ADDED_BY_EMAIL_BY_ADMIN',
};

const FEED_ITEM_BODY = {
  generic: 'generic',
  measurable: 'measurable',
  measurement: 'measurement',
  joinedMember: 'joinedMember',
  channel: 'channel',
  notebook: 'notebook',
};

const INVITATION_STATUS = {
  AWAITING: 'AWAITING',
  ACCEPTED: 'ACCEPTED',
};

const MEASURABLE_STATE = {
  OPEN: 'OPEN',
  JUDGEMENT_PENDING: 'JUDGEMENT_PENDING',
  JUDGED: 'JUDGED',
  CLOSED_AS_UNRESOLVED: 'CLOSED_AS_UNRESOLVED',
};

const MEASURABLE_VALUE_TYPE = {
  FLOAT: 'FLOAT',
  DATE: 'DATE',
  PERCENTAGE: 'PERCENTAGE',
};

const MEASUREMENT_COMMENT_TYPE = {
  GENERIC: 'GENERIC',
  QUESTION_FEEDBACK: 'QUESTION_FEEDBACK',
  UPDATE: 'UPDATE',
};

const MEASUREMENT_COMPETITOR_TYPE = {
  OBJECTIVE: 'OBJECTIVE',
  COMPETITIVE: 'COMPETITIVE',
  AGGREGATION: 'AGGREGATION',
  UNRESOLVED: 'UNRESOLVED',
  COMMENT: 'COMMENT',
};

const UNRESOLVABLE_RESOLUTIONS = {
  AMBIGUOUS: 'AMBIGUOUS',
  RESULT_NOT_AVAILABLE: 'RESULT_NOT_AVAILABLE',
  FALSE_CONDITIONAL: 'FALSE_CONDITIONAL',
  OTHER: 'OTHER',
};

const MEASUREMENT_VALUE = {
  floatCdf: 'floatCdf',
  floatPoint: 'floatPoint',
  percentage: 'percentage',
  binary: 'binary',
  unresolvableResolution: 'unresolvableResolution',
  comment: 'comment',
  none: 'none',
};

const NOTIFICATION_ERROR_REASON = {
  INTERNAL_ERROR: 'INTERNAL_ERROR',
  EXTERNAL_ERROR: 'EXTERNAL_ERROR',
  PREFERENCES_ERROR: 'PREFERENCES_ERROR',
  EMAIL_ADDRESS_ERROR: 'EMAIL_ADDRESS_ERROR',
};

const NOTIFICATION_TYPE = {
  EMAIL: 'EMAIL',
  PUSH: 'PUSH',
  WEB_PUSH: 'WEB_PUSH',
};

const TEMPLATE_NAME = {
  MEASURABLE_STATE_IS_CHANGED: 'MEASURABLE_STATE_IS_CHANGED',
  MEASURABLE_STATE_IS_RESOLVED: 'MEASURABLE_STATE_IS_RESOLVED',
  MEMBER_ADDED_TO_COMMUNITY: 'MEMBER_ADDED_TO_COMMUNITY',
  NEW_INVITATION: 'NEW_INVITATION',
  EMAIL_OUTER_TEMPLATE: 'EMAIL_OUTER_TEMPLATE',

  MEMBER_JOINED_COMMUNITY_FEED_ITEM: 'MEMBER_JOINED_COMMUNITY_FEED_ITEM',
  NEW_MEASUREMENT_PREDICTION_FEED_ITEM: 'NEW_MEASUREMENT_PREDICTION_FEED_ITEM',
  NEW_MEASUREMENT_COMMENT_FEED_ITEM: 'NEW_MEASUREMENT_COMMENT_FEED_ITEM',
  NEW_MEASUREMENT_RESOLUTION_FEED_ITEM: 'NEW_MEASUREMENT_RESOLUTION_FEED_ITEM',
  NEW_MEASUREMENT_RESOLUTION_NOT_AVAILABLE_FEED_ITEM:
    'NEW_MEASUREMENT_RESOLUTION_NOT_AVAILABLE_FEED_ITEM',
  MEASURABLE_REACHED_RESOLUTION_DATE_FEED_ITEM:
    'MEASURABLE_REACHED_RESOLUTION_DATE_FEED_ITEM',
  NEW_MEASURABLE_FEED_ITEM: 'NEW_MEASURABLE_FEED_ITEM',
  NEW_CHANNEL_FEED_ITEM: 'NEW_CHANNEL_FEED_ITEM',
  NEW_NOTEBOOK_FEED_ITEM: 'NEW_NOTEBOOK_FEED_ITEM',
};

const TEMPLATE_TYPE = {
  EMAIL: 'EMAIL',
  EMAIL_ENVELOPE: 'EMAIL_ENVELOPE',
  FEED_ITEM: 'FEED_ITEM',
};

const TOKEN_TYPE = {
  ACCESS_TOKEN: 'ACCESS_TOKEN',
  AUTH_TOKEN: 'AUTH_TOKEN',
};

module.exports = {
  MARKET_TYPE,
  START_AT,
  FINAL_COMPARISON_MEASUREMENT,
  AGENT_TYPE,
  MEASURABLE_VALUE_TYPE,
  CHANNEL_MEMBERSHIP_ROLES,
  CHANNEL_MEMBERSHIP_TYPE,
  FEED_ITEM_BODY,
  INVITATION_STATUS,
  MEASURABLE_STATE,
  MEASUREMENT_COMMENT_TYPE,
  MEASUREMENT_COMPETITOR_TYPE,
  UNRESOLVABLE_RESOLUTIONS,
  NOTIFICATION_ERROR_REASON,
  NOTIFICATION_TYPE,
  TEMPLATE_NAME,
  TOKEN_TYPE,
  TEMPLATE_TYPE,
  MEASUREMENT_VALUE,
};

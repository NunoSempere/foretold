const { shield, allow, and, or, not } = require('graphql-shield');

const { currentAgentIsAuthenticated } = require('./agents');
const { currentAgentIsApplicationAdmin } = require('./agents');
const { channelIsPublic } = require('./channels');
const { currentAgentIsChannelAdmin } = require('./channel-memberships');
const { currentAgentIsChannelViewer } = require('./channel-memberships');
const {
  channelHasMembershipWithCurrentAgent,
} = require('./channel-memberships');
const { channelHasMultipleAdmins } = require('./channel-memberships');
const { membershipBelongsToCurrentAgent } = require('./channel-memberships');
const { membershipHasAdminRole } = require('./channel-memberships');
const { measurableIsOwnedByCurrentAgent } = require('./measurables');
const { measurableIsArchived } = require('./measurables');
const { botBelongsToCurrentUser } = require('./bots');
const { userIsOwnedByCurrentAgent } = require('./users');
const { preferenceIsOwnedByCurrentAgent } = require('./preferences');
const { agentIdFromRootId } = require('./predicates');
const { agentIdFromContext } = require('./predicates');
const { agentIdFromRootAgentId } = require('./predicates');
const { notebookIsOwnedByCurrentAgent } = require('./notebooks');
const { rateLimit } = require('./rate-limit');
const { measurableIsCompetitiveOrCommentOnly } = require('./measurables');

const currentAgentIsApplicationAdminOrChannelAdmin = or(
  currentAgentIsApplicationAdmin,
  currentAgentIsChannelAdmin,
);

const rulesChannels = () => ({
  Query: {},
  Mutation: {
    channelUpdate: and(
      currentAgentIsAuthenticated,
      currentAgentIsApplicationAdminOrChannelAdmin,
    ),
    leaveChannel: and(
      currentAgentIsAuthenticated,
      channelHasMembershipWithCurrentAgent,
      or(
        and(
          currentAgentIsApplicationAdminOrChannelAdmin,
          channelHasMultipleAdmins,
        ),
        not(currentAgentIsApplicationAdminOrChannelAdmin),
      ),
    ),
    joinChannel: and(
      currentAgentIsAuthenticated,
      channelIsPublic,
      not(channelHasMembershipWithCurrentAgent),
    ),
    channelMembershipCreate: and(
      currentAgentIsAuthenticated,
      currentAgentIsApplicationAdminOrChannelAdmin,
    ),
  },
});

const rulesChannelMemberships = () => ({
  Query: {},
  Mutation: {
    channelMembershipDelete: and(
      currentAgentIsAuthenticated,
      currentAgentIsApplicationAdminOrChannelAdmin,
      or(
        and(membershipHasAdminRole, channelHasMultipleAdmins),
        not(membershipBelongsToCurrentAgent),
      ),
    ),
    channelMembershipRoleUpdate: and(
      currentAgentIsAuthenticated,
      currentAgentIsApplicationAdminOrChannelAdmin,
      or(
        and(channelHasMultipleAdmins, membershipBelongsToCurrentAgent),
        and(channelHasMultipleAdmins, membershipHasAdminRole),
        not(membershipBelongsToCurrentAgent),
      ),
    ),
  },
});

const rulesMeasurables = () => ({
  Query: {},
  Mutation: {
    measurementCreate: and(
      currentAgentIsAuthenticated,
      or(
        channelIsPublic,
        or(
          currentAgentIsApplicationAdminOrChannelAdmin,
          currentAgentIsChannelViewer,
        ),
      ),
    ),
    measurableArchive: and(
      currentAgentIsAuthenticated,
      or(
        currentAgentIsApplicationAdminOrChannelAdmin,
        measurableIsOwnedByCurrentAgent,
      ),
      not(measurableIsArchived),
    ),
    measurableUnarchive: and(
      currentAgentIsAuthenticated,
      or(
        currentAgentIsApplicationAdminOrChannelAdmin,
        measurableIsOwnedByCurrentAgent,
      ),
      measurableIsArchived,
    ),
    measurableUpdate: and(
      currentAgentIsAuthenticated,
      or(
        currentAgentIsApplicationAdminOrChannelAdmin,
        measurableIsOwnedByCurrentAgent,
      ),
    ),
  },
});

const rulesBots = () => ({
  Query: {},
  Mutation: {
    botUpdate: and(
      currentAgentIsAuthenticated,
      botBelongsToCurrentUser,
    ),
  },
});

const rulesInvitations = () => ({
  Query: {},
  Mutation: {
    invitationCreate: and(
      currentAgentIsAuthenticated,
      currentAgentIsApplicationAdminOrChannelAdmin,
    ),
  },
});

const rulesNotebooks = () => ({
  Query: {},
  Mutation: {
    notebookDelete: and(
      currentAgentIsAuthenticated,
      notebookIsOwnedByCurrentAgent,
    ),
    notebookUpdate: and(
      currentAgentIsAuthenticated,
      notebookIsOwnedByCurrentAgent,
    ),
  },
});

const rulesVotes = () => ({
  Query: {},
  Mutation: {
    upvote: and(
      currentAgentIsAuthenticated,
      measurableIsCompetitiveOrCommentOnly,
    ),
    downvote: and(
      currentAgentIsAuthenticated,
      measurableIsCompetitiveOrCommentOnly,
    ),
  },
});

const rules = () => ({
  Bot: {
    token: botBelongsToCurrentUser,
  },
  User: {
    email: userIsOwnedByCurrentAgent(agentIdFromRootAgentId),
    auth0Id: userIsOwnedByCurrentAgent(agentIdFromRootAgentId),
    isEmailVerified: userIsOwnedByCurrentAgent(agentIdFromRootAgentId),
  },
  Agent: {
    preference: userIsOwnedByCurrentAgent(agentIdFromRootId),
  },
  Query: {
    '*': and(allow, rateLimit),

    ...rulesInvitations().Query,
  },
  Mutation: {
    '*': and(
      currentAgentIsAuthenticated,
      // @todo: To apply for other places too.
      rateLimit,
    ),

    mutexTake: currentAgentIsAuthenticated,
    mutexFree: currentAgentIsAuthenticated,
    channelCreate: currentAgentIsAuthenticated,
    botCreate: currentAgentIsAuthenticated,
    userUpdate: and(
      currentAgentIsAuthenticated,
      userIsOwnedByCurrentAgent(agentIdFromContext),
    ),
    preferenceUpdate: and(
      currentAgentIsAuthenticated,
      preferenceIsOwnedByCurrentAgent,
    ),
    subscribe: and(
      currentAgentIsAuthenticated,
      preferenceIsOwnedByCurrentAgent,
    ),
    unsubscribe: and(
      currentAgentIsAuthenticated,
      preferenceIsOwnedByCurrentAgent,
    ),
    seriesCreate: and(
      currentAgentIsAuthenticated,
      or(
        channelIsPublic,
        currentAgentIsApplicationAdminOrChannelAdmin,
      ),
    ),
    measurableCreate: and(
      currentAgentIsAuthenticated,
      or(
        channelIsPublic,
        or(
          currentAgentIsApplicationAdminOrChannelAdmin,
          currentAgentIsChannelViewer,
        ),
      ),
    ),
    globalSettingUpdate: currentAgentIsApplicationAdmin,

    ...rulesBots().Mutation,
    ...rulesMeasurables().Mutation,
    ...rulesChannels().Mutation,
    ...rulesChannelMemberships().Mutation,
    ...rulesInvitations().Mutation,
    ...rulesNotebooks().Mutation,
    ...rulesVotes().Mutation,
  },
});

function getPermissions() {
  return shield(
    rules(),
    { debug: false },
  );
}

module.exports = {
  rules,

  rulesBots,
  rulesChannels,
  rulesMeasurables,
  rulesChannelMemberships,

  getPermissions,
};

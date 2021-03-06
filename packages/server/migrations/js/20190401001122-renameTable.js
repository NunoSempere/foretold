// @todo: In this case "IF EXISTS" is necessary.

module.exports = {
  up: async function (queryInterface) {
    try {
      await queryInterface.sequelize.query(`
        BEGIN;

        DROP TABLE IF EXISTS "ChannelMemberships";

        ALTER TABLE "AgentsChannels"
        RENAME TO "ChannelMemberships";

        ALTER TABLE "ChannelMemberships" DROP CONSTRAINT "AgentsChannels_channelId_fkey";
        ALTER TABLE "ChannelMemberships" DROP CONSTRAINT "AgentsChannels_agentId_fkey";

        ALTER TABLE "ChannelMemberships" ADD CONSTRAINT "ChannelMemberships_channelId_fkey"
        FOREIGN KEY ("channelId") REFERENCES "Channels" ("id")
        on update cascade on delete set null;

        ALTER TABLE "ChannelMemberships" ADD CONSTRAINT "ChannelMemberships_agentId_fkey"
        FOREIGN KEY ("agentId") REFERENCES "Agents" ("id")
        on update cascade on delete set null;

        COMMIT;
    `);
    } catch (e) {
      console.error(e);
      await queryInterface.sequelize.query('ROLLBACK');
      throw e;
    }
  },

  down: async function (queryInterface) {
    try {
      await queryInterface.sequelize.query(`
        BEGIN;

        DROP TABLE IF EXISTS "AgentsChannels";

        ALTER TABLE "ChannelMemberships"
        RENAME TO "AgentsChannels";

        ALTER TABLE "AgentsChannels" DROP CONSTRAINT "ChannelMemberships_channelId_fkey";
        ALTER TABLE "AgentsChannels" DROP CONSTRAINT "ChannelMemberships_agentId_fkey";

        ALTER TABLE "AgentsChannels" ADD CONSTRAINT "AgentsChannels_channelId_fkey"
        FOREIGN KEY ("channelId") REFERENCES "Channels" ("id")
        on update cascade on delete set null;

        ALTER TABLE "AgentsChannels" ADD CONSTRAINT "AgentsChannels_agentId_fkey"
        FOREIGN KEY ("agentId") REFERENCES "Agents" ("id")
        on update cascade on delete set null;

        COMMIT;
    `);
    } catch (e) {
      console.error(e);
      await queryInterface.sequelize.query('ROLLBACK');
      throw e;
    }
  }
};

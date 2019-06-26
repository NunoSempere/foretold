module Queries = {
  module Authentication = Foretold__GraphQL__Authentication;
  module Agent = Foretold__GraphQL__GetAgent;
  module Bots = Foretold__GraphQL__BotsGet;
  module Bot = Foretold__GraphQL__BotGet;
  module Agents = Foretold__GraphQL__GetAgents;
  module Measurable = Foretold__GraphQL__GetMeasurable;
  module Measurables = Foretold__GraphQL__GetMeasurables;
  module Measurements = Foretold__GraphQL__MeasurementsGet;
  module MeasurablesStateStats = Foretold__GraphQL__GetMeasurablesStateStats;
  module MeasurableWithMeasurements = Foretold__GraphQL__GetMeasurableWithMeasurements;
  module Series = Foretold__GraphQL__GetSeries;
  module User = Foretold__GraphQL__GetUser;
  module SeriesCollection = Foretold__GraphQL__GetSeriesCollection;
  module Channels = Foretold__GraphQL__ChannelsGet;
  module Channel = Foretold__GraphQL__ChannelGet;
  module ChannelMemberships = Foretold__GraphQL__ChannelGetMemberships;
};

module Mutations = {
  module MeasurementCreate = Foretold__GraphQL__CreateMeasurementMutation;
  module MeasurableArchive = Foretold__GraphQL__MeasurableArchive;
  module MeasurableUnarchive = Foretold__GraphQL__MeasurableUnarchive;
  module ChannelCreate = Foretold__GraphQL__ChannelCreate;
  module ChannelJoin = Foretold__GraphQL__ChannelJoin;
  module ChannelLeave = Foretold__GraphQL__ChannelLeave;
  module ChannelMembershipCreate = Foretold__GraphQL__ChannelMembershipCreate;
  module ChannelMembershipDelete = Foretold__GraphQL__ChannelMembershipDelete;
  module ChannelUpdate = Foretold__GraphQL__ChannelUpdate;
  module ChannelMembershipRoleUpdate = Foretold__GraphQL__ChannelMembershipRoleUpdate;
  module SeriesCreate = Foretold__GraphQL__SeriesCreate;
  module BotCreate = Foretold__GraphQL__BotCreate;
  module BotUpdate = Foretold__GraphQL__BotUpdate;
};
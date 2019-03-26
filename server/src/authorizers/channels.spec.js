const channels = require('./channels');

describe('authorizers index', () => {

  describe('index', () => {
    it('index', () => {
      expect(channels).toBeInstanceOf(Object);
      expect(channels.authorize).toBeInstanceOf(Function);
    });
  });

  describe('authorize', () => {
    it('authorize', () => {
      expect(channels.authorize()).toBe(false);
      expect(channels.authorize({ isPublic: true })).toBe(true);
      expect(channels.authorize({ isPublic: false })).toBe(false);
      expect(channels.authorize({ isPublic: false })).toBe(false);
      expect(channels.authorize({ isPublic: false }, {})).toBe(true);
    });
  });

  describe('isChannelAllowedRule', () => {
    const root = {};
    const args = {};
    const context = {
      channel: { id: 'id1' },
      agentChannel: { agentId: 'agentId2' },
    };
    const info = {};
    beforeEach(() => {
      jest.spyOn(channels, 'authorize').mockReturnValue(true);
    });
    it('isChannelAllowedRule', () => {
      return channels.isChannelAllowedRule(root, args, context, info).then((res) => {
        expect(channels.authorize).toHaveBeenCalledWith(
          context.channel,
          context.agentChannel,
        );
        expect(res).toBe(true);
      });
    });
  });

});

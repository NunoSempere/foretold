const users = require('./users');
const data = require('../data');

describe('Users Resolvers', () => {

  describe('users()', () => {
    const root = {};
    const args = { id: 'id1', input: { name: 'name1' } };
    const context = { user: { agentId: 'agentId1' } };
    beforeEach(() => {
      jest.spyOn(data.users, 'updateOne').mockReturnValue(
        Promise.resolve(true),
      );
    });
    it('edits user', () => {
      return users.update(root, args, context).then((result) => {
        expect(data.users.updateOne).toHaveBeenCalledWith(
          'id1',
          { name: 'name1' },
          context.user,
        );
        expect(result).toBe(true);
      });
    });
  });

});

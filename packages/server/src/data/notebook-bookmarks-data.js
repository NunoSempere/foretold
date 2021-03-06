const { DataBase } = require('./data-base');
const { NotebookBookmarkModel } = require('./models');

const { Options } = require('./classes');
const { Params } = require('./classes');
const { Data } = require('./classes');
const { Query } = require('./classes');

const logger = require('../lib/log');

/**
 * @implements {Layers.DataSource.DataGeneric}
 * @property {VoteModel} model
 */
class NotebookBookmarksData extends DataBase {
  constructor() {
    super();
    this.model = new NotebookBookmarkModel();
    this.log = logger.module('NotebookBookmarksData');
  }

  /**
   * @param {Defs.NotebookID} notebookId
   * @param {Defs.AgentID} agentId
   * @returns {Promise<boolean>}
   */
  async toggle(notebookId, agentId) {
    const notebookBookmark = await this.one(notebookId, agentId);
    if (!!notebookBookmark) {
      await this.delete(notebookId, agentId);
      return false;
    }
    await this.create(notebookId, agentId);
    return true;
  }

  /**
   * @param {Defs.NotebookID} notebookId
   * @param {Defs.AgentID} agentId
   * @returns {Promise<boolean>}
   */
  async create(notebookId, agentId) {
    const data = new Data({ notebookId, agentId });
    const options = new Options({ raw: true });
    return this.createOne(data, options);
  }

  /**
   * @param {Defs.NotebookID} notebookId
   * @param {Defs.AgentID} agentId
   * @returns {Promise<boolean>}
   */
  async one(notebookId, agentId) {
    const params = new Params({ notebookId, agentId });
    const query = new Query();
    const options = new Options({ raw: true });
    return this.getOne(params, query, options);
  }

  /**
   * @param {Defs.NotebookID} notebookId
   * @param {Defs.AgentID} agentId
   * @returns {Promise<boolean>}
   */
  async delete(notebookId, agentId) {
    const params = new Params({ notebookId, agentId });
    const query = new Query();
    return this.deleteOne(params, query);
  }
}

module.exports = {
  NotebookBookmarksData,
};

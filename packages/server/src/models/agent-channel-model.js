const models = require('./definitions');
const { ModelPostgres } = require('./model-postgres');

/**
 * @implements {Layers.AbstractModelsLayer.AbstractModel}
 */
class AgentChannelModel extends ModelPostgres {
  constructor() {
    super({
      model: models.AgentChannel,
      sequelize: models.sequelize,
    });
  }

  /**
   * @param {Layers.AbstractModelsLayer.options} options
   * @returns {Promise<*>}
   */
  async updateMaterializedView(options) {
    return this._updateMaterializedView('AgentChannels', options);
  }
}

module.exports = {
  AgentChannelModel,
};
const _ = require('lodash');

const models = require('../models');
const { ModelPostgres } = require('./model-postgres');

/**
 * @implements {Layers.AbstractModelsLayer.AbstractModel}
 */
class SeriesModel extends ModelPostgres {

  constructor() {
    super({
      model: models.Series,
      sequelize: models.sequelize,
    });
  }
}

module.exports = {
  SeriesModel,
};

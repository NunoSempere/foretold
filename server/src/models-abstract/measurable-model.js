const _ = require('lodash');

const models = require('../models');
const { ModelPostgres } = require('./model-postgres');

const { MEASURABLE_STATE } = require('../models/measurable-state');

class MeasurableModel extends ModelPostgres {

  constructor() {
    super({
      model: models.Measurable,
      sequelize: models.sequelize,
    });
    this.Op = this.sequelize.Op;
    this.fn = this.sequelize.fn;
  }

  /**
   * @return {Sequelize.literal|*}
   */
  getStateOrderField() {
    return this.sequelize.literal(
      `(CASE WHEN "state"='${MeasurableModel.MEASURABLE_STATE.OPEN}' THEN 1 ` +
      `WHEN "state"='${MeasurableModel.MEASURABLE_STATE.JUDGEMENT_PENDING}' THEN 2 ` +
      `WHEN "state"='${MeasurableModel.MEASURABLE_STATE.JUDGED}' THEN 3 ` +
      `ELSE 5 END) AS "stateOrder"`,
    );
  }

  /**
   * @return {Promise<boolean>}
   */
  needsToBePending() {
    return this.model.needsToBePending();
  }
}

MeasurableModel.MEASURABLE_STATE = MEASURABLE_STATE;

module.exports = {
  MeasurableModel,
};
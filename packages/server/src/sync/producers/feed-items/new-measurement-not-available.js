const { MEASUREMENT_COMPETITOR_TYPE } = require('../../../enums');
const { Producer } = require('../producer');
const { NewMeasurement } = require('./new-measurement');

class NewMeasurementNotAvailable extends NewMeasurement {
  /**
   * @param {Defs.Measurement} measurement
   */
  constructor(measurement) {
    super(measurement);
    this.templateName = Producer.TEMPLATE_NAME
      .NEW_MEASUREMENT_RESOLUTION_NOT_AVAILABLE_FEED_ITEM;
  }

  /**
   * @return {Promise<boolean>}
   */
  async _isActual() {
    return this.input.competitorType === MEASUREMENT_COMPETITOR_TYPE.UNRESOLVED;
  }
}

module.exports = {
  NewMeasurementNotAvailable,
};

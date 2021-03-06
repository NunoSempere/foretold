const { DataBase } = require('./data-base');
const { NotificationStatusModel } = require('./models');

/**
 * @implements {Layers.DataSource.DataGeneric}
 * @property {NotificationStatusModel} model
 */
class NotificationStatusesData extends DataBase {
  constructor() {
    super();
    this.model = new NotificationStatusModel();
  }
}

module.exports = {
  NotificationStatusesData,
};

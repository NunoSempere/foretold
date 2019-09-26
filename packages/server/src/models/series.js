const Sequelize = require('sequelize');

const { MEASURABLE_VALUE_TYPE } = require('../enums/measurable-value-type');

module.exports = (sequelize, DataTypes) => {
  const Series = sequelize.define('Series', {
    id: {
      type: DataTypes.UUID(),
      primaryKey: true,
      defaultValue: DataTypes.UUIDV4,
      allowNull: false,
    },
    name: {
      type: DataTypes.STRING,
      allowNull: true,
    },
    description: {
      type: DataTypes.STRING,
      allowNull: true,
    },
    subjects: {
      type: DataTypes.ARRAY(DataTypes.STRING),
      allowNull: true,
    },
    properties: {
      type: DataTypes.ARRAY(DataTypes.STRING),
      allowNull: true,
    },
    dates: {
      type: DataTypes.ARRAY(DataTypes.DATE),
      allowNull: true,
    },
    channelId: {
      type: DataTypes.UUID(),
      allowNull: false,
    },
    creatorId: {
      type: DataTypes.UUID(),
      allowNull: false,
    },
    createdAt: {
      type: DataTypes.DATE,
      defaultValue: DataTypes.NOW,
    },
    updatedAt: {
      type: DataTypes.DATE,
      defaultValue: DataTypes.NOW,
    },
  });

  Series.prototype.createMeasurables = async function createMeasurables() {
    for (const subject of this.subjects) {
      for (const property of this.properties) {
        for (const date of this.dates) {
          console.log('Making Measurable for Series:', subject, property, date);
          await sequelize.models.Measurable.create({
            name: '',
            labelSubject: subject,
            labelProperty: property,
            labelOnDate: date,
            expectedResolutionDate: date,
            seriesId: this.id,
            creatorId: this.creatorId,
            channelId: this.channelId,
            valueType: MEASURABLE_VALUE_TYPE.FLOAT,
          });
        }
      }
    }
  };

  Series.associate = function associate(models) {
    Series.Creator = Series.belongsTo(models.Agent, {
      foreignKey: 'creatorId',
      as: 'creator',
    });

    Series.Measurables = Series.hasMany(models.Measurable, {
      foreignKey: 'seriesId',
      as: 'Measurables',
    });

    // Usage
    // const se = await models.Series.find();
    // const ch = await se.getChannel();
    Series.Channel = Series.belongsTo(models.Channel, {
      foreignKey: 'channelId',
    });
  };

  return Series;
};

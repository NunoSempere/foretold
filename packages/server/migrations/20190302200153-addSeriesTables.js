module.exports = {
  up: async function (queryInterface, Sequelize) {
    try {
    var ID_TYPE = Sequelize.UUID;
    const ID = {
      allowNull: false,
      primaryKey: true,
      type: ID_TYPE
    };
    const standardColumns = {
      id: ID,
      createdAt: {
        type: Sequelize.DATE
      },
      updatedAt: {
        type: Sequelize.DATE
      },
    };
    const referenceTo = (target, allowNull) => ({
      type: ID_TYPE,
      allowNull,
      references: {
        model: target,
        key: "id"
      }
    });

    await queryInterface.createTable('Series', {
      ...standardColumns,
      name: {
        allowNull: true,
        type: Sequelize.STRING
      },
      description: {
        allowNull: true,
        type: Sequelize.TEXT
      },
      channel: {
        allowNull: false,
        type: Sequelize.TEXT
      },
      creatorId: referenceTo("Agents", false),
      subjects: {
        type: Sequelize.ARRAY(Sequelize.STRING),
        allowNull: true,
      },
      properties: {
        type: Sequelize.ARRAY(Sequelize.STRING),
        allowNull: true,
      },
      dates: {
        type: Sequelize.ARRAY(Sequelize.DATE),
        allowNull: true,
      },
    });

    await queryInterface.addColumn("Measurables", "seriesId",
      referenceTo("Series", true),
    );
    } catch (e) {
      console.error(e);
      throw e;
    }
  },

  down: async function (queryInterface) {
    try {
    await queryInterface.removeColumn("Measurables", "series");
    await queryInterface.dropTable('Series');
    } catch (e) {
      console.error(e);
      throw e;
    }
  }
};


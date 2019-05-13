module.exports = {
  up: async function (queryInterface, Sequelize){
    await queryInterface.addColumn("Measurables", "descriptionProperty", {
          type: Sequelize.STRING,
          allowNull:true 
    });
  },

  down: async function (queryInterface, Sequelize){
      await queryInterface.removeColumn("Measurables", "descriptionProperty");
  }
};


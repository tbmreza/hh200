'use strict';
module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.createTable('request_bodies', {
      id: {
        allowNull: false,
        autoIncrement: true,
        primaryKey: true,
        type: Sequelize.INTEGER
      },
      request_id: {
        type: Sequelize.INTEGER,
        allowNull: false,
        references: { model: 'requests', key: 'id' },
        onDelete: 'CASCADE'
      },
      direction: {
        type: Sequelize.TEXT, // 'sent' | 'received'
        allowNull: false
      },
      content: {
        type: Sequelize.BLOB,
        allowNull: true
      },
      truncated: {
        type: Sequelize.INTEGER,
        allowNull: false,
        defaultValue: 0
      }
    });
  },
  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('request_bodies');
  }
};

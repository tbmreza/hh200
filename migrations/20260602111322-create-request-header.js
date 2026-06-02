'use strict';
module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.createTable('request_headers', {
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
      name: {
        type: Sequelize.TEXT,
        allowNull: false
      },
      value: {
        type: Sequelize.TEXT,
        allowNull: false
      }
    });
  },
  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('request_headers');
  }
};

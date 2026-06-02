'use strict';
module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.createTable('runs', {
      id: {
        allowNull: false,
        autoIncrement: true,
        primaryKey: true,
        type: Sequelize.INTEGER
      },
      name: {
        type: Sequelize.TEXT,
        allowNull: false
      },
      script_path: {
        type: Sequelize.TEXT,
        allowNull: false
      },
      started_at: {
        type: Sequelize.BIGINT, // Unix ms fits in SQLite INTEGER
        allowNull: false
      },
      ended_at: {
        type: Sequelize.BIGINT,
        allowNull: true
      },
      status: {
        type: Sequelize.TEXT, // 'running' | 'done' | 'aborted' | 'race_won'
        allowNull: false
      },
      concurrency: {
        type: Sequelize.INTEGER,
        allowNull: true
      },
      rate_limit: {
        type: Sequelize.REAL,
        allowNull: true
      },
      control_socket: {
        type: Sequelize.TEXT,
        allowNull: true
      }
    });
  },
  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('runs');
  }
};

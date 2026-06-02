'use strict';
module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.createTable('requests', {
      id: {
        allowNull: false,
        autoIncrement: true,
        primaryKey: true,
        type: Sequelize.INTEGER
      },
      run_id: {
        type: Sequelize.INTEGER,
        allowNull: false,
        references: { model: 'runs', key: 'id' },
        onDelete: 'CASCADE'
      },
      seq: {
        type: Sequelize.INTEGER,
        allowNull: false
      },
      sent_at: {
        type: Sequelize.BIGINT,
        allowNull: false
      },
      duration_ms: {
        type: Sequelize.REAL,
        allowNull: false
      },
      method: {
        type: Sequelize.TEXT,
        allowNull: false
      },
      url: {
        type: Sequelize.TEXT,
        allowNull: false
      },
      status_code: {
        type: Sequelize.INTEGER,
        allowNull: true
      },
      error: {
        type: Sequelize.TEXT,
        allowNull: true
      },
      bytes_in: {
        type: Sequelize.INTEGER,
        allowNull: true
      },
      bytes_out: {
        type: Sequelize.INTEGER,
        allowNull: true
      },
      worker_id: {
        type: Sequelize.INTEGER,
        allowNull: false
      }
    });
  },
  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('requests');
  }
};

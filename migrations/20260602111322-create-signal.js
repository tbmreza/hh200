'use strict';
module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.createTable('signals', {
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
      kind: {
        type: Sequelize.TEXT, // 'pause' | 'resume' | 'abort'
        allowNull: false
      },
      sent_at: {
        type: Sequelize.BIGINT,
        allowNull: false
      },
      acked_at: {
        type: Sequelize.BIGINT,
        allowNull: true
      }
    });
  },
  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('signals');
  }
};

'use strict';
const { Model } = require('sequelize');
module.exports = (sequelize, DataTypes) => {
  class Run extends Model {
    static associate(models) {
      Run.hasMany(models.Request, { foreignKey: 'run_id' });
      Run.hasMany(models.Signal, { foreignKey: 'run_id' });
    }
  }
  Run.init({
    name: DataTypes.TEXT,
    script_path: DataTypes.TEXT,
    started_at: DataTypes.BIGINT,
    ended_at: DataTypes.BIGINT,
    status: DataTypes.TEXT,
    concurrency: DataTypes.INTEGER,
    rate_limit: DataTypes.REAL,
    control_socket: DataTypes.TEXT
  }, {
    sequelize,
    modelName: 'Run',
    tableName: 'runs',
    timestamps: false
  });
  return Run;
};

'use strict';
const { Model } = require('sequelize');
module.exports = (sequelize, DataTypes) => {
  class Request extends Model {
    static associate(models) {
      Request.belongsTo(models.Run, { foreignKey: 'run_id' });
      Request.hasMany(models.RequestHeader, { foreignKey: 'request_id' });
      Request.hasMany(models.RequestBody, { foreignKey: 'request_id' });
    }
  }
  Request.init({
    run_id: DataTypes.INTEGER,
    seq: DataTypes.INTEGER,
    sent_at: DataTypes.BIGINT,
    duration_ms: DataTypes.REAL,
    method: DataTypes.TEXT,
    url: DataTypes.TEXT,
    status_code: DataTypes.INTEGER,
    error: DataTypes.TEXT,
    bytes_in: DataTypes.INTEGER,
    bytes_out: DataTypes.INTEGER,
    worker_id: DataTypes.INTEGER
  }, {
    sequelize,
    modelName: 'Request',
    tableName: 'requests',
    timestamps: false
  });
  return Request;
};
// 'use strict';
// const {
//   Model
// } = require('sequelize');
// module.exports = (sequelize, DataTypes) => {
//   class Request extends Model {
//     /**
//      * Helper method for defining associations.
//      * This method is not a part of Sequelize lifecycle.
//      * The `models/index` file will call this method automatically.
//      */
//     static associate(models) {
//       // define association here
//     }
//   }
//   Request.init({
//     run_id: DataTypes.INTEGER,
//     seq: DataTypes.INTEGER,
//     sent_at: DataTypes.BIGINT,
//     duration_ms: DataTypes.FLOAT,
//     method: DataTypes.STRING,
//     url: DataTypes.STRING,
//     status_code: DataTypes.INTEGER,
//     error: DataTypes.STRING,
//     bytes_in: DataTypes.INTEGER,
//     bytes_out: DataTypes.INTEGER,
//     worker_id: DataTypes.INTEGER
//   }, {
//     sequelize,
//     modelName: 'Request',
//   });
//   return Request;
// };

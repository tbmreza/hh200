'use strict';
const { Model } = require('sequelize');
module.exports = (sequelize, DataTypes) => {
  class RequestBody extends Model {
    static associate(models) {
      RequestBody.belongsTo(models.Request, { foreignKey: 'request_id' });
    }
  }
  RequestBody.init({
    request_id: DataTypes.INTEGER,
    direction: DataTypes.TEXT,
    content: DataTypes.BLOB,
    truncated: {
      type: DataTypes.INTEGER,
      defaultValue: 0
    }
  }, {
    sequelize,
    modelName: 'RequestBody',
    tableName: 'request_bodies',
    timestamps: false
  });
  return RequestBody;
};
// 'use strict';
// const {
//   Model
// } = require('sequelize');
// module.exports = (sequelize, DataTypes) => {
//   class RequestBody extends Model {
//     /**
//      * Helper method for defining associations.
//      * This method is not a part of Sequelize lifecycle.
//      * The `models/index` file will call this method automatically.
//      */
//     static associate(models) {
//       // define association here
//     }
//   }
//   RequestBody.init({
//     request_id: DataTypes.INTEGER,
//     direction: DataTypes.STRING,
//     content: DataTypes.BLOB,
//     truncated: DataTypes.INTEGER
//   }, {
//     sequelize,
//     modelName: 'RequestBody',
//   });
//   return RequestBody;
// };

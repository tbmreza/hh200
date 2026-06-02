'use strict';
const { Model } = require('sequelize');
module.exports = (sequelize, DataTypes) => {
  class RequestHeader extends Model {
    static associate(models) {
      RequestHeader.belongsTo(models.Request, { foreignKey: 'request_id' });
    }
  }
  RequestHeader.init({
    request_id: DataTypes.INTEGER,
    direction: DataTypes.TEXT,
    name: DataTypes.TEXT,
    value: DataTypes.TEXT
  }, {
    sequelize,
    modelName: 'RequestHeader',
    tableName: 'request_headers',
    timestamps: false
  });
  return RequestHeader;
};
// 'use strict';
// const {
//   Model
// } = require('sequelize');
// module.exports = (sequelize, DataTypes) => {
//   class RequestHeader extends Model {
//     /**
//      * Helper method for defining associations.
//      * This method is not a part of Sequelize lifecycle.
//      * The `models/index` file will call this method automatically.
//      */
//     static associate(models) {
//       // define association here
//     }
//   }
//   RequestHeader.init({
//     request_id: DataTypes.INTEGER,
//     direction: DataTypes.STRING,
//     name: DataTypes.STRING,
//     value: DataTypes.STRING
//   }, {
//     sequelize,
//     modelName: 'RequestHeader',
//   });
//   return RequestHeader;
// };

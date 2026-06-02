'use strict';
const { Model } = require('sequelize');
module.exports = (sequelize, DataTypes) => {
  class Signal extends Model {
    static associate(models) {
      Signal.belongsTo(models.Run, { foreignKey: 'run_id' });
    }
  }
  Signal.init({
    run_id: DataTypes.INTEGER,
    kind: DataTypes.TEXT,
    sent_at: DataTypes.BIGINT,
    acked_at: DataTypes.BIGINT
  }, {
    sequelize,
    modelName: 'Signal',
    tableName: 'signals',
    timestamps: false
  });
  return Signal;
};
// 'use strict';
// const {
//   Model
// } = require('sequelize');
// module.exports = (sequelize, DataTypes) => {
//   class Signal extends Model {
//     /**
//      * Helper method for defining associations.
//      * This method is not a part of Sequelize lifecycle.
//      * The `models/index` file will call this method automatically.
//      */
//     static associate(models) {
//       // define association here
//     }
//   }
//   Signal.init({
//     run_id: DataTypes.INTEGER,
//     kind: DataTypes.STRING,
//     sent_at: DataTypes.BIGINT,
//     acked_at: DataTypes.BIGINT
//   }, {
//     sequelize,
//     modelName: 'Signal',
//   });
//   return Signal;
// };

'use strict';

module.exports = {
  async up (queryInterface, Sequelize) {
    // 1. Insert a Run
    await queryInterface.bulkInsert('runs', [{
      id: 1,
      name: 'load_test_v1.hhs',
      script_path: '/scripts/load_test_v1.js',
      started_at: Date.now(),
      ended_at: null,
      status: 'running',
      concurrency: 10,
      rate_limit: 250.5,
      control_socket: '/tmp/run_1.sock'
    }], {});

    // 2. Insert a Request bound to that Run
    await queryInterface.bulkInsert('requests', [{
      id: 1,
      run_id: 1,
      seq: 1,
      sent_at: Date.now(),
      duration_ms: 45.2,
      method: 'POST',
      url: 'https://api.example.com/v1/login',
      status_code: 200,
      error: null,
      bytes_in: 512,
      bytes_out: 128,
      worker_id: 3
    }], {});

    // 3. Insert Headers and Bodies for that Request
    await queryInterface.bulkInsert('request_headers', [{
      id: 1,
      request_id: 1,
      direction: 'sent',
      name: 'Content-Type',
      value: 'application/json'
    }], {});

    await queryInterface.bulkInsert('request_bodies', [{
      id: 1,
      request_id: 1,
      direction: 'sent',
      content: Buffer.from('{"user":"test"}'),
      truncated: 0
    }], {});
  },

  async down (queryInterface, Sequelize) {
    await queryInterface.bulkDelete('request_bodies', null, {});
    await queryInterface.bulkDelete('request_headers', null, {});
    await queryInterface.bulkDelete('requests', null, {});
    await queryInterface.bulkDelete('runs', null, {});
  }
};
// 'use strict';
//
// /** @type {import('sequelize-cli').Migration} */
// module.exports = {
//   async up (queryInterface, Sequelize) {
//     /**
//      * Add seed commands here.
//      *
//      * Example:
//      * await queryInterface.bulkInsert('People', [{
//      *   name: 'John Doe',
//      *   isBetaMember: false
//      * }], {});
//     */
//   },
//
//   async down (queryInterface, Sequelize) {
//     /**
//      * Add commands to revert seed here.
//      *
//      * Example:
//      * await queryInterface.bulkDelete('People', null, {});
//      */
//   }
// };

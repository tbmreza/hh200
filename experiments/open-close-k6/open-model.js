import http from 'k6/http';
import { check } from 'k6';

export const options = {
  scenarios: {
    open_model: {
      executor: 'constant-arrival-rate',

      // 10 requests per second
      rate: 10,

      // The time unit for `rate`
      timeUnit: '1s',

      // Start with enough VUs to handle the arrival rate
      preAllocatedVUs: 10,

      // Allow k6 to create more VUs if requests take longer
      maxVUs: 100,

      duration: '30s',
    },
  },
};

export default function () {
  const res = http.get('http://localhost:8080/');

  check(res, {
    'status is 200': (r) => r.status === 200,
  });
}

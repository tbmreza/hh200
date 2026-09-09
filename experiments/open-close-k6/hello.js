import http from 'k6/http';
import { check } from 'k6';

export default function () {
  const res = http.get('http://localhost:8080/');

  check(res, {
    'status is 200': (r) => r.status === 200,
  });
}

import { fileURLToPath } from 'node:url';
import path from 'node:path';
import { PrismaLibSql } from '@prisma/adapter-libsql';
import { PrismaClient } from '../src/generated/prisma/client';
import { faker } from '@faker-js/faker';
import { secondsAgo, randomHttpStatus, randomStatus } from './seed-helpers';

// The Prisma dev database (prisma/app.db). The Haskell dashboard reads a
// *different* database by default (the `HH200_SQLITE` env var, or the XDG data
// dir). To view seeded data in the dashboard, start it with
//   HH200_SQLITE=$PWD/prisma/app.db
const dbPath = path.join(path.dirname(fileURLToPath(import.meta.url)), 'app.db');
const adapter = new PrismaLibSql({ url: `file:${dbPath}` });
const prisma = new PrismaClient({ adapter });

const METHODS = ['GET', 'POST', 'PUT', 'DELETE', 'PATCH'] as const;

async function truncateAll(): Promise<void> {
	await prisma.run.deleteMany();
}

async function seedRun(): Promise<void> {
	const startedAt = secondsAgo(3_600);
	const finished = faker.datatype.boolean();
	const endedAt = finished
		? startedAt + BigInt(faker.number.int({ min: 5, max: 300 }))
		: null;

	const run = await prisma.run.create({
		data: {
			name: `${faker.hacker.noun()}-run`,
			script_path: `/scripts/${faker.system.commonFileName('hhs')}`,
			started_at: startedAt,
			ended_at: endedAt,
			status: randomStatus(finished),
			concurrency: faker.number.int({ min: 1, max: 200 }),
			rate_limit: faker.number.float({ min: 1, max: 1_000, fractionDigits: 2 }),
			control_socket: `/tmp/hh200-${faker.string.alphanumeric(8)}.sock`,
		},
	});

	const requestCount = faker.number.int({ min: 5, max: 40 });

	for (let seq = 0; seq < requestCount; seq++) {
		const sentAt = run.started_at + BigInt(seq * faker.number.int({ min: 0, max: 2 }));
		const isError = faker.datatype.boolean({ probability: 0.1 });

		const request = await prisma.request.create({
			data: {
				run_id: run.id,
				seq,
				sent_at: sentAt,
				duration_ms: faker.number.float({ min: 5, max: 2_000, fractionDigits: 1 }),
				method: faker.helpers.arrayElement([...METHODS]),
				url: faker.internet.url(),
				status_code: randomHttpStatus(isError),
				error: isError ? faker.hacker.phrase() : null,
				bytes_in: faker.number.int({ min: 100, max: 50_000 }),
				bytes_out: faker.number.int({ min: 50, max: 5_000 }),
				worker_id: faker.number.int({ min: 0, max: run.concurrency - 1 }),
			},
		});

		await prisma.requestHeader.createMany({
			data: [
				{ request_id: request.id, direction: 'request', name: 'User-Agent', value: 'hh200/0.1' },
				{ request_id: request.id, direction: 'request', name: 'x-hh-vu', value: String(run.id) },
				{ request_id: request.id, direction: 'response', name: 'Content-Type', value: faker.system.mimeType() },
				{ request_id: request.id, direction: 'response', name: 'x-hh-vu', value: String(request.worker_id) },
			],
		});

		const truncated = faker.datatype.boolean({ probability: 0.15 });
		const rawBody = truncated
			? faker.lorem.paragraphs(20)
			: JSON.stringify({ ok: !isError, id: faker.string.uuid() });

		await prisma.requestBody.create({
			data: {
				request_id: request.id,
				direction: 'response',
				content: Buffer.from(rawBody, 'utf8'),
				truncated,
			},
		});
	}
}

async function main(): Promise<void> {
	await truncateAll();

	const runCount = faker.number.int({ min: 3, max: 8 });

	for (let i = 0; i < runCount; i++) {
		await seedRun();
	}

	console.log(`Seeded ${runCount} runs with dummy data.`);
}

main()
	.catch((e) => {
		console.error(e);
		process.exit(1);
	})
	.finally(() => prisma.$disconnect());

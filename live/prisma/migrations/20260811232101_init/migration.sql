/*
  Warnings:

  - You are about to drop the column `avgLatency` on the `metric_windows` table. All the data in the column will be lost.
  - You are about to drop the column `failCount` on the `metric_windows` table. All the data in the column will be lost.
  - You are about to drop the column `maxLatency` on the `metric_windows` table. All the data in the column will be lost.
  - You are about to drop the column `minLatency` on the `metric_windows` table. All the data in the column will be lost.
  - You are about to drop the column `runId` on the `metric_windows` table. All the data in the column will be lost.
  - Added the required column `avg_latency` to the `metric_windows` table without a default value. This is not possible if the table is not empty.
  - Added the required column `fail_count` to the `metric_windows` table without a default value. This is not possible if the table is not empty.
  - Added the required column `max_latency` to the `metric_windows` table without a default value. This is not possible if the table is not empty.
  - Added the required column `min_latency` to the `metric_windows` table without a default value. This is not possible if the table is not empty.
  - Added the required column `run_id` to the `metric_windows` table without a default value. This is not possible if the table is not empty.

*/
-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_metric_windows" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "run_id" INTEGER NOT NULL,
    "endpoint" TEXT NOT NULL,
    "start" DATETIME NOT NULL,
    "end" DATETIME NOT NULL,
    "count" INTEGER NOT NULL,
    "fail_count" INTEGER NOT NULL,
    "min_latency" REAL NOT NULL,
    "max_latency" REAL NOT NULL,
    "avg_latency" REAL NOT NULL,
    "rps" REAL NOT NULL,
    CONSTRAINT "metric_windows_run_id_fkey" FOREIGN KEY ("run_id") REFERENCES "runs" ("id") ON DELETE RESTRICT ON UPDATE CASCADE
);
INSERT INTO "new_metric_windows" ("count", "end", "endpoint", "id", "rps", "start") SELECT "count", "end", "endpoint", "id", "rps", "start" FROM "metric_windows";
DROP TABLE "metric_windows";
ALTER TABLE "new_metric_windows" RENAME TO "metric_windows";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;

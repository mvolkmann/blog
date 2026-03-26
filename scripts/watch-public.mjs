import {readdirSync, statSync} from 'node:fs';
import path from 'node:path';
import {spawn} from 'node:child_process';

const rootDir = process.cwd();
const watchRoots = ['src/assets', 'src/js'].map(relativePath =>
  path.join(rootDir, relativePath)
);
const pollIntervalMs = 1000;

function getLatestMtimeMs(dirPath) {
  let latest = 0;
  const entries = readdirSync(dirPath, {withFileTypes: true});

  for (const entry of entries) {
    if (entry.name.startsWith('.')) continue;

    const entryPath = path.join(dirPath, entry.name);
    if (entry.isDirectory()) {
      latest = Math.max(latest, getLatestMtimeMs(entryPath));
      continue;
    }

    latest = Math.max(latest, statSync(entryPath).mtimeMs);
  }

  return latest;
}

function getCurrentStamp() {
  return Math.max(...watchRoots.map(getLatestMtimeMs));
}

function runPreparePublic() {
  const child = spawn(process.execPath, ['scripts/prepare-public.mjs'], {
    cwd: rootDir,
    stdio: 'inherit'
  });

  child.on('exit', code => {
    if (code !== 0) process.exit(code ?? 1);
  });
}

let lastStamp = getCurrentStamp();
runPreparePublic();

setInterval(() => {
  const currentStamp = getCurrentStamp();
  if (currentStamp === lastStamp) return;
  lastStamp = currentStamp;
  runPreparePublic();
}, pollIntervalMs);

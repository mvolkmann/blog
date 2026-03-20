import {cpSync, existsSync, mkdirSync, rmSync} from 'node:fs';
import path from 'node:path';

const rootDir = process.cwd();
const publicDir = path.join(rootDir, 'public');
const mappings = [
  ['src/assets', 'public/assets'],
  ['src/js', 'public/js']
];

mkdirSync(publicDir, {recursive: true});

for (const [, target] of mappings) {
  rmSync(path.join(rootDir, target), {force: true, recursive: true});
}

for (const [source, target] of mappings) {
  const sourcePath = path.join(rootDir, source);
  const targetPath = path.join(rootDir, target);

  if (!existsSync(sourcePath)) continue;

  cpSync(sourcePath, targetPath, {
    recursive: true,
    filter: sourceEntry => !sourceEntry.endsWith('.DS_Store')
  });
}

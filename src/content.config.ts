import {defineCollection} from 'astro:content';
import {glob} from 'astro/loaders';
import {z} from 'astro/zod';
import path from 'node:path';

/**
 * This preserves the legacy route shape when generating collection IDs.
 */
function generateTopicId({entry}: {entry: string}) {
  const normalizedEntry = entry.split(path.sep).join('/');
  const withoutExtension = normalizedEntry.replace(/\.md$/, '');
  const basename = path.posix.basename(withoutExtension);
  const dirname = path.posix.basename(path.posix.dirname(withoutExtension));

  if (withoutExtension === 'index') return 'index';
  if (withoutExtension.endsWith('/index')) {
    return withoutExtension.slice(0, -'/index'.length);
  }
  if (basename === dirname) {
    return path.posix.dirname(withoutExtension);
  }

  return withoutExtension;
}

const topics = defineCollection({
  loader: glob({
    base: './src/content/topics',
    generateId: generateTopicId,
    pattern: '**/*.md'
  }),
  schema: z
    .object({
      css: z.string().optional(),
      eleventyNavigation: z
        .object({
          key: z.string().optional(),
          order: z.number().optional(),
          parent: z.string().optional(),
          title: z.string().optional(),
          url: z.string().optional()
        })
        .optional(),
      extraTitle: z.string().optional(),
      layout: z.string().optional(),
      permalink: z.union([z.literal(false), z.string()]).optional(),
      title: z.string().optional()
    })
    .passthrough()
});

export const collections = {topics};

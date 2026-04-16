import {defineConfig} from 'astro/config';
import rehypeExternalLinks from './scripts/rehype-external-links.mjs';

export default defineConfig({
  markdown: {
    rehypePlugins: [rehypeExternalLinks]
  },
  outDir: './_site',
  site: 'https://mvolkmann.github.io',
  base: '/blog',
  trailingSlash: 'always'
});

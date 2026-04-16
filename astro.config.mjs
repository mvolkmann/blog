import {defineConfig} from 'astro/config';
import rehypeExternalLinks from './scripts/rehype-external-links.mjs';

export default defineConfig({
  markdown: {
    rehypePlugins: [rehypeExternalLinks],
    syntaxHighlight: false
  },
  outDir: './_site',
  site: 'https://mvolkmann.github.io',
  base: '/blog',
  trailingSlash: 'always'
});

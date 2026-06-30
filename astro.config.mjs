import {unified} from '@astrojs/markdown-remark';
import {defineConfig} from 'astro/config';
import rehypeExternalLinks from './scripts/rehype-external-links.mjs';

export default defineConfig({
  compressHTML: true,
  markdown: {
    processor: unified({rehypePlugins: [rehypeExternalLinks]}),
    syntaxHighlight: false
  },
  outDir: './_site',
  site: 'https://mvolkmann.github.io',
  base: '/blog',
  trailingSlash: 'always'
});

import {defineConfig} from 'astro/config';

export default defineConfig({
  srcDir: './app',
  outDir: './_site',
  site: 'https://mvolkmann.github.io',
  base: '/blog',
  trailingSlash: 'always'
});

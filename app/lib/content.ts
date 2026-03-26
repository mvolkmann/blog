import {existsSync, readFileSync, readdirSync} from 'node:fs';
import path from 'node:path';
import matter from 'gray-matter';
import MarkdownIt from 'markdown-it';
import markdownItAnchor from 'markdown-it-anchor';
import markdownItFootnote from 'markdown-it-footnote';
import markdownItMathJax from 'markdown-it-mathjax';
import markdownItSub from 'markdown-it-sub';
import markdownItSup from 'markdown-it-sup';
import pkg from '../../package.json';

const CONTENT_ROOT = path.join(process.cwd(), 'src');
const INCLUDES_ROOT = path.join(CONTENT_ROOT, '_includes');
const SKIP_DIRECTORIES = new Set([
  '_data',
  '_includes',
  'actions',
  'assets',
  'js'
]);
const GENERATOR = `Astro v${pkg.dependencies.astro.replace(/^[^\d]*/, '')}`;

const markdown = MarkdownIt({
  breaks: false,
  html: true,
  linkify: true
})
  .use(markdownItMathJax())
  .use(markdownItSub)
  .use(markdownItSup)
  .use(markdownItFootnote)
  .use(markdownItAnchor);

type NavData = {
  key?: string;
  parent?: string;
  title?: string;
  order?: number;
};

type Frontmatter = {
  css?: string;
  eleventyNavigation?: NavData;
  extraTitle?: string;
  layout?: string;
  permalink?: false | string;
  title?: string;
};

export type NavNode = {
  children?: NavNode[];
  key: string;
  order?: number;
  title: string;
  url: string;
};

export type TopicPage = {
  css?: string;
  extraTitle?: string;
  html: string;
  permalink?: string;
  slug: string[];
  sourcePath: string;
  title: string;
  tocHtml: string;
  url: string;
};

type CachedData = {
  navTree: NavNode[];
  pages: TopicPage[];
};

let cachedData: CachedData | undefined;

function getRelativeContentFiles(dir = CONTENT_ROOT, prefix = ''): string[] {
  const entries = readdirSync(dir, {withFileTypes: true});
  const files: string[] = [];

  for (const entry of entries) {
    if (entry.name.startsWith('.')) continue;
    if (entry.isDirectory()) {
      if (SKIP_DIRECTORIES.has(entry.name) && prefix === '') continue;
      files.push(
        ...getRelativeContentFiles(
          path.join(dir, entry.name),
          path.join(prefix, entry.name)
        )
      );
      continue;
    }

    if (!entry.name.endsWith('.md')) continue;
    files.push(path.join(prefix, entry.name));
  }

  return files.sort();
}

function normalizePath(value: string) {
  return value.split(path.sep).join('/');
}

function looksLikeUrl(value: string) {
  return /^(?:https?:\/\/|\/)/.test(value);
}

function escapeHtml(text: string) {
  return text
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;');
}

function stripHtml(text: string) {
  return text.replace(/<[^>]+>/g, '').trim();
}

function appendVersion(url: string) {
  const separator = url.includes('?') ? '&' : '?';
  return `${url}${separator}v=${pkg.version}`;
}

function makeAnchor(url: string, text: string, targetBlank: boolean) {
  const safeUrl = escapeHtml(url);
  const safeText = text.trim();
  if (targetBlank) {
    return `<a href="${safeUrl}" rel="noopener" target="_blank">${safeText}</a>`;
  }
  return `<a href="${safeUrl}" target="frame">${safeText}</a>`;
}

function replaceRawBlocks(markdownText: string) {
  return markdownText.replace(
    /\{%\s*raw\s*%\}([\s\S]*?)\{%\s*endraw\s*%\}/g,
    (_match, content) => content
  );
}

function replaceIncludes(markdownText: string) {
  return markdownText.replace(
    /\{%\s*include\s+["']([^"']+)["']\s*%\}/g,
    (_match, includePath: string) => {
      const resolved = path.join(INCLUDES_ROOT, includePath);
      if (!existsSync(resolved)) return '';
      return readFileSync(resolved, 'utf8');
    }
  );
}

function replaceShortcodes(markdownText: string) {
  return markdownText.replace(
    /\{%\s*(aTargetBlankNoVersion|aTargetBlank|aInternal)\s*([\s\S]*?)%\}/g,
    (_match, shortcode: string, argsText: string) => {
      const args = [...argsText.matchAll(/["']([\s\S]*?)["']/g)].map(
        match => match[1]
      );
      if (args.length < 2) return '';

      let [first, second] = args;
      if (
        shortcode === 'aInternal' &&
        !looksLikeUrl(first) &&
        looksLikeUrl(second)
      ) {
        [first, second] = [second, first];
      }

      if (shortcode === 'aInternal') {
        return makeAnchor(first, second, false);
      }

      const href =
        shortcode === 'aTargetBlankNoVersion' ? first : appendVersion(first);
      return makeAnchor(href, second, true);
    }
  );
}

function preprocessMarkdown(markdownText: string) {
  return replaceShortcodes(
    replaceIncludes(replaceRawBlocks(markdownText))
  ).replaceAll('{{pkg.version}}', pkg.version);
}

function buildTocHtml(html: string) {
  const headings = [
    ...html.matchAll(/<h([2-6]) id="([^"]+)".*?>([\s\S]*?)<\/h\1>/g)
  ];
  if (headings.length === 0) return '';

  let result = '';
  let currentLevel = 0;

  for (const heading of headings) {
    const level = Number(heading[1]) - 1;
    const id = heading[2];
    const text = escapeHtml(stripHtml(heading[3]));

    while (currentLevel < level) {
      result += '<ul>';
      currentLevel += 1;
    }

    while (currentLevel > level) {
      result += '</ul>';
      currentLevel -= 1;
    }

    result += `<li><a href="#${id}">${text}</a></li>`;
  }

  while (currentLevel > 0) {
    result += '</ul>';
    currentLevel -= 1;
  }

  return result;
}

function sortNavItems(items: NavNode[]) {
  items.sort((left, right) => {
    const leftOrder = left.order;
    const rightOrder = right.order;

    if (typeof leftOrder === 'number' && typeof rightOrder === 'number') {
      return leftOrder - rightOrder;
    }
    if (typeof leftOrder === 'number') return -1;
    if (typeof rightOrder === 'number') return 1;
    return left.title.localeCompare(right.title);
  });

  for (const item of items) {
    if (item.children) sortNavItems(item.children);
  }
}

function resolvePermalink(relativePath: string, frontmatter: Frontmatter) {
  if (frontmatter.permalink === false) return undefined;

  const permalink = frontmatter.permalink;
  if (typeof permalink === 'string' && !permalink.includes('{{')) {
    return permalink.replace(/^\/+|\/+$/g, '');
  }

  const withoutExtension = relativePath.replace(/\.md$/, '');
  const basename = path.basename(withoutExtension);
  const dirname = path.basename(path.dirname(withoutExtension));

  if (withoutExtension === 'index') return '';
  if (withoutExtension.endsWith('/index')) {
    return withoutExtension.slice(0, -'/index'.length);
  }
  if (basename === dirname) {
    return path.dirname(withoutExtension);
  }
  return withoutExtension;
}

function buildPage(relativePath: string): TopicPage | undefined {
  const sourcePath = path.join(CONTENT_ROOT, relativePath);
  const sourceText = readFileSync(sourcePath, 'utf8');
  const {content, data} = matter(sourceText);
  const frontmatter = data as Frontmatter;
  const permalink = resolvePermalink(relativePath, frontmatter);

  if (permalink === undefined) return undefined;
  if (relativePath === 'topics/index.md') return undefined;

  const html = markdown.render(preprocessMarkdown(content));
  const slug = permalink === '' ? [] : permalink.split('/').filter(Boolean);
  const title =
    frontmatter.eleventyNavigation?.title ||
    frontmatter.eleventyNavigation?.key ||
    frontmatter.title ||
    path.basename(relativePath, '.md');

  return {
    css: frontmatter.css,
    extraTitle: frontmatter.extraTitle,
    html,
    permalink,
    slug,
    sourcePath: normalizePath(relativePath),
    title,
    tocHtml: buildTocHtml(html),
    url: `/blog/${permalink === '' ? '' : `${permalink}/`}`
  };
}

function loadData(): CachedData {
  if (cachedData) return cachedData;

  const pages = getRelativeContentFiles()
    .map(buildPage)
    .filter((page): page is TopicPage => Boolean(page));

  const pagesBySource = new Map(pages.map(page => [page.sourcePath, page]));
  const allNavItems = new Map<string, NavNode>();
  const roots: NavNode[] = [];

  for (const relativePath of getRelativeContentFiles()) {
    const sourcePath = path.join(CONTENT_ROOT, relativePath);
    const frontmatter = matter(readFileSync(sourcePath, 'utf8'))
      .data as Frontmatter;
    const nav = frontmatter.eleventyNavigation;
    const page = pagesBySource.get(normalizePath(relativePath));

    if (!nav?.key || !page) continue;

    allNavItems.set(nav.key, {
      key: nav.key,
      order: nav.order,
      title: nav.title || nav.key,
      url: appendVersion(page.url)
    });
  }

  for (const relativePath of getRelativeContentFiles()) {
    const sourcePath = path.join(CONTENT_ROOT, relativePath);
    const frontmatter = matter(readFileSync(sourcePath, 'utf8'))
      .data as Frontmatter;
    const nav = frontmatter.eleventyNavigation;

    if (!nav?.key) continue;

    const item = allNavItems.get(nav.key);
    if (!item) continue;

    if (nav.parent) {
      const parent = allNavItems.get(nav.parent);
      if (parent) {
        parent.children ??= [];
        parent.children.push(item);
        continue;
      }
    }

    roots.push(item);
  }

  sortNavItems(roots);
  cachedData = {navTree: roots, pages};
  return cachedData;
}

export function getGenerator() {
  return GENERATOR;
}

export function getSiteVersion() {
  return pkg.version;
}

export function getNavTree() {
  return loadData().navTree;
}

export function getTopicPages() {
  return loadData().pages.filter(page => page.permalink !== '');
}

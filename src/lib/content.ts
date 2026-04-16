import path from 'node:path';
import {getCollection} from 'astro:content';
import pkg from '../../package.json';

type Heading = {
  depth: number;
  slug: string;
  text: string;
};

type NavData = {
  key?: string;
  order?: number;
  parent?: string;
  title?: string;
};

type TopicEntry = Awaited<ReturnType<typeof getCollection<'topics'>>>[number];

export type NavNode = {
  children?: NavNode[];
  key: string;
  order?: number;
  title: string;
  url: string;
};

/**
 * This appends the site version to a URL.
 */
function appendVersion(url: string) {
  const separator = url.includes('?') ? '&' : '?';
  return `${url}${separator}v=${pkg.version}`;
}

/**
 * This converts rendered headings into nested HTML for the table of contents.
 */
export function buildTocHtml(headings: Heading[]) {
  const filteredHeadings = headings.filter(({depth}) => depth >= 2 && depth <= 6);
  if (filteredHeadings.length === 0) return '';

  let currentLevel = 0;
  let result = '';

  for (const {depth, slug, text} of filteredHeadings) {
    const level = depth - 1;

    while (currentLevel < level) {
      result += '<ul>';
      currentLevel += 1;
    }

    while (currentLevel > level) {
      result += '</ul>';
      currentLevel -= 1;
    }

    result += `<li><a href="#${slug}">${text}</a></li>`;
  }

  while (currentLevel > 0) {
    result += '</ul>';
    currentLevel -= 1;
  }

  return result;
}

/**
 * This returns the path segments used to route a topic entry.
 */
function getEntryPath(entry: TopicEntry) {
  if (entry.id === 'index') return undefined;
  if (entry.data.permalink === false) return undefined;

  const permalink = entry.data.permalink;
  if (typeof permalink === 'string' && !permalink.includes('{{')) {
    return permalink.replace(/^\/+|\/+$/g, '');
  }

  return entry.id;
}

/**
 * This resolves the display title for a topic entry.
 */
export function getEntryTitle(entry: TopicEntry) {
  const basename = path.posix.basename(entry.id, path.posix.extname(entry.id));

  return (
    entry.data.eleventyNavigation?.title ||
    entry.data.eleventyNavigation?.key ||
    entry.data.title ||
    basename
  );
}

/**
 * This returns the browser URL for a topic entry.
 */
function getEntryUrl(entry: TopicEntry) {
  const entryPath = getEntryPath(entry);
  if (entryPath === undefined) return undefined;

  return `/blog/${entryPath === '' ? '' : `${entryPath}/`}`;
}

/**
 * This builds the navigation tree for the topics page.
 */
export async function getNavTree() {
  const entries = await getTopicEntries();
  const allNavItems = new Map<string, NavNode>();
  const roots: NavNode[] = [];

  for (const entry of entries) {
    const nav = entry.data.eleventyNavigation as NavData | undefined;
    const url = getEntryUrl(entry);

    if (!nav?.key || !url) continue;

    allNavItems.set(nav.key, {
      key: nav.key,
      order: nav.order,
      title: nav.title || nav.key,
      url: appendVersion(url)
    });
  }

  for (const entry of entries) {
    const nav = entry.data.eleventyNavigation as NavData | undefined;
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
  return roots;
}

/**
 * This returns the topic entries that generate standalone article pages.
 */
export async function getTopicEntries() {
  const entries = await getCollection('topics');
  return entries.filter(entry => getEntryPath(entry) !== undefined);
}

/**
 * This sorts navigation items by explicit order and then by title.
 */
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

/**
 * This returns the route path for a topic entry.
 */
export function toRoutePath(entry: TopicEntry) {
  return getEntryPath(entry) ?? '';
}

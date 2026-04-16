/**
 * This determines whether an href points to an external destination.
 */
function isExternalHref(href) {
  return /^https?:\/\//.test(href);
}

/**
 * This adds target and rel attributes to external links in Markdown output.
 */
export default function rehypeExternalLinks() {
  return tree => {
    visitNodes(tree, node => {
      if (node?.type !== 'element' || node.tagName !== 'a') return;

      const href = node.properties?.href;
      if (typeof href !== 'string' || !isExternalHref(href)) return;

      node.properties ??= {};
      node.properties.rel = 'noopener';
      node.properties.target = '_blank';
    });
  };
}

/**
 * This visits every node in a rehype tree and runs a callback for each one.
 */
function visitNodes(node, callback) {
  callback(node);

  const children = node?.children;
  if (!Array.isArray(children)) return;

  for (const child of children) {
    visitNodes(child, callback);
  }
}

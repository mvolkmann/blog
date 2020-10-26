/*
This renders a single nav item in an li element.
It calls renderNavItem recursively to create a tree of links.
*/
function renderNavItem(entry) {
  let classes = '';
  if (entry.order) classes += ' ordered';
  if (!entry.parent) classes += ' top';

  // handleLinkClick sets the iframe url instead of having
  // an href attribute on the a element so we can animate opacity.
  return `
    <li class="${classes}">
      <a
        class="nav-link"
        onclick="handleLinkClick(this, '${entry.url}')"
        target="frame"
      >
        ${entry.title}
        ${entry.children ? '<div class="triangle">\u{25B6}\u{FE0E}</div>' : ''}
      </a>
      ${renderNavList(entry.children)}
    </li>
  `;
}

/*
This renders a list of nav items inside a ul element.
*/
function renderNavList(entries) {
  if (!entries) return '';
  const items = entries.map(renderNavItem);
  return `<ul>${items.join('')}</ul>`;
}

module.exports = {renderNavList};

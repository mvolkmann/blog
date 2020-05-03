/* global toggleHamburgerMenu: false */

/**
 * This function is used in src/_includes/topics.11ty.js
 * as the click handler for anchor elements in the topic nav.
 */
function handleLinkClick(link, url) {
  // We only need to toggle the "expanded" class for non-leaf links.
  // Links for leaf nodes do not have a next sibling.
  if (link.nextSibling) link.classList.toggle('expanded');

  const activeLink = document.querySelector('.active');

  // If the clicked link is not already the active one ...
  if (link !== activeLink) {
    const iframe = document.querySelector('iframe');
    iframe.style.opacity = 0;
    setTimeout(() => (iframe.src = url), 500); // matches transition-duration in topics.scss

    // If there's a previous active link, style it as no longer active.
    if (activeLink) activeLink.classList.remove('active');

    // Style the clicked link as active.
    link.classList.add('active');

    // Put new page URL in URL hash so it can be bookmarked.
    const {origin} = location;
    location.hash = url.startsWith(origin) ? url.substring(origin.length) : url;
  }

  toggleHamburgerMenu();
}

/*
This renders a single nav item in an li element.
It calls renderNavItem recursively to create a tree of links.
*/
function renderNavItem(entry) {
  let classes = '';
  if (entry.order) classes += ' ordered';
  if (!entry.parent) classes += ' top';

  // handleLinkClick is defined in src/topics/topics.11tydata.js.
  // It sets the iframe url instead of having
  // an href attribute on the a element so we can animate opacity.
  return `
    <li class="${classes}">
      <a
        class="nav-link"
        onclick="handleLinkClick(this, '${entry.url}')"
        target="frame"
      >
        ${entry.title}
        ${entry.children ? '<div class="triangle">&#x25b6;</div>' : ''}
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

module.exports = {
  handleLinkClick,
  renderNavList
};

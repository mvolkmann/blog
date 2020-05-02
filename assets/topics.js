/* global toggleHamburgerMenu: false */

/**
 * This function is used in src/_includes/topics.11ty.js
 * as the click handler for anchor elements in the topic nav.
 */
// eslint-disable-next-line no-unused-vars
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
    const {href} = link;
    const {origin} = location;
    location.hash = href.startsWith(origin)
      ? href.substring(origin.length)
      : href;
  }

  toggleHamburgerMenu();
}

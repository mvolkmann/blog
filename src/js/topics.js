/* global toggleHamburgerMenu: false */

/**
 * This handles clicks on topic navigation links.
 */
// eslint-disable-next-line no-unused-vars
function handleLinkClick(event, link, hasChildren) {
  if (hasChildren) {
    event.preventDefault();
    link.classList.toggle('expanded');
    return false;
  }

  toggleHamburgerMenu();
  return true;
}

/**
 * This prints the current page.
 */
// eslint-disable-next-line no-unused-vars
function printCurrentPage() {
  window.print();
}

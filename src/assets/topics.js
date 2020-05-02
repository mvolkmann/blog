/* global toggleHamburgerMenu: false */

/**
 * This function is used in src/_includes/topics.11ty.js
 * as the click handler for anchor elements.
 */
// eslint-disable-next-line no-unused-vars
function handleClick(link) {
  //TODO: Really only need to do this for links with children.
  link.classList.toggle('expanded');

  const activeLink = document.querySelector('.active');

  // If the clicked link is not already the active one ...
  if (link !== activeLink) {
    // If there's a previous active link, style it as no longer active.
    if (activeLink) activeLink.classList.remove('active');

    // Style the clicked link as active.
    link.classList.add('active');

    //TODO: Why is this delay helpful in getting proper page rendering?
    setTimeout(() => {
      // Put new page URL in URL hash so it can be bookmarked.
      const {href} = link;
      const {origin} = location;
      location.hash = href.startsWith(origin)
        ? href.substring(origin.length)
        : href;

      toggleHamburgerMenu();
    });
  }
}

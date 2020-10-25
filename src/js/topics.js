/* global navLeftWhenClosed: false */

/**
 * This is used as the click handler for topic links.
 */
// eslint-disable-next-line no-unused-vars
function handleLinkClick(link, url) {
  // If the URL starts with "http", open it in a new window.
  if (url.startsWith('http')) {
    window.open(url);
    return;
  }

  // Only toggle the "expanded" class for non-leaf links.
  // Links for leaf nodes do not have a next sibling.
  if (link.nextSibling) {
    link.classList.toggle('expanded');
  } else {
    // Only change the main content area if a leaf link was clicked.
    const activeLink = document.querySelector('.active');

    // If the clicked link is not already the active one ...
    if (link !== activeLink) {
      // If there's a previous active link, style it as no longer active.
      if (activeLink) activeLink.classList.remove('active');

      // Style the clicked link as active.
      link.classList.add('active');

      const iframe = document.querySelector('iframe');
      iframe.style.opacity = 0;
      setTimeout(() => (iframe.src = url), 500); // matches transition-duration in topics.scss

      // Put new page URL in URL hash so it can be bookmarked.
      const {origin} = location;
      location.hash = url.startsWith(origin)
        ? url.substring(origin.length)
        : url;
    }

    // Only toggle hamburger menu if a leaf link was clicked.
    toggleHamburgerMenu();
  }
}

// eslint-disable-next-line no-unused-vars
function iframeLoaded(iframe) {
  iframe.style.opacity = 1;
}

// eslint-disable-next-line no-unused-vars
function toggleHamburgerMenu() {
  const nav = document.querySelector('nav');
  const {left} = nav.style;
  const isOpen = !left || left === '0px';
  nav.style.left = isOpen ? navLeftWhenClosed : 0;
}

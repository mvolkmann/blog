// Only register the service worker when not on localhost.
if (location.hostname !== 'localhost' && 'serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/blog/service-worker.js');
  });
}

let nav;
let navLeftWhenClosed;

/**
 * This is used as the click handler for topic links.
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
    const {origin} = location;
    location.hash = url.startsWith(origin) ? url.substring(origin.length) : url;
  }

  toggleHamburgerMenu();
}

// eslint-disable-next-line no-unused-vars
function iframeLoaded(iframe) {
  iframe.style.opacity = 1;
}

// eslint-disable-next-line no-unused-vars
function toggleHamburgerMenu() {
  const isOpen = nav.style.left === '0px';
  nav.style.left = isOpen ? navLeftWhenClosed : 0;
}

window.onload = () => {
  // If the URL contains a hash, use it to
  // restore the proper page in the iframe.
  const {hash} = location;
  if (hash) {
    const iframe = document.querySelector('iframe');
    iframe.src = hash.substring(1); // removes leading #
  }

  nav = document.querySelector('nav');
  if (nav) {
    navLeftWhenClosed = '-' + nav.getBoundingClientRect().width + 'px';
    nav.style.left = navLeftWhenClosed; // to initially hide it
  }

  const mainLinks = Array.from(document.querySelectorAll('.main-links > a'));
  for (const a of mainLinks) {
    if (a.href === location.href) a.classList.add('selected');
  }
};

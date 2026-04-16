// Only register the service worker when not on localhost.
/*
if (location.hostname !== 'localhost' && 'serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/blog/js/service-worker.js');
  });
}
*/

var navLeftWhenClosed;

/**
 * This computes the off-screen position used to hide the nav.
 */
function getClosedNavOffset(nav) {
  return '-' + nav.getBoundingClientRect().width + 'px';
}

/**
 * This initializes the nav position when the page loads.
 */
function initializeNav() {
  const nav = document.querySelector('nav');
  if (!nav) return;

  navLeftWhenClosed = getClosedNavOffset(nav);
  nav.style.left = navLeftWhenClosed;

  if (location.pathname === '/blog/topics/') {
    nav.style.left = 0;
  }
}

/**
 * This toggles the hamburger menu open and closed.
 */
// eslint-disable-next-line no-unused-vars
function toggleHamburgerMenu() {
  const nav = document.querySelector('nav');
  if (!nav) return;

  const {left} = nav.style;
  const isOpen = !left || left === '0px';
  nav.style.left = isOpen ? navLeftWhenClosed : 0;
}

window.onload = () => {
  initializeNav();

  // Make "Topics" be the default page.
  if (location.pathname === '/blog/') location.href += 'topics/';
};

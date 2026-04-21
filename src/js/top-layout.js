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
 * This gets the sidebar nav element in the main layout.
 */
function getSidebarNav() {
  return document.querySelector('body > main > nav');
}

/**
 * This initializes the nav position when the page loads.
 */
function initializeNav() {
  const nav = getSidebarNav();
  if (!nav) return;

  navLeftWhenClosed = getClosedNavOffset(nav);
  nav.style.left = navLeftWhenClosed;

  if (location.pathname === '/blog/topics/') {
    nav.style.left = '0px';
  }
}

/**
 * This determines whether the nav is currently visible.
 */
function isNavOpen(nav) {
  const left = nav.style.left || getComputedStyle(nav).left;
  return left === '0' || left === '0px';
}

/**
 * This toggles the hamburger menu open and closed.
 */
// eslint-disable-next-line no-unused-vars
function toggleHamburgerMenu() {
  const nav = getSidebarNav();
  if (!nav) return;

  if (isNavOpen(nav)) {
    navLeftWhenClosed = getClosedNavOffset(nav);
    nav.style.left = navLeftWhenClosed;
  } else {
    nav.style.left = '0px';
  }
}

window.onload = () => {
  initializeNav();

  // Make "Topics" be the default page.
  if (location.pathname === '/blog/') location.href += 'topics/';
};

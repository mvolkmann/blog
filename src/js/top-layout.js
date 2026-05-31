// Only register the service worker when not on localhost.
/*
if (location.hostname !== 'localhost' && 'serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/blog/js/service-worker.js');
  });
}
*/

let nav;
let navLeftWhenClosed;

/**
 * This determines whether the nav is currently visible.
 */
function isNavOpen(nav) {
  const left = nav.style.left || getComputedStyle(nav).left;
  return left === '0' || left === '0px';
}

/**
 * This toggles the hamburger menu between open and closed.
 */
// eslint-disable-next-line no-unused-vars
function toggleHamburgerMenu() {
  nav.style.left = isNavOpen(nav) ? navLeftWhenClosed : '0px';
}

window.onload = () => {
  nav = document.querySelector('body > main > nav');
  navLeftWhenClosed = '-' + nav.getBoundingClientRect().width + 'px';
  nav.style.left = navLeftWhenClosed;

  if (location.pathname === '/blog/topics/') {
    nav.style.left = '0px';
  }

  // Make "Topics" be the default page.
  if (location.pathname === '/blog/') location.href += 'topics/';
};

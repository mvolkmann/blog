// Only register the service worker when not on localhost.
if (location.hostname !== 'localhost' && 'serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/blog/service-worker.js');
  });
}

let nav;
let navLeftWhenClosed;

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

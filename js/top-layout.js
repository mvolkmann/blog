// Only register the service worker when not on localhost.
/*
if (location.hostname !== 'localhost' && 'serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/blog/js/service-worker.js');
  });
}
*/

var navLeftWhenClosed;

window.onload = () => {
  // If the URL contains a hash, use it to
  // restore the proper page in the iframe.
  const {hash} = location;
  if (hash) {
    const iframe = document.querySelector('iframe');
    iframe.src = hash.substring(1); // removes leading #
  }

  const nav = document.querySelector('nav');
  if (nav) {
    navLeftWhenClosed = '-' + nav.getBoundingClientRect().width + 'px';
    nav.style.left = navLeftWhenClosed; // to initially hide it
  }

  // Make "Topics" be the default page.
  if (location.pathname === '/blog/') location.href += 'topics/';

  console.log('top-layout.js x: location.href =', location.href);
  const mainLinks = Array.from(document.querySelectorAll('.main-links > a'));
  for (const a of mainLinks) {
    console.log('top-layout.js x: a.href =', a.href);
    if (location.href.startsWith(a.href)) a.classList.add('selected');
  }
};

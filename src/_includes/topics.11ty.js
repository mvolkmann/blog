// This is a layout implemented in JavaScript instead of Nunjucks.
// It is used by src/topics.md.

// This describes front-matter.
exports.data = {
  layout: 'top-layout.njk'
};

// This returns what this layout will render.
//TODO: Is the eleventy-navigation package still needed?
exports.render = data => {
  const {nav} = data.collections;
  // toggleHamburgerMenu and iframeLoaded are
  // defined in src/top-layout.js.
  return `
    <link rel="stylesheet" href="/blog/assets/topics.css">
    <button class="hamburger" onclick="toggleHamburgerMenu()">&#x2630;</button>
    <nav>
      ${data.renderNavList(nav)}
    </nav>
    <section class="topic-content">
      <iframe
        name="frame"
        onLoad={iframeLoaded(this)}
        src="../welcome/"
        title="topic content"></iframe>
    </section>
  `;
};

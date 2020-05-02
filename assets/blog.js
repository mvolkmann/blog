export var lastNavUrl;

export function saveUrl(url) {
  console.log('blog.js saveUrl: url =', url);
  lastNavUrl = url;
}

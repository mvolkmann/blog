import Wrec, {css, html} from './wrec.min.js';

class BasicWrec extends Wrec {
  static css = css`
    span {
      font-family: fantasy;
      font-size: 2rem;
    }
  `;
  static html = html`<span>Hello, World!</span>`;
}

BasicWrec.register();

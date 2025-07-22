import Wrec, {css, html} from './wrec.js';

class CounterWrec extends Wrec {
  static properties = {
    count: {type: Number}
  };

  static css = css`
    :host {
      display: block;
    }
    button {
      background-color: lightgreen;
    }
    button:disabled {
      background-color: gray;
    }
  `;

  static html = html`
    <button disabled="this.count === 0" onClick="this.count--" type="button">
      -
    </button>
    <span>this.count</span>
    <button onClick="this.count++" type="button">+</button>
  `;
}

CounterWrec.register();

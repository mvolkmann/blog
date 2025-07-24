import Wrec, {css, html} from './wrec.js';

class CounterWrec extends Wrec {
  static properties = {
    label: {type: String},
    count: {type: Number}
  };

  static css = css`
    label {
      font-weight: bold;
    }
    button {
      background-color: lightgreen;
    }
    button:disabled {
      opacity: 0.8;
    }
  `;

  static html = html`
    <label>this.label</label>
    <button onClick="this.count--" type="button" disabled="this.count === 0">
      -
    </button>
    <span>this.count</span>
    <button onClick="this.count++" type="button">+</button>
  `;
}

CounterWrec.register();

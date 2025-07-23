import Wrec, {css, html} from './wrec.js';

class NumberSlider extends Wrec {
  static properties = {
    label: {type: String},
    labelWidth: {type: String},
    max: {type: Number, value: 100},
    min: {type: Number, value: 0},
    value: {type: Number}
  };

  static css = css`
    :host {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    input[type='number'] {
      width: 6rem;
    }

    label {
      --label-width: this.labelWidth;
      font-weight: bold;
      text-align: right;
      width: var(--label-width);
    }
  `;

  static html = html`
    <label>this.label</label>
    <input
      type="range"
      min="this.min"
      max="this.max"
      value:input="this.value"
    />
    <span>this.value</span>
  `;
}

NumberSlider.register();

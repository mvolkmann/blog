import Wrec, {css, html} from './wrec.min.js';

class ColorDemo extends Wrec {
  static properties = {
    color: {type: String},
    size: {type: Number, value: 18}
  };

  static css = css`
    :host {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
      font-family: sans-serif;
    }
    p {
      color: this.color;
      font-size: this.size + 'px';
    }
  `;

  static html = html`
    <color-picker color="this.color"></color-picker>
    <number-slider
      label="Size"
      max="48"
      min="12"
      value="this.size"
    ></number-slider>
    <p>This is a test.</p>
  `;
}

ColorDemo.register();

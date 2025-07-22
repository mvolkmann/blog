import Wrec, {css, html} from './wrec.js';

class ColorPicker extends Wrec {
  static properties = {
    labelWidth: {type: String, value: '3rem'},
    red: {type: Number},
    green: {type: Number},
    blue: {type: Number},
    color: {
      type: String,
      computed: '`rgb(${this.red}, ${this.green}, ${this.blue})`'
    }
  };

  static css = css`
    :host {
      display: flex;
      gap: 0.5rem;
    }

    #sliders {
      display: flex;
      flex-direction: column;
      justify-content: space-between;
    }

    #swatch {
      --color: this.color;
      background-color: var(--color);
      height: 5rem;
      width: 5rem;
    }
  `;

  static html = html`
    <div id="swatch"></div>
    <div id="sliders">
      ${this.makeSlider('Red')} ${this.makeSlider('Green')}
      ${this.makeSlider('Blue')}
    </div>
  `;

  static makeSlider(label) {
    return html`
      <number-slider
        label=${label}
        label-width="this.labelWidth"
        max="255"
        value="this.${label.toLowerCase()}"
      ></number-slider>
    `;
  }
}

ColorPicker.register();

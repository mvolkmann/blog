import Wrec, {css, html} from './wrec.min.js';

const capitalize = str =>
  str ? str.charAt(0).toUpperCase() + str.slice(1) : str;

class DataBinding extends Wrec {
  static properties = {
    color: {type: String},
    colors: {type: String, required: true},
    labels: {
      type: String,
      //computed: "this.colors.split(',').map(color => this.capitalize(color)).join(',')",
      computed: 'this.getLabels()',
      uses: 'colors'
    },
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
      --color: this.color;
      --size: this.size;
      color: var(--color);
      font-size: calc(var(--size) * 1px);
      margin: 6px 0;
    }
  `;

  static html = html`
    <div>
      <label>Color Options (comma-separated):</label>
      <input value="this.colors" />
    </div>
    <radio-group
      name="color1"
      labels="this.labels"
      value="this.color"
      values="this.colors"
    ></radio-group>
    <select-list
      name="color2"
      labels="this.labels"
      value="this.color"
      values="this.colors"
    ></select-list>
    <number-slider
      label="Size"
      max="48"
      min="12"
      value="this.size"
    ></number-slider>
    <p>You selected the color <span id="selected-color">this.color</span>.</p>
  `;

  getLabels() {
    return this.colors.split(',').map(capitalize).join(',');
  }
}

DataBinding.register();

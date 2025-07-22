import Wrec, {html} from './wrec.js';

class SelectList extends Wrec {
  static formAssociated = true;

  static properties = {
    name: {type: String, required: true},
    labels: {type: String},
    values: {type: String, required: true},
    value: {type: String}
  };

  static html = html`
    <select name="${this.name}" value="this.value">
      this.values.split(",").map((value, index) => this.makeOption(value, index,
      this.labels)).join("")
    </select>
  `;

  connectedCallback() {
    super.connectedCallback();

    // Wait for the DOM to update.
    requestAnimationFrame(() => {
      const values = this.values.split(',');
      if (!values.includes(this.value)) this.value = values[0];
    });
  }

  makeOption(value, index) {
    let label = this.labels.split(',')[index];
    if (!label) return '';
    value = value.trim();
    return html`<option value="${value}">${label}</option>`;
  }
}

SelectList.register();

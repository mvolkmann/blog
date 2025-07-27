import Wrec, {html} from './wrec.min.js';

class SelectList extends Wrec {
  static formAssociated = true;

  static properties = {
    name: {type: String, required: true},
    labels: {type: String, required: true},
    values: {type: String, required: true},
    value: {type: String}
  };

  static html = html`
    <select name="${this.name}" value="this.value">
      <!-- prettier-ignore -->
      this.values
        .split(",")
        .map(this.makeOption.bind(this))
        .join("")
    </select>
  `;

  #labelArray = [];

  connectedCallback() {
    super.connectedCallback();
    this.#fixValue();
  }

  attributeChangedCallback(attrName, oldValue, newValue) {
    super.attributeChangedCallback(attrName, oldValue, newValue);
    if (attrName === 'labels') {
      this.#labelArray = this.labels.split(',');
    }
  }

  // This handles the case when the specified value
  // is not in the list of values.
  #fixValue() {
    requestAnimationFrame(() => {
      const values = this.values.split(',');
      if (this.value) {
        if (!values.includes(this.value)) this.value = values[0];
      } else {
        this.value = values[0];
      }
    });
  }

  // This method cannot be private because it is
  // called from the expression in the html method.
  makeOption(value, index) {
    return html`
      <option value="${value.trim()}">${this.#labelArray[index]}</option>
    `;
  }
}

SelectList.register();

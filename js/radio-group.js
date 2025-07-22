import Wrec, {css, html} from './wrec.js';

class RadioGroup extends Wrec {
  static formAssociated = true;

  static properties = {
    labels: {type: String},
    name: {type: String, required: true},
    values: {type: String, required: true},
    value: {type: String}
  };

  static css = css`
    :host > div {
      display: flex;
      gap: 0.5rem;

      > div {
        display: flex;
        align-items: center;
      }
    }
  `;

  static html = html`
    <div>
      this.values.split(",").map((value, index) => this.makeRadio(value, index,
      this.labels )).join("")
    </div>
  `;

  connectedCallback() {
    super.connectedCallback();
    if (!this.value) this.value = this.values.split(',')[0];
    this.#fixValue();
  }

  attributeChangedCallback(attrName, oldValue, newValue) {
    super.attributeChangedCallback(attrName, oldValue, newValue);
    if (attrName === 'value') {
      // Update the checked state of the radio buttons.
      const inputs = this.shadowRoot.querySelectorAll('input');
      for (const input of inputs) {
        input.checked = input.value === newValue;
      }
    } else if (attrName === 'values') {
      this.#fixValue();
    }
  }

  // This handles the case when the specified value
  // is not in the list of values.
  #fixValue() {
    requestAnimationFrame(() => {
      const values = this.values.split(',');
      if (!values.includes(this.value)) this.value = values[0];
    });
  }

  handleChange(event) {
    this.value = event.target.value;
  }

  makeRadio(value, index) {
    let label = this.labels.split(',')[index];
    if (!label) return '';
    value = value.trim();
    return html`
      <div>
        <input
          type="radio"
          id="${value}"
          name="${this.name}"
          onchange="handleChange"
          value="${value}"
          ${value === this.value ? 'checked' : ''}
        />
        <label for="${value}">${label}</label>
      </div>
    `;
  }
}

RadioGroup.register();

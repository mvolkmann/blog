import Wrec, {css, createElement, html} from './wrec.min.js';
// This component demonstrates a different way of implementing reactivity.
// Rather than relying on the parsing of JavaScript expressions in HTML,
// it implements the propertyChangedCallback method which is unique to wrec.
class TablePlus extends Wrec {
  static properties = {
    headings: {type: Array, value: []},
    properties: {type: Array, value: []},
    data: {type: Array, value: []}
  };
  static css = css`
    .sort-indicator {
      color: white;
      display: inline-block;
      line-height: 1rem;
      margin-left: 0.5rem;
      width: 1rem;
    }
    table {
      border-collapse: collapse;
    }
    td,
    th {
      border: 2px solid gray;
      padding: 0.5rem;
    }
    th {
      background-color: cornflowerblue;
      color: white;
      cursor: pointer;
    }
  `;
  static html = html`
    <table>
      <thead>
        <tr></tr>
      </thead>
      <tbody></tbody>
    </table>
  `;
  sortAscending = true;
  sortHeader = null;
  connectedCallback() {
    super.connectedCallback();
    requestAnimationFrame(() => {
      this.buildRows();
      this.buildHeadings();
    });
  }
  buildHeadings() {
    const tr = this.shadowRoot?.querySelector('table > thead > tr');
    if (!tr) return; // should never happen
    tr.innerHTML = ''; // removes existing children
    this.headings.forEach((header, index) => {
      tr.appendChild(this.buildTh(header, index));
    });
  }
  buildRow(obj) {
    return html`
      <tr>
        ${this.properties.map(prop => html`<td>${obj[prop]}</td>`).join('')}
      </tr>
    `;
  }
  buildRows() {
    const tbody = this.shadowRoot?.querySelector('table > tbody');
    if (!tbody) return; // should never happen
    const rows = this.data.map(this.buildRow.bind(this));
    tbody.innerHTML = rows.join('');
  }
  buildTh(heading, index) {
    const property = this.properties[index];
    const th = createElement(
      'th',
      {
        'aria-label': `sort by ${heading}`,
        role: 'button',
        tabindex: '0'
      },
      html`
        <span>${heading}</span>
        <span class="sort-indicator"></span>
      `
    );
    th.addEventListener('click', () => {
      const sameProperty = th === this.sortHeader;
      this.sortAscending = sameProperty ? !this.sortAscending : true;
      this.data.sort((a, b) => {
        const aValue = a[property];
        const bValue = b[property];
        let compare =
          typeof aValue === 'string'
            ? aValue.localeCompare(bValue)
            : typeof aValue === 'number'
            ? aValue - bValue
            : 0;
        return this.sortAscending ? compare : -compare;
      });
      // Trigger the property set method by assigning a clone.
      this.data = [...this.data];
      // Clear sort indicator from previously selected header.
      if (this.sortHeader) {
        const sortIndicator = this.sortHeader.querySelector('.sort-indicator');
        if (sortIndicator) sortIndicator.textContent = '';
      }
      // Add sort indicator to currently selected header.
      const sortIndicator = th.querySelector('.sort-indicator');
      if (sortIndicator) {
        sortIndicator.textContent = this.sortAscending ? '\u25B2' : '\u25BC';
      }
      this.sortHeader = th;
    });
    return th;
  }
  propertyChangedCallback(propName) {
    if (propName === 'headings') {
      this.buildHeadings();
    } else if (propName === 'properties' || propName === 'data') {
      this.buildRows();
    }
  }
}
TablePlus.register();
//# sourceMappingURL=table-plus.js.map

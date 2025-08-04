import Wrec, {css, html} from './wrec.min.js';
class TableWired extends Wrec {
  static properties = {
    headings: {type: Array},
    properties: {type: Array},
    data: {type: Array}
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
      > span {
        pointer-events: none;
      }
    }
  `;
  static html = html`
    <table>
      <thead>
        <tr>
          this.headings.map(this.makeTh.bind(this)).join('')
        </tr>
      </thead>
      <tbody>
        this.data.map(this.makeTr.bind(this)).join('')
      </tbody>
    </table>
  `;
  sortAscending = true;
  sortHeader = null;
  makeTd(index, prop) {
    const value = this.data[index][prop];
    return html`<td>${value}</td>`;
  }
  makeTh(heading, index) {
    return html`
      <th
        aria-label="sort by ${heading}"
        data-property="${this.properties[index]}"
        onclick="sort"
        role="button"
        tabindex="0"
      >
        <span>${heading}</span>
        <span class="sort-indicator"></span>
      </th>
    `;
  }
  makeTr(obj, index) {
    return html`
      <tr>
        this.properties.map(this.makeTd.bind(this, ${index})).join('')
      </tr>
    `;
  }
  sort(event) {
    let th = event.target;
    const property = th.getAttribute('data-property');
    this.sortAscending = th === this.sortHeader ? !this.sortAscending : true;
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
  }
}
TableWired.register();
//# sourceMappingURL=table-wired.js.map

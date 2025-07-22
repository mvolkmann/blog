const FIRST_CHAR = "a-zA-Z_$";
const OTHER_CHAR = FIRST_CHAR + "0-9";
const IDENTIFIER = `[${FIRST_CHAR}][${OTHER_CHAR}]*`;
const REFERENCE_RE = new RegExp(`^this.${IDENTIFIER}$`);
const REFERENCES_RE = new RegExp(`this.${IDENTIFIER}`, "g");
const SKIP = "this.".length;

const defaultForType = (type) =>
  type === Number ? 0 : type === Boolean ? false : "";

function updateAttribute(element, attrName, value) {
  const currentValue = element.getAttribute(attrName);
  if (typeof value === "boolean") {
    if (value) {
      if (currentValue !== attrName) {
        element.setAttribute(attrName, attrName);
      }
    } else {
      element.removeAttribute(attrName);
    }
  } else if (currentValue !== value) {
    element.setAttribute(attrName, value);
  }
}

function updateValue(element, attrName, value) {
  if (element instanceof CSSRule) {
    element.style.setProperty(attrName, value); // CSS variable
  } else {
    updateAttribute(element, attrName, value);
  }
}

class Wrec extends HTMLElement {
  static #propToAttrMap = new Map();
  static #attrToPropMap = new Map();

  #expressionToReferencesMap = new Map();
  #formData;
  #internals;
  #propertyToBindingsMap = new Map();

  constructor() {
    super();
    this.attachShadow({ mode: "open" });

    if (!this.constructor.properties) this.constructor.properties = {};

    const map = this.constructor["#propertyToExpressionsMap"];
    if (!map) this.constructor["#propertyToExpressionsMap"] = new Map();

    if (this.constructor.formAssociated) {
      this.#internals = this.attachInternals();
      this.#formData = new FormData();
      this.#internals.setFormValue(this.#formData);
    }
  }

  attributeChangedCallback(attrName, _, newValue) {
    // Update corresponding property.
    const propertyName = Wrec.getPropertyName(attrName);
    const value = this.#typedValue(propertyName, newValue);
    this[propertyName] = value;
    this.#setFormValue(propertyName, value);
  }

  // attrName must be "value" OR undefined!
  #bind(element, propertyName, attrName) {
    element.addEventListener("input", (event) => {
      this[propertyName] = event.target.value;
    });

    let bindings = this.#propertyToBindingsMap.get(propertyName);
    if (!bindings) {
      bindings = [];
      this.#propertyToBindingsMap.set(propertyName, bindings);
    }
    bindings.push(attrName ? { element, attrName } : element);
  }

  #buildDOM() {
    const clazz = this.constructor;
    let { _template } = clazz;
    if (!_template) {
      _template = clazz.template = document.createElement("template");
      let text = clazz.css ? `<style>${clazz.css}</style>` : "";
      text += clazz.html;
      _template.innerHTML = text;
    }
    this.shadowRoot.replaceChildren(_template.content.cloneNode(true));
  }

  connectedCallback() {
    this.#validateAttributes();
    this.#defineProperties();
    this.#buildDOM();

    // Wait for the DOM to update.
    requestAnimationFrame(() => {
      this.#wireEvents(this.shadowRoot);
      this.#makeReactive(this.shadowRoot);
      this.constructor.processed = true;
      this.#computeProperties();
    });
  }

  #computeProperties() {
    const { properties } = this.constructor;
    for (const [propertyName, { computed }] of Object.entries(properties)) {
      if (computed) this[propertyName] = this.#evaluateInContext(computed);
    }
  }

  #defineProperties() {
    const { observedAttributes, properties } = this.constructor;
    for (const [propertyName, config] of Object.entries(properties)) {
      this.#defineProperty(propertyName, config, observedAttributes);
    }
  }

  #defineProperty(propertyName, config, observedAttributes) {
    const attrName = Wrec.getAttributeName(propertyName);
    if (config.required && !this.hasAttribute(attrName)) {
      this.#throw(this, propertyName, "is a required attribute");
    }

    // Copy the property value to a private property.
    // The property is replaced below with Object.defineProperty.
    const value =
      observedAttributes.includes(propertyName) && this.hasAttribute(attrName)
        ? this.#typedAttribute(propertyName)
        : config.value || defaultForType(config.type);
    const privateName = "#" + propertyName;
    this[privateName] = value;

    if (config.computed) this.#registerComputedProperty(propertyName, config);

    Object.defineProperty(this, propertyName, {
      enumerable: true,
      get() {
        return this[privateName];
      },
      set(value) {
        const oldValue = this[privateName];
        if (value === oldValue) return;

        this[privateName] = value;

        // Update all computed properties that reference this property.
        let map = this.constructor["#propertyToComputedMap"];
        if (map) {
          const computes = map.get(propertyName) || [];
          for (const [computedName, expression] of computes) {
            this[computedName] = this.#evaluateInContext(expression);
          }
        }

        // If there is a matching attribute on the custom element,
        // update that attribute.
        if (this.hasAttribute(attrName)) {
          const oldValue = this.#typedAttribute(propertyName);
          if (value !== oldValue) updateAttribute(this, propertyName, value);
        }

        this.#react(propertyName);

        // If this property is bound to a parent web component property,
        // update that as well.
        map = this.propertyToParentPropertyMap;
        const parentProperty = map ? map.get(propertyName) : null;
        if (parentProperty) {
          const parent = this.getRootNode().host;
          parent.setAttribute(parentProperty, value);
        }

        this.#setFormValue(propertyName, value);

        if (config.dispatch) {
          this.dispatchEvent(
            new CustomEvent("change", {
              bubbles: true, // up DOM tree
              composed: true, // can pass through shadow DOM
              detail: { propertyName },
            })
          );
        }
      },
    });
  }

  // This inserts a dash before each uppercase letter
  // that is preceded by a lowercase letter or digit.
  static elementName() {
    return this.name.replace(/([a-z0-9])([A-Z])/g, "$1-$2").toLowerCase();
  }

  #evaluateAttributes(element) {
    const isWC = element.localName.includes("-");

    for (const attrName of element.getAttributeNames()) {
      const text = element.getAttribute(attrName);

      // If the attribute value is a single property reference,
      // configure two-way data binding.
      const propertyName = this.#propertyReferenceName(element, text);
      if (propertyName) {
        const value = this[propertyName];
        if (value === undefined) {
          this.#throwInvalidReference(element, attrName, propertyName);
        }

        element[propertyName] = value;
        if (attrName === "value") this.#bind(element, propertyName, attrName);

        // If the element is a web component,
        // save a mapping from the attribute name in this web component
        // to the property name in the parent web component.
        if (isWC) {
          let map = element.propertyToParentPropertyMap;
          if (!map) {
            map = new Map();
            element.propertyToParentPropertyMap = map;
          }
          map.set(attrName, propertyName);
        }
      }

      this.#registerPlaceholders(text, element, attrName);
    }
  }

  #evaluateInContext(expression) {
    // oxlint-disable-next-line no-eval
    return (() => eval(expression)).call(this);
  }

  #evaluateText(element) {
    const { localName } = element;

    if (localName === "style") {
      for (const rule of element.sheet.cssRules) {
        if (rule.type === CSSRule.STYLE_RULE) {
          for (const prop of rule.style) {
            if (prop.startsWith("--")) {
              const value = rule.style.getPropertyValue(prop);
              this.#registerPlaceholders(value, rule, prop);
            }
          }
        }
      }
    } else {
      const text = element.textContent.trim();
      // Only add a binding the element is a "textarea" and
      // its text content is a single property reference.
      const propertyName = this.#propertyReferenceName(element, text);
      if (localName === "textarea" && propertyName) {
        // Configure data binding.
        this.#bind(element, propertyName);
        element.textContent = this[propertyName];
      } else {
        this.#registerPlaceholders(text, element);
      }
    }
  }

  static getAttributeName(propertyName) {
    let attrName = Wrec.#propToAttrMap.get(propertyName);
    if (!attrName) {
      attrName = propertyName
        .replace(/([a-z0-9])([A-Z])/g, "$1-$2")
        .toLowerCase();
      Wrec.#propToAttrMap.set(propertyName, attrName);
    }
    return attrName;
  }

  static getPropertyName(attrName) {
    let propertyName = Wrec.#attrToPropMap.get(attrName);
    if (!propertyName) {
      propertyName = attrName.replace(/-([a-z])/g, (_, char) =>
        char.toUpperCase()
      );
      Wrec.#attrToPropMap.set(attrName, propertyName);
    }
    return propertyName;
  }

  #makeReactive(root) {
    const elements = root.querySelectorAll("*");
    for (const element of elements) {
      this.#evaluateAttributes(element);

      // If the element has no child elements, evaluate its text content.
      if (!element.firstElementChild) this.#evaluateText(element);
    }
    /* These lines are useful for debugging.
    console.log("#propertyToExpressionsMap =", this.constructor["#propertyToExpressionsMap"]);
    console.log(
      "#expressionToReferencesMap =",
      this.#expressionToReferencesMap
    );
    console.log("#propertyToComputedMap =", this.constructor["#propertyToComputedMap"]);
    */
  }

  static get observedAttributes() {
    return Object.keys(this.properties || {}).map(Wrec.getAttributeName);
  }

  #propertyReferenceName(element, text) {
    if (!REFERENCE_RE.test(text)) return;
    const propertyName = text.substring(SKIP);
    if (this[propertyName] === undefined) {
      this.#throwInvalidReference(element, null, propertyName);
    }
    return propertyName;
  }

  #react(propertyName) {
    // Update all expression references.
    const map = this.constructor["#propertyToExpressionsMap"];
    const expressions = map.get(propertyName) || [];
    for (const expression of expressions) {
      const value = this.#evaluateInContext(expression);
      const references = this.#expressionToReferencesMap.get(expression) || [];
      for (const reference of references) {
        if (reference instanceof Element) {
          this.#updateElementContent(reference, value);
        } else {
          updateValue(reference.element, reference.attrName, value);
        }
      }
    }

    // Wait for the DOM to update.
    requestAnimationFrame(() => {
      this.#updateBindings(propertyName);
    });
  }

  static register() {
    const elementName = this.elementName();
    if (!customElements.get(elementName)) {
      customElements.define(elementName, this);
    }
  }

  #registerComputedProperty(propertyName, config) {
    const { computed, uses } = config;

    let map = this.constructor["#propertyToComputedMap"];
    if (!map) map = this.constructor["#propertyToComputedMap"] = new Map();

    function register(referencedProperty, expression) {
      let computes = map.get(referencedProperty);
      if (!computes) {
        computes = [];
        map.set(referencedProperty, computes);
      }
      computes.push([propertyName, expression]);
    }

    const matches = computed.match(REFERENCES_RE) || [];
    for (const match of matches) {
      const referencedProperty = match.substring(SKIP);
      if (this[referencedProperty] === undefined) {
        this.#throwInvalidReference(null, propertyName, referencedProperty);
      }
      if (typeof this[referencedProperty] !== "function") {
        register(referencedProperty, computed);
      }
    }

    if (uses) {
      for (const use of uses.split(",")) {
        register(use, computed);
      }
    }
  }

  // Do not place untrusted expressions in
  // attribute values or the text content of elements!
  #registerPlaceholders(text, element, attrName) {
    const matches = this.#validateExpression(element, attrName, text);
    if (!matches) {
      const value = text.replaceAll("this..", "this.");
      if (attrName) {
        updateValue(element, attrName, value);
      } else {
        element.textContent = value;
      }
      return;
    }

    // Only map properties to expressions once for each web component because
    // the mapping will be the same for every instance of the web component.
    if (!this.constructor.processed) {
      matches.forEach((capture) => {
        const propertyName = capture.substring(SKIP);
        const map = this.constructor["#propertyToExpressionsMap"];
        let expressions = map.get(propertyName);
        if (!expressions) {
          expressions = [];
          map.set(propertyName, expressions);
        }
        expressions.push(text);
      });
    }

    let references = this.#expressionToReferencesMap.get(text);
    if (!references) {
      references = [];
      this.#expressionToReferencesMap.set(text, references);
    }
    references.push(attrName ? { element, attrName } : element);

    const value = this.#evaluateInContext(text);
    if (attrName) {
      updateValue(element, attrName, value);
    } else {
      this.#updateElementContent(element, value);
    }
  }

  #setFormValue(propertyName, value) {
    if (!this.#formData) return;
    this.#formData.set(propertyName, value);
    this.#internals.setFormValue(this.#formData);
  }

  #throw(element, attrName, message) {
    throw new Error(
      `component ${this.constructor.elementName()}` +
        (element ? `, element "${element.localName}"` : "") +
        (attrName ? `, attribute "${attrName}"` : "") +
        ` ${message}`
    );
  }

  #throwInvalidReference(element, attrName, propertyName) {
    this.#throw(
      element,
      attrName,
      `refers to missing property "${propertyName}"`
    );
  }

  #typedAttribute(attrName) {
    return this.#typedValue(attrName, this.getAttribute(attrName));
  }

  #typedValue(propertyName, stringValue) {
    if (stringValue?.match(REFERENCES_RE)) return stringValue;

    const { type } = this.constructor.properties[propertyName];
    if (type === String) return stringValue;
    if (type === Number) {
      const number = Number(stringValue);
      if (!isNaN(number)) return number;
      this.#throw(
        null,
        propertyName,
        `must be a number, but was "${stringValue}"`
      );
    }
    if (type === Boolean) {
      if (stringValue === "true") return true;
      if (stringValue === "false") return false;
      if (stringValue && stringValue !== propertyName) {
        this.#throw(
          null,
          propertyName,
          "is a Boolean attribute, so its value " +
            "must match attribute name or be missing"
        );
      }
      return stringValue === propertyName;
    }
    this.#throw(null, propertyName, "does not specify its type");
  }

  #updateBindings(propertyName) {
    const value = this[propertyName];
    const bindings = this.#propertyToBindingsMap.get(propertyName) || [];
    for (const binding of bindings) {
      if (binding instanceof Element) {
        if (binding.localName === "textarea") {
          binding.value = value;
        } else {
          binding.textContent = value;
        }
      } else {
        const { element, attrName } = binding;
        updateAttribute(element, attrName, value);
        element[attrName] = value;
      }
    }
  }

  #updateElementContent(element, value) {
    const { localName } = element;
    const t = typeof value;
    if (t !== "string" && t !== "number") {
      this.#throw(element, null, ` computed content is not a string or number`);
    }

    if (localName === "textarea") {
      element.value = value;
    } else if (t === "string" && value.trim().startsWith("<")) {
      element.innerHTML = value;
      this.#wireEvents(element);
      this.#makeReactive(element);
    } else {
      element.textContent = value;
    }
  }

  #validateAttributes() {
    const propertyNames = new Set(Object.keys(this.constructor.properties));
    for (const attrName of this.getAttributeNames()) {
      if (attrName === "id") continue;
      if (attrName.startsWith("on")) continue;
      if (!propertyNames.has(Wrec.getPropertyName(attrName))) {
        this.#throw(null, attrName, "is not a supported attribute");
      }
    }
  }

  #validateExpression(element, attrName, expression) {
    const matches = expression.match(REFERENCES_RE);
    if (!matches) return;

    matches.forEach((capture) => {
      const propertyName = capture.substring(SKIP);
      if (this[propertyName] === undefined) {
        this.#throwInvalidReference(element, attrName, propertyName);
      }
    });

    return matches;
  }

  #wireEvents(root) {
    for (const element of root.querySelectorAll("*")) {
      // We don't want to remove attributes while we are iterating over them.
      const attributesToRemove = [];

      for (const attr of element.attributes) {
        const attrName = attr.name;
        if (attrName.startsWith("on")) {
          const eventName = attrName.slice(2).toLowerCase();
          const attrValue = attr.value;
          this.#validateExpression(element, attrName, attrValue);

          let fn;
          if (typeof this[attrValue] === "function") {
            fn = (event) => this[attrValue](event);
          } else {
            this.#validateExpression(element, attrName, attrValue);
            // oxlint-disable-next-line no-eval no-unused-vars
            fn = (event) => eval(attrValue);
          }
          element.addEventListener(eventName, fn);
          attributesToRemove.push(attrName);
        }
      }

      for (const attrName of attributesToRemove) {
        element.removeAttribute(attrName);
      }
    }
  }
}

export default Wrec;

// These enable the VS Code extensions Prettier and es6-string-html to
// provide formatting and syntax highlighting of tagged template literals.
export const css = String.raw;
export const html = String.raw;

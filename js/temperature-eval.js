import Wrec, {html} from './wrec.min.js';

class TemperatureEval extends Wrec {
  static properties = {
    temperature: {type: Number}
  };

  static html = html`
    <p>this.temperature < 32 ? "freezing" : "not freezing"</p>
  `;
}

TemperatureEval.register();

import Wrec, {css, html} from './wrec.min.js';

class RectangleArea extends Wrec {
  static properties = {
    width: {type: Number, value: 10},
    height: {type: Number, value: 5},
    /*
    area: {
      type: Number,
      computed: "this.width * this.height",
    },
    area: {
      type: Number,
      computed: "this.rectangleArea(this.width, this.height)",
    },
    */
    area: {
      type: Number,
      computed: 'this.rectangleArea()',
      uses: 'width,height'
    }
  };

  static css = css`
    .area {
      font-weight: bold;
    }
  `;

  static html = html`
    <number-slider label="Width" value="this.width"></number-slider>
    <number-slider label="Height" value="this.height"></number-slider>
    <div class="area">Area: <span>this.area</span></div>
  `;

  /*
  rectangleArea(width, height) {
    return width * height;
  }
  */
  rectangleArea() {
    return this.width * this.height;
  }
}

RectangleArea.register();

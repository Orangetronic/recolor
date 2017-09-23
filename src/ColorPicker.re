type hsla   = (int, int, int, float);

type actions =
  | Hue int
  | Saturation int
  | Lightness int
  | Alpha float;

type state = {
  hue: int,
  saturation: int,
  lightness: int,
  alpha: float
};

let component = ReasonReact.reducerComponent "ColourPicker";
let getValueFromReactEvent e => (ReactDOMRe.domElementToObj (ReactEventRe.Form.target e))##value;

let hslaFromState state => (state.hue, state.saturation, state.lightness, state.alpha);

let hslaToString value::(value: hsla) : (string, string, string, string) => {
  let (h,s,l,a) = value;

  let h = Js.Int.toString   h;
  let s = Js.Int.toString   s;
  let l = Js.Int.toString   l;
  let a = Js.Float.toString a;

  (h,s,l,a);
};

let getHuePreviewStyle value::(value: hsla) => {
  let (_h,s,l,a) = hslaToString value::value;

  let rec gradBuilder = fun str index => {
    let indexStr      = Js.Int.toString index;
    let contents      = "hsla(" ^ indexStr ^ ", " ^ s ^ "%, " ^ l ^ "%, " ^ a ^ ")";
    let str           = str ^ contents;
    let index         = index + 15;
    index == 375 ? str ^ ")" : gradBuilder (str ^ ", ") index;
  };

  ReactDOMRe.Style.make background::(gradBuilder "linear-gradient(to right, " 0) ()
};

let getSaturationPreviewStyle value::(value: hsla) => {
  let (h,_s,l,a) = hslaToString value::value;
  let gradient = "linear-gradient(to right, hsla(" ^ h ^ ", 0%, " ^ l ^ "%, " ^ a ^ "), hsla(" ^ h ^ ", 100%, " ^ l ^ "%, " ^ a ^ "))";
  ReactDOMRe.Style.make backgroundImage::gradient ()
};

let getLightnessPreviewStyle value::(value: hsla) => {
  let (h,s,_l,a) = hslaToString value::value;
  let gradient = "linear-gradient(to right, hsla(" ^ h ^ ", " ^ s ^ "%, 0%, " ^ a ^ "), hsla(" ^ h ^ ", " ^ s ^ "%, 50%, " ^ a ^ "), hsla(" ^ h ^ ", " ^ s ^ "%, 100%, " ^ a ^ "))";
  ReactDOMRe.Style.make backgroundImage::gradient ()
};

let getAlphaPreviewStyle value::(value: hsla) => {
  let (h,s,l,_a) = hslaToString value::value;
  let gradient = "linear-gradient(to right, hsla(" ^ h ^ ", " ^ s ^ "%, " ^ l ^ "%, 0.0), hsla(" ^ h ^ ", " ^ s ^ "%, " ^ l ^ "%, 1.0 ) )";
  ReactDOMRe.Style.make backgroundImage::gradient ()
};

/* 😜 */
let echo = ReasonReact.stringToElement;

let make value::(value: hsla) _children => {

  ...component,

  initialState : fun () => {
    let (h,s,l,a) = value;
    {
      hue: h,
      saturation: s,
      lightness: l,
      alpha: a
    }
  },

  reducer: fun action state => {

    switch action {
    | Hue        newVal => ReasonReact.Update { ...state, hue : newVal }
    | Saturation newVal => ReasonReact.Update { ...state, saturation: newVal }
    | Lightness  newVal => ReasonReact.Update { ...state, lightness: newVal }
    | Alpha      newVal => ReasonReact.Update { ...state, alpha: newVal }
    };

  },

  render: fun self => {

    let hslaVal = hslaFromState self.state;

    let (h,s,l,a) = hslaToString value::hslaVal;

    let handleHue        e => Hue        (getValueFromReactEvent e);
    let handleSaturation e => Saturation (getValueFromReactEvent e);
    let handleLightness  e => Lightness  (getValueFromReactEvent e);
    let handleAlpha      e => Alpha      (getValueFromReactEvent e);

    let values = "hsla(" ^ h ^ ", " ^ s ^ "%, " ^ l ^ "%, " ^ a ^ ")";

    let huePreviewStyle = getHuePreviewStyle value:: hslaVal; 

    let saturationPreviewStyle = getSaturationPreviewStyle value::hslaVal;
    let lightnessPreviewStyle  = getLightnessPreviewStyle  value::hslaVal;
    let alphaPreviewStyle      = getAlphaPreviewStyle      value::hslaVal;
    
    <div className="color-picker">
      <div className="hue-group input-group">
        <label htmlFor="hue">(echo "hue:")</label>
        <input
          id="hue"
          min=0
          max="360"
          _type="range" 
          value=h
          onChange=(self.reduce handleHue)
        />
        <div className="preview" style=huePreviewStyle />
      </div>
      <div className="saturation-group input-group">
        <label htmlFor="saturation">(echo "saturation:")</label>
        <input
          id="saturation"
          value=s
          _type="range" 
          onChange=(self.reduce handleSaturation)
        />
        <div className="preview" style=saturationPreviewStyle />
      </div>
      <div className="lightness-group input-group">
        <label htmlFor="lightness">(echo "lightness:")</label>
        <input
          id="lightness"
          value=l
          _type="range" 
          onChange=(self.reduce handleLightness)
        />
        <div className="preview" style=lightnessPreviewStyle />
      </div>
      <div className="alpha-group input-group">
        <label htmlFor="alpha">(echo "alpha:")</label>
        <input
          id="alpha"
          value=a
          min=0
          max="1"
          step=0.01
          _type="range" 
          onChange=(self.reduce handleAlpha)
        />
        <div className="preview" style=alphaPreviewStyle />
      </div>
      <div>(echo values)</div>

      <Swatch value=hslaVal size=350 />
    </div>
  }

};
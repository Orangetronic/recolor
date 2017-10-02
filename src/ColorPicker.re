type hsla   = (int, int, int, float);

type actions =
  | Hex Color.hex
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

external parseInt : 'a => int = "parseInt" [@@bs.val];
let getIntValueFromReactEvent e => parseInt (getValueFromReactEvent e);


/* 😜 */
let echo = ReasonReact.stringToElement;

let hslaFromState (state: state) : hsla => (state.hue, state.saturation, state.lightness, state.alpha);

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

type rgb  = Color.rgb;
type hsl  = Color.hsl;


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
    | Hex        newVal => 
      let rgb = Color.hexToRGB newVal;
      let (h, s, l) = Color.rgbToHSL rgb;
      ReasonReact.Update { hue: h, saturation: s, lightness: l, alpha: 1.0 }
    | Hue        newVal => ReasonReact.Update { ...state, hue : newVal }
    | Saturation newVal => ReasonReact.Update { ...state, saturation: newVal }
    | Lightness  newVal => ReasonReact.Update { ...state, lightness: newVal }
    | Alpha      newVal => ReasonReact.Update { ...state, alpha: newVal }
    };

  },

  render: fun self => {

    
    let hslaVal  = hslaFromState self.state;

    let (h,s,l, _) = hslaVal;
    
    Js.log "hsl to rgb";
    Js.log (Color.hslToRGB (h,s,l));

    let (h,s,l,a) = hslaToString value::hslaVal;

    let handleHex        e => Hex        (getValueFromReactEvent e);
    let handleHue        e => Hue        (getIntValueFromReactEvent e);
    let handleSaturation e => Saturation (getIntValueFromReactEvent e);
    let handleLightness  e => Lightness  (getIntValueFromReactEvent e);
    let handleAlpha      e => Alpha      (getValueFromReactEvent e);

    let values = "hsla(" ^ h ^ ", " ^ s ^ "%, " ^ l ^ "%, " ^ a ^ ")";

    let huePreviewStyle = getHuePreviewStyle value:: hslaVal; 

    let saturationPreviewStyle = getSaturationPreviewStyle value::hslaVal;
    let lightnessPreviewStyle  = getLightnessPreviewStyle  value::hslaVal;
    let alphaPreviewStyle      = getAlphaPreviewStyle      value::hslaVal;

    
    <div className="color-picker">
    <div className="hex-group input-group">
        <label htmlFor="hex">(echo "hex:")</label>
        <input
          id="hex"
          _type="text"
          onChange=(self.reduce handleHex)
        />
      </div>
      <div className="hue-group input-group">
        <label htmlFor="hue">(echo "hue:")</label>
        <input
          id="hue"
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
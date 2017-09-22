type rgba   = (int, int, int, float);
type hsla   = (int, int, int, float);

type format = RGBA | HSLA;

/* 

let rgbaTohsla color::(color : rgba) : hsla => {

  let (r,g,b,a) = color;

  let l = (r + g + b) / 3;

  let h = 120;

  let s = 50;

  (h,s,l,a);

}; */

/* 
let hslaTorgba color::(color : hsla) : rgba => {
  
    let (h,s,l,a) = color;
  
    let r = 162;
  
    let g = 120;
  
    let b = 50;
  
    (r,g,b,a);
  
  };
 */


/* let hslaTorgba = func ::hsla => {

}; */




let component = ReasonReact.statelessComponent "ColourPicker";

let make value::(value: hsla) _children => {
  ...component,
  render: fun _self => {

    let (h,s,l,a) = value;

    let h = Js.Int.toString h;
    let s = Js.Int.toString s;
    let l = Js.Int.toString l;
    let a = Js.Float.toString a;
  
    <div className="color-picker">
      (ReasonReact.stringToElement ("whuuuu: " ^ h ^ " " ^ s ^ " " ^ l ^ " " ^ a ^ " …yah"))
      <Swatch value=value size=35 />
    </div>
  }

};





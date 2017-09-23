[%bs.raw {|require('./app.css')|}];

type rgba   = (int, int, int, float);
type hsla   = (int, int, int, float);

let intToPercent i => Js.Int.toString i ^ "%";

let component = ReasonReact.statelessComponent "Swatch";

let make value::(value: hsla) size::(size: int)=15 _children => {
  ...component,
  render: fun _self => {

    let (h,s,l,a) = value;

    let color = "hsla("
      ^ Js.Int.toString h ^ ", "
      ^ intToPercent s ^ ", "
      ^ intToPercent l ^ ", "
      ^ Js.Float.toString a ^ ")";

    let size = Js.Int.toString size ^ "px";

    let style = (ReactDOMRe.Style.make
      backgroundColor::color 
      width::size
      height::size
      ()
    );

    <div className="swatch" style=style />

  }

};
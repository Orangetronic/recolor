[%bs.raw {|require('./app.css')|}];

external logo : string = "./logo.svg" [@@bs.module];

let component = ReasonReact.statelessComponent "App";

type hsla   = (int, int, int, float);

let st = ReasonReact.stringToElement;

let make _children => {
  ...component,
  render: fun _self => {

    let color: hsla = (50,70,12,0.5);

    let message = {|
      Yo it me the long string
    |};
  
    <div className="App">
      <div className="App-header">
        <h2> (st message) </h2>
      </div>
      <div className="center-me">
        <ColorPicker value=color />
      </div>
    </div>
  }

};



type hsla = (int, int, int, float);
type rgba = (int, int, int, float);
type cmyk = (int, int, int, int);
type rgb  = (int, int, int);
type hsl  = (int, int, int);
type hex  = string;

type color = 
  | HSLA hsla
  | RGBA rgba
  | CMYK cmyk
  | RGB  rgb
  | HSL  hsl
  | Hex  hex;


let hexCharToInt (hexChar: char) : int => {
  switch hexChar {
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | 'A' => 10
  | 'B' => 11
  | 'C' => 12
  | 'D' => 13
  | 'E' => 14
  | 'F' => 15
  | 'a' => 10
  | 'b' => 11
  | 'c' => 12
  | 'd' => 13
  | 'e' => 14
  | 'f' => 15
  | _ => 0
  }
};

let rec parseHex ::multiplier=1 ::accumulator=0 (value: string) : int => {

  let len          = String.length value;
  let char         = String.get value (len - 1);
  let remain       = String.sub value 0 (len - 1);
  let remainLength = String.length remain;
  let charVal      = hexCharToInt char;
  let a            = accumulator + charVal * multiplier;
  let m            = multiplier * 16;  
  let result       = remainLength > 0 ? parseHex remain multiplier::m accumulator::a : a;
 
  result
};


let hexToRGB (value: hex) : rgb => {
  let len   = String.length value;
  let value = len > 0 && (String.get value 0) == '#' ? String.sub value 1 (len - 1) : value;
  let len   = String.length value;

  let (rStr, gStr, bStr) = switch len {
  | 6 => ((String.sub value 0 2), (String.sub value 2 2), (String.sub value 4 2))
  | 3 => (
            (String.sub value 0 1) ^ (String.sub value 0 1),
            (String.sub value 1 1) ^ (String.sub value 1 1), 
            (String.sub value 2 1) ^ (String.sub value 2 1)
        )
  | _ => ("0", "0", "0")
  };

  let (r,g,b) = (
    parseHex rStr,
    parseHex gStr,
    parseHex bStr
  );

  (r,g,b)
};

let minMaxFromRgb (rgb: rgb) : (int, int) => {
  let (r,g,b) = rgb;
  let arr = [|r, g, b|];

  let sortFunc (a: int) (b: int) => a - b;
  
  Array.sort sortFunc arr;

  (arr.(0), arr.(2))
};

let rangeTo100 (value: int) (base: int) : int => {
  /* scale up — we're operating on ints */
  let v = value * 1000;
  let v = v / base;
  let v = v * 100;
  /* scale back down again */
  let v = v / 1000;
  v
};

let rangeTo255 (value: int) (base: int) : int => {
  /* scale up — we're operating on ints */
  let v = value * 1000;
  let v = v / base;
  let v = v * 255;
  /* scale back down again */
  let v = v / 1000;
  v
};


let rgbToHSL (rgb: rgb) : hsl => {

  let (r, g, b)  = rgb;

  let r = rangeTo100 r 255;
  let g = rangeTo100 g 255;
  let b = rangeTo100 b 255;

  let (min, max) = minMaxFromRgb (r,g,b);

  let d = max - min;

  let l = (min + max ) / 2;
  
  let s = min == max ? 0 : l < 50 ? 
    (100 * d) / (min + max) :
    (100 * d) / (100 - Math.modulus (2 * l - 100)); 

  let rdeg = r * 360;
  let gdeg = g * 360;
  let bdeg = b * 360;
  let ddeg = d * 360;

  let h = min == max ? 0 : (switch max {
  | r => 60 * (gdeg - bdeg) / ddeg + (g < b ? 360 : 0)
  | g => 60 * (bdeg - rdeg) / ddeg + 2
  | b => 60 * (rdeg - gdeg) / ddeg + 4
  });

  (h, s, l)
};

let hue2rgb p q t => {

  /* Js.log "p q t";
  Js.log p;
  Js.log q;
  Js.log t; */


  let t =
    if      (t < 0)   { t + 100 }
    else if (t > 100) { t - 100 }
    else              { t };

  /* Js.log "t";
  Js.log t; */

  let result =
    if      (t < 120) { p + (q - t) * 6 * t }
    else if (t < 180) { q }
    else if (t < 270) { p + (q - p) * (270 - t) * 6 }
    else              { p };

  /* Js.log "result";
  Js.log result; */

  result
};
let hslToRGB (hsl : hsl) : rgb => {

  let (h, s , l) = hsl;

  let mono = s == 0;

  let rgb = switch mono {
  | true =>
    let n = rangeTo255 l 100;
    (n,n,n)
  | false =>

    let c = (100 - (Math.modulus (2 * l - 100))) * s / 100;

    let x = c * (100 - (Math.modulus (h * 100 / 60 mod 200 - 100))) / 100;
    let m = l - c / 2;

    let rgb =
      if      (h < 60)  {(c, x, 0)}
      else if (h < 120) {(x, c, 0)}
      else if (h < 180) {(0, c, x)}
      else if (h < 240) {(0, x, c)}
      else if (h < 300) {(x, 0, c)}
      else              {(c, 0, x)};

    let (r,g,b) = rgb;
    let (r,g,b) = (r + m, g + m, b + m);
    let (r,g,b) = (
      r * 255 / 100, 
      g * 255 / 100,
      b * 255 / 100
    );

    (r, g, b)
  };

  rgb
};


/* 
  hslToRgb(h, s, l){
    var r, g, b;

    if(s == 0){
        r = g = b = l; // achromatic
    }else{
        var hue2rgb = function hue2rgb(p, q, t){
            if(t < 0) t += 1;
            if(t > 1) t -= 1;
            if(t < 1/6) return p + (q - p) * 6 * t;
            if(t < 1/2) return q;
            if(t < 2/3) return p + (q - p) * (2/3 - t) * 6;
            return p;
        }

        var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
        var p = 2 * l - q;
        r = hue2rgb(p, q, h + 1/3);
        g = hue2rgb(p, q, h);
        b = hue2rgb(p, q, h - 1/3);
    }

    return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)]; */
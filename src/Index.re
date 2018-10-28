[@bs.deriving abstract]
type canvas = {
  mutable width: int,
  mutable height: int,
};

[@bs.deriving abstract]
type context = {
  mutable imageSmoothingQuality: string,
  mutable imageSmoothingEnabled: bool,
  mutable lineWidth: float,
  mutable strokeStyle: string,
  mutable fillStyle: string,
};

[@bs.send] external beginPath: context => unit = "";
[@bs.send] external moveTo: (context, float, float) => unit = "";
[@bs.send] external lineTo: (context, float, float) => unit = "";
[@bs.send] external stroke: context => unit = "";
[@bs.send] external closePath: context => unit = "";
[@bs.send] external fill: context => unit = "";
[@bs.send] external fillRect: (context, float, float, int, int) => unit = "";
[@bs.send]
external arc: (context, float, float, float, float, float, bool) => unit = "";

type config = {
  play: bool,
  radius: option(float),
  step: float,
  roundNumbers: bool,
  drawLineRadius: bool,
  drawLineXAxis: bool,
  drawLineYAxis: bool,
  drawLineSin: bool,
  drawLineCos: bool,
  drawLineTan: bool,
  drawLineCot: bool,
  drawLineSec: bool,
  drawLineCsc: bool,
  drawFullNames: bool,
  drawNameRadius: bool,
  drawNameTheta: bool,
  drawNameFPS: bool,
  drawNameInfo: bool,
  drawNameCredits: bool,
  drawNameSin: bool,
  drawNameCos: bool,
  drawNameTan: bool,
  drawNameCot: bool,
  drawNameSec: bool,
  drawNameCsc: bool,
};

let cfg: config = {
  play: true,
  radius: None,
  step: 0.03,
  roundNumbers: true,
  drawLineRadius: true,
  drawLineXAxis: true,
  drawLineYAxis: true,
  drawLineSin: true,
  drawLineCos: true,
  drawLineTan: true,
  drawLineCot: true,
  drawLineSec: true,
  drawLineCsc: true,
  drawFullNames: true,
  drawNameRadius: true,
  drawNameTheta: true,
  drawNameFPS: true,
  drawNameInfo: true,
  drawNameCredits: true,
  drawNameSin: true,
  drawNameCos: true,
  drawNameTan: true,
  drawNameCot: true,
  drawNameSec: true,
  drawNameCsc: true,
};

[@bs.deriving abstract]
type contextOptions = {alpha: bool};

[@bs.val] [@bs.scope "document"]
external getCanvasById: string => canvas = "getElementById";

[@bs.send]
external getContext: (canvas, string, contextOptions) => context = "";

let tPI = 2. *. Js_math._PI;

let drawLine =
    (~fromX, ~fromY, ~toX, ~toY, ~color, ~lineWidth=1., ctx: context) => {
  lineWidthSet(ctx, lineWidth);
  strokeStyleSet(ctx, color);
  beginPath(ctx);
  moveTo(ctx, fromX, fromY);
  lineTo(ctx, toX, toY);
  stroke(ctx);
};

let drawCircle =
    (
      ~x,
      ~y,
      ~r,
      ~color,
      ~shouldFill: bool,
      ~startAngle=0.,
      ~endAngle=tPI,
      ~aCW=true,
      ~lineWidth=1.5,
      ctx: context,
    ) => {
  lineWidthSet(ctx, lineWidth);
  strokeStyleSet(ctx, color);
  fillStyleSet(ctx, color);
  moveTo(ctx, x, y);
  beginPath(ctx);
  if (shouldFill) {
    moveTo(ctx, x, y);
  };
  arc(ctx, x, y, r, startAngle, endAngle, aCW);
  if (shouldFill) {
    closePath(ctx);
    fill(ctx);
  } else {
    stroke(ctx);
  };
};

/* Math related functions */
type radian = float;
type degree = float;
let degToRad = (deg: degree): radian => deg /. 360. *. tPI;

let findQuadrant = (sinus: float, cosinus: float): int => {
  let sinGreaterThanZero = sinus > 0.;
  let cosGreaterThanZero = cosinus > 0.;

  switch (sinGreaterThanZero, cosGreaterThanZero) {
  | (false, true) => 1
  | (false, false) => 2
  | (true, false) => 3
  | (true, true) => 4
  };
};

/* Document related values */
[@bs.val] [@bs.scope ("document", "body")] external clientWidth: int = "";
[@bs.val] [@bs.scope ("document", "body")] external clientHeight: int = "";
[@bs.val] [@bs.scope "window"]
external requestAnimationFrame: (float => unit) => unit = "";

let resize = (canvas: canvas) =>
  if (widthGet(canvas) !== clientWidth && heightGet(canvas) !== clientHeight) {
    widthSet(canvas, clientWidth);
    heightSet(canvas, clientHeight);
  };

let draw = (canvas: canvas, ctx: context) => {
  let w = widthGet(canvas);
  let h = heightGet(canvas);

  let x = float_of_int(w) /. 2.;
  let y = float_of_int(w) /. 2.;
  let r =
    switch (cfg.radius) {
    | Some(r) => r
    | None => float_of_int(min(w, h)) /. 3.
    };

  /* Paint the canvas white */
  fillStyleSet(ctx, Colors.white);
  fillRect(ctx, 0., 0., w, h);

  /* Draw the main circle */
  drawCircle(~x, ~y, ~r, ~color=Colors.gray, ~shouldFill=false, ctx);

  /* Draw X axis */
  drawLine(
    ~fromX=0.,
    ~fromY=y,
    ~toX=float_of_int(w),
    ~toY=y,
    ~color=Colors.gray,
    ctx,
  );

  /* Draw Y axis */
  drawLine(
    ~fromX=x,
    ~fromY=0.,
    ~toX=x,
    ~toY=float_of_int(h),
    ~color=Colors.gray,
    ctx,
  );

  let deg: degree = 45.;
  let degInRad: radian = degToRad(deg);
  let sinus = -. sin(degInRad);
  let cosinus = cos(degInRad);

  let quadrant = findQuadrant(sinus, cosinus);
  let isEvenQuad = quadrant mod 2 > 0;

  let lineX = x +. cosinus *. r;
  let lineY = y +. sinus *. r;
  let sinHeight = y -. lineY;
  let cosWidth = lineX -. x;
  let coDegree =
    isEvenQuad ? 90. -. mod_float(deg, 90.) : mod_float(deg, 90.);

  let tanOfCoDegree = tan(degToRad(coDegree));
  let tanDistance = sinHeight /. tanOfCoDegree;
  let cotDistance = cosWidth /. (1. /. tanOfCoDegree);

  let tanX = isEvenQuad ? lineX +. tanDistance : lineX -. tanDistance;
  let cotY = isEvenQuad ? lineY -. cotDistance : lineY +. cotDistance;

  /* Draw sin */
  drawLine(
    ~fromX=lineX,
    ~fromY=lineY,
    ~toX=lineX,
    ~toY=y,
    ~color=Colors.purple,
    ctx,
  );

  /* Draw cos */
  drawLine(
    ~fromX=lineX,
    ~fromY=lineY,
    ~toX=x,
    ~toY=lineY,
    ~color=Colors.green,
    ctx,
  );

  /* Draw tan */
  drawLine(
    ~fromX=tanX,
    ~fromY=y,
    ~toX=lineX,
    ~toY=lineY,
    ~color=Colors.orange,
    ctx,
  );

  /* Draw cotan */
  drawLine(
    ~fromX=lineX,
    ~fromY=lineY,
    ~toX=x,
    ~toY=cotY,
    ~color=Colors.pink,
    ctx,
  );
};

let run = () => {
  Js.log("Start Trigonoparty");

  let canvas = getCanvasById("js-canvas");
  let ctx = getContext(canvas, "2d", contextOptions(~alpha=false));
  imageSmoothingEnabledSet(ctx, true);
  imageSmoothingQualitySet(ctx, "high");
  resize(canvas);
  draw(canvas, ctx);
};

run();
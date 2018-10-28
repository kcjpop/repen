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
  mutable font: string,
  mutable textBaseLine: string, /* Should be an enum? */
  mutable textAlign: string,
};

[@bs.deriving abstract]
type contextOptions = {alpha: bool};

[@bs.send]
external arc: (context, float, float, float, float, float, bool) => unit = "";
[@bs.send] external beginPath: context => unit = "";
[@bs.send] external closePath: context => unit = "";
[@bs.send] external fill: context => unit = "";
[@bs.send] external fillRect: (context, float, float, int, int) => unit = "";
[@bs.send] external fillText: (context, string, float, float) => unit = "";
[@bs.send] external moveTo: (context, float, float) => unit = "";
[@bs.send] external lineTo: (context, float, float) => unit = "";
[@bs.send] external restore: context => unit = "";
[@bs.send] external rotate: (context, float) => unit = "";
[@bs.send] external stroke: context => unit = "";
[@bs.send] external save: context => unit = "";
[@bs.send] external translate: (context, float, float) => unit = "";

[@bs.send]
external getContext: (canvas, string, contextOptions) => context = "";

/* Math related functions */
type radian = float;
type degree = float;

let tPI = 2. *. Js_math._PI;
let degToRad = (deg: degree): radian => deg /. 360. *. tPI;

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

let withAngle = (ctx, x, y, angle: degree, fn) => {
  save(ctx);
  translate(ctx, x, y);
  rotate(ctx, degToRad(angle));
  fn(ctx);
  restore(ctx);
};

let rec drawText =
        (
          ~x,
          ~y,
          ~text,
          ~color=Colors.gray,
          ~size=14,
          ~angle=0.,
          ~align="center",
          ~baseline="bottom",
          ctx: context,
        ) =>
  if (angle !== 0.) {
    withAngle(
      ctx,
      x,
      y,
      angle,
      drawText(
        ~x=0.,
        ~y=0.,
        ~text,
        ~color,
        ~size,
        ~angle=0.,
        ~align,
        ~baseline,
      ),
    );
  } else {
    fontSet(ctx, string_of_int(size) ++ "px sans-serif");
    textBaseLineSet(ctx, baseline);
    textAlignSet(ctx, align);
    fillStyleSet(ctx, color);
    fillText(ctx, text, x, y);
  };
open Canvas;

type config = {radius: option(float)};
let cfg = {radius: None};

[@bs.val] [@bs.scope "document"]
external getCanvasById: string => canvas = "getElementById";

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

let labelPadding = 5.;

let resize = (canvas: canvas) =>
  if (widthGet(canvas) !== clientWidth && heightGet(canvas) !== clientHeight) {
    widthSet(canvas, clientWidth);
    heightSet(canvas, clientHeight);
  };

let draw = (canvas: canvas, ctx: context) => {
  let w = widthGet(canvas);
  let h = heightGet(canvas);

  let x = float_of_int(w) /. 2.;
  let y = float_of_int(h) /. 2.;
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
  let tanCotAngle = isEvenQuad ? coDegree : -. coDegree;

  /* Draw sin */
  drawLine(
    ~fromX=lineX,
    ~fromY=lineY,
    ~toX=lineX,
    ~toY=y,
    ~color=Colors.purple,
    ctx,
  );

  drawText(
    ~x=lineX +. labelPadding,
    ~y=y +. (lineY -. y) /. 2.,
    ~text="sin",
    ~color=Colors.purple,
    ~angle=90.,
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

  drawText(
    ~x=x +. (lineX -. x) /. 2.,
    ~y=lineY -. labelPadding,
    ~text="cos",
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

  drawText(
    ~x=lineX +. (tanX -. lineX) /. 2.,
    ~y=y -. labelPadding +. (lineY -. y) /. 2.,
    ~text="tan",
    ~color=Colors.orange,
    ~angle=tanCotAngle,
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

  drawText(
    ~x=x +. (lineX -. x) /. 2.,
    ~y=lineY -. labelPadding +. (cotY -. lineY) /. 2.,
    ~text="cotan",
    ~color=Colors.pink,
    ~angle=tanCotAngle,
    ctx,
  );

  /* Draw secant */
  drawLine(~fromX=tanX, ~fromY=y, ~toX=x, ~toY=y, ~color=Colors.blue, ctx);

  drawText(
    ~x=x +. (tanX -. x) /. 2.,
    ~y=y +. labelPadding *. 3.,
    ~text="secant",
    ~color=Colors.blue,
    ctx,
  );

  /* Draw cosecant */
  drawLine(~fromX=x, ~fromY=cotY, ~toX=x, ~toY=y, ~color=Colors.cyan, ctx);
  drawText(
    ~x=x -. labelPadding *. 3.,
    ~y=y +. (cotY -. y) /. 2.,
    ~text="cosecant",
    ~angle=90.,
    ~color=Colors.cyan,
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
open Canvas;

type config = {
  radius: option(float),
  step: float,
  degree,
};

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

let labelPadding = 5.;

let rec draw = (cfg: config, canvas: canvas, ctx: context) => {
  let w = widthGet(canvas);
  let h = heightGet(canvas);

  clearRect(ctx, 0, 0, w, h);

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

  let deg: degree = cfg.degree;
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

  /* Draw right angle notations */
  let rightAngleSquareSize = 12.;
  let rightAngleSquareHeight =
    quadrant > 2 ? -. rightAngleSquareSize : rightAngleSquareSize;
  let drawRightAngle =
    drawRect(
      ~x=lineX,
      ~y=lineY,
      ~h=rightAngleSquareHeight,
      ~color=Colors.gray,
      ~angle=tanCotAngle,
      ~lineWidth=1.,
      ctx,
    );

  drawRightAngle(~w=rightAngleSquareSize);
  drawRightAngle(~w=-. rightAngleSquareSize);

  /* Draw theta */
  let drawTheta =
    drawCircle(~x, ~y, ~r=20., ~endAngle=degToRad(360. -. deg), ctx);
  drawTheta(~color=Colors.greenLight, ~shouldFill=true);
  drawTheta(~color=Colors.greenDark, ~shouldFill=false);

  drawText(
    ~x=x +. labelPadding *. 5.,
    ~y=y -. labelPadding *. 2.,
    ~text={js|θ|js},
    ~color=Colors.greenDark,
    ~align="left",
    ctx,
  );

  /* Draw radius */
  drawLine(
    ~fromX=x,
    ~fromY=y,
    ~toX=lineX,
    ~toY=lineY,
    ~color=Colors.night,
    ctx,
  );
  /* FIXME: Radius label being overlapped */
  drawText(
    ~x=x +. (lineX -. x) /. 2.,
    ~y=y +. (lineY -. y) /. 2. -. labelPadding,
    ~text="radius",
    ~color=Colors.night,
    ~angle=-. deg,
    ctx,
  );

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

  /* Draw origin point and radius' endpoint */
  drawCircle(~x, ~y, ~r=3., ~color=Colors.night, ~shouldFill=true, ctx);
  drawCircle(
    ~x=lineX,
    ~y=lineY,
    ~r=3.,
    ~color=Colors.night,
    ~shouldFill=true,
    ctx,
  );

  /* Show FPS */
  drawText(
    ~x=100.,
    ~y=50.,
    ~color=Colors.night,
    ~text=
      "FPS: " ++ Js.Float.toFixedWithPrecision(~digits=0, FPS.calculate()),
    ctx,
  );

  Util.Dom.requestAnimationFrame(_ts => {
    let degree =
      switch (deg +. cfg.step) {
      | v when v > 360. => mod_float(v, 360.)
      | v when v < 0. => v +. 360.
      | v => v
      };

    draw({...cfg, degree}, canvas, ctx);
  });
};

let run = (~canvas: Canvas.t, ~ctx) => {
  let initConfig: config = {radius: None, step: 0.2, degree: 1.};

  draw(initConfig, canvas, ctx);
};
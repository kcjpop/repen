open Canvas;
type rgbaColor = (int, int, int, float);

type ball = {
  x: float,
  y: float,
  vx: float,
  vy: float,
  r: float,
  alpha: float,
  phase: float,
};

type direction =
  | Top
  | Right
  | Bottom
  | Left;

type config = {
  ballColor: rgbaColor,
  distanceLimit: float,
  lineWidth: float,
  alphaChange: float,
  totalBalls: int,
};

let cfg = {
  ballColor: (207, 255, 4, 1.0),
  distanceLimit: 260.,
  lineWidth: 0.8,
  alphaChange: 0.03,
  totalBalls: 25,
};

let toFloat = Util.toFloat;
let randomFloat = Util.Random.float;
let randomInt = Util.Random.int;

let minSpeed = (-1.0);
let maxSpeed = 1.0;
let makeRandomSpeed =
  fun
  | Top => (randomFloat(minSpeed, maxSpeed), randomFloat(0.1, maxSpeed))
  | Right => (randomFloat(minSpeed, -0.1), randomFloat(minSpeed, maxSpeed))
  | Bottom => (randomFloat(minSpeed, maxSpeed), randomFloat(minSpeed, -0.1))
  | Left => (randomFloat(0.1, maxSpeed), randomFloat(minSpeed, maxSpeed));

let getRandomDirection = () => {
  switch (randomInt(0, 4)) {
  | 0 => Top
  | 1 => Right
  | 2 => Bottom
  | _ => Left
  };
};

let getRandomPositionFromDirection = (~r, ~xRange, ~yRange, direction) => {
  switch (direction) {
    | Top => (randomFloat(0., xRange), -. r)
    | Right => (xRange +. r, randomFloat(0., yRange))
    | Bottom => (randomFloat(0., xRange), yRange +. r)
    | Left => (-. r, randomFloat(0., yRange))
    };
};

let makeBalls =
    (
      ~direction=getRandomDirection(),
      ~init=false,
      ~n: int,
      ~xRange: float,
      ~yRange: float,
      (),
    ) =>
  Tablecloth.List.initialize(
    n,
    _ => {
      let r = 2.0;
      let (vx, vy) = makeRandomSpeed(direction);
      let (x, y) = init ? (
        randomFloat(0., xRange),
        randomFloat(0., yRange)
      ) : getRandomPositionFromDirection(~r, ~xRange, ~yRange, direction);

      {x, y, vx, vy, r, alpha: 1.0, phase: randomFloat(0., 10.)};
    },
  );

let renderBalls = (ctx, balls) => {
  let renderBall = (ctx, ball) => {
    let (r, g, b, a) = cfg.ballColor;
    fillStyleSet(ctx, {j|rgba($r, $g, $b, $a)|j});
    beginPath(ctx);
    arc(ctx, ball.x, ball.y, ball.r, 0., Js_math._PI *. 2., true);
    closePath(ctx);
    fill(ctx);
  };

  Tablecloth.List.iter(~f=renderBall(ctx), balls);
};

let updateBalls = (balls, canvasWidth, canvasHeight) => {
  let newBall = ball => {
    let x = ball.x +. ball.vx;
    let y = ball.y +. ball.vy;
    let phase = ball.phase +. cfg.alphaChange;
    let alpha = Js.Math.abs_float(Js.Math.cos(phase));

    {x, y, vx: ball.vx, vy: ball.vy, r: ball.r, alpha, phase};
  };

  let outOfBound = ball =>
    ball.x > (-50.)
    && ball.x < canvasWidth
    +. 50.
    && ball.y > (-50.)
    && ball.y < canvasHeight
    +. 50.;

  balls
  ->Tablecloth.List.map(~f=newBall)
  ->Tablecloth.List.filter(~f=outOfBound);
};

let getDistance = (a, b) => {
  let deltaX = Js.Math.abs_float(a.x -. b.x);
  let deltaY = Js.Math.abs_float(a.y -. b.y);

  Js.Math.sqrt(deltaX *. deltaX -. deltaY *. deltaY);
};

let renderLines = (ctx, balls) => {
  let toPairOfBalls = (el, balls) =>
    Tablecloth.List.map(~f=ball => (el, ball), balls);

  let pairs =
    Tablecloth.List.map(~f=ball => toPairOfBalls(ball, balls), balls)
    ->List.flatten;

  let renderLine = (ctx, (a, b)) => {
    let fraction = getDistance(a, b) /. cfg.distanceLimit;
    if (fraction < 1.) {
      let alpha = 1. -. fraction;
      strokeStyleSet(ctx, {j|rgba(150, 150, 150, $alpha)|j});
      lineWidthSet(ctx, cfg.lineWidth);
      ctx->beginPath;
      moveTo(ctx, a.x, a.y);
      lineTo(ctx, b.x, b.y);
      stroke(ctx);
      ctx->closePath;
    };
  };

  Tablecloth.List.iter(~f=pair => renderLine(ctx, pair), pairs);
};

let complementBalls = (balls, xRange, yRange) => {
  let missing = cfg.totalBalls - Tablecloth.List.length(balls);
  if (missing > 0) {
    balls @ makeBalls(~n=missing, ~xRange, ~yRange, ());
  } else {
    balls;
  };
};

let rec render = (canvas, ctx, balls) => {
  let canvasWidth = canvas->widthGet;
  let canvasHeight = canvas->heightGet;

  clearRect(ctx, 0, 0, canvasWidth, canvasHeight);
  fillStyleSet(ctx, "#111");
  fillRect(ctx, 0., 0., canvasWidth, canvasHeight );

  ctx->renderBalls(balls);
  ctx->renderLines(balls);
  let updatedBalls =
    balls
    ->updateBalls(canvasWidth->toFloat, canvasHeight->toFloat)
    ->complementBalls(canvasWidth->toFloat, canvasHeight->toFloat);

  Util.Dom.requestAnimationFrame(_ => render(canvas, ctx, updatedBalls));
};

let run = (~canvas: Canvas.t, ~ctx: Canvas.context) => {
  let xRange = canvas->widthGet;
  let yRange = canvas->heightGet;

  let balls: list(ball) =
    makeBalls(
      ~direction=Top,
      ~n=cfg.totalBalls,
      ~xRange=xRange->toFloat,
      ~yRange=yRange->toFloat,
      ~init=true,
      (),
    );

  render(canvas, ctx, balls);
};

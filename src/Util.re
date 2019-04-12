module Dom = {
  [@bs.get] external getClientWidth: Dom.element => int = "clientWidth";
  [@bs.get] external getClientHeight: Dom.element => int = "clientHeight";

  [@bs.val] [@bs.scope "document"] external getCanvasById: string => Canvas.t = "getElementById";
  [@bs.val] [@bs.scope "document"] external getElementById: string => Dom.element = "";

  [@bs.val] [@bs.scope "window"]
  external requestAnimationFrame: (float => unit) => unit = "";
};

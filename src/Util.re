module Dom = {
  type el = Dom.element;

  [@bs.get] external getClientWidth: el => int = "clientWidth";
  [@bs.get] external getClientHeight: el => int = "clientHeight";

  [@bs.send] external appendChild: el => el => unit = "";
  [@bs.send] external setAttribute: el => string => string => unit = "";
  [@bs.set] external setInnerText: el => string => unit = "innerText";

  [@bs.val] [@bs.scope "document"] external getCanvasById: string => Canvas.t = "getElementById";
  [@bs.val] [@bs.scope "document"] external getElementById: string => el = "";
  [@bs.val] [@bs.scope "document"] external createElement: string => el = "";
  [@bs.val] [@bs.scope "window"] external requestAnimationFrame: (float => unit) => unit = "";

  let setAttr = (el, key, value) => {
    el->setAttribute(key, value)
    el;
  };

  let setInnerText = (el, text) => {
    setInnerText(el, text);
    el;
  }
};

module Dom = {
  type el = Dom.element;

  [@bs.get] external getClientWidth: el => int = "clientWidth";
  [@bs.get] external getClientHeight: el => int = "clientHeight";
  [@bs.get] external currentTarget: Dom.event => el = "";
  [@bs.get] external firstChild: el => el = "";
  [@bs.get] external hasFirstChild: el => bool = "firstChild";

  [@bs.new] external newEvent: string => Dom.event = "Event";

  [@bs.send] external appendChild: el => el => unit = "";
  [@bs.send] external getAttr: el => string => string = "getAttribute";
  [@bs.send] external setAttr: el => string => string => unit = "setAttribute";
  [@bs.send] external preventDefault: Dom.event => unit = "";
  [@bs.send] external remove: el => unit = "";

  [@bs.send]
    external on: (el, string, Dom.event => unit) => unit = "addEventListener";
  [@bs.send]
    external onWindow: (Dom.window, string, unit => unit) => unit = "addEventListener";
  [@bs.send]
    external dispatchEvent: (Dom.window, Dom.event) => unit = "dispatchEvent";

  [@bs.set] external setInnerText: el => string => unit = "innerText";

  [@bs.val] [@bs.scope "document"] external getCanvasById: string => Canvas.t = "getElementById";
  [@bs.val] [@bs.scope "document"] external getElementById: string => el = "";
  [@bs.val] [@bs.scope "document"] external createElement: string => el = "";
  [@bs.val] [@bs.scope "window"] external requestAnimationFrame: (float => unit) => unit = "";
  [@bs.val] external window: Dom.window = "";

  let removeChildrenNodes = (parent: el) => {
    while (hasFirstChild(parent)) {
      firstChild(parent)->remove;
    };
  }


  let on = (el, event, handler) => {
    on(el, event, handler);
    el;
  }

  let setAttr = (el, key, value) => {
    el->setAttr(key, value);
    el;
  };

  let setInnerText = (el, text) => {
    setInnerText(el, text);
    el;
  }
};

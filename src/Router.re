[@bs.val] [@bs.scope "window"] external history: Dom.history = "";
[@bs.val] [@bs.scope "window"] external location: Dom.location = "";
[@bs.send]
external pushState:
  (Dom.history, [@bs.as {json|null|json}] _, [@bs.as ""] _, ~path: string) =>
  unit =
  "";

[@bs.get] external pathname: Dom.location => string = "";

let path = () =>
  switch (location |> pathname) {
  | ""
  | "/" => []
  | raw =>
    open Tablecloth.String;
    let trimmed =
      startsWith(~prefix="/", raw) ? dropLeft(~count=1, raw) : raw;
    split(~on="/", trimmed);
  };

let popstateEvent = Util.Dom.newEvent("popstate");

let push = path => {
  pushState(history, ~path);
  Util.Dom.dispatchEvent(Util.Dom.window, popstateEvent);
};

let watch = (f: list(string) => unit) => {
  f(path());
  Util.Dom.(onWindow(window, "popstate", _ => f(path())));
};
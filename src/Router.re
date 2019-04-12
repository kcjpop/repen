[@bs.val] [@bs.scope ("window")] external history: Dom.history = "";
[@bs.val] [@bs.scope ("window")] external location: Dom.location = "";

[@bs.get] external pathname: Dom.location => string = "";

let path = () =>
  switch (location |> pathname) {
  | ""
  | "/" => []
  | raw => {
    open Tablecloth.String;
    let trimmed = startsWith(~prefix="/", raw) ? dropLeft(~count=1, raw) : raw;
    split(~on="/", trimmed);
  }
  | _ => []
  };


[%%debugger.chrome];

let renderMainContent = currentPath => {
  open Util.Dom;
  open Canvas;
  let canvas = getCanvasById("js-canvas");
  let mainEl = getElementById("js-main");
  let canvasWidth = getClientWidth(mainEl);
  let canvasHeight = max(getClientHeight(mainEl), getClientHeight(body));

  let ctx = getContext(canvas, "2d", contextOptions(~alpha=false));
  imageSmoothingEnabledSet(ctx, true);
  imageSmoothingQualitySet(ctx, "high");
  resize(canvas, canvasWidth, canvasHeight);

  switch (currentPath) {
  | ["trigonoparty"] => Trigonoparty.run(~canvas, ~ctx)
  | ["particles"] => Particles.run(~canvas, ~ctx)
  | [] => Trigonoparty.run(~canvas, ~ctx)
  | _ => Js.log("Not found")
  };
};

let renderSidebar = _ => {
  open Util.Dom;
  let sidebarEl = getElementById("js-sidebar");

  let makeLinkEl = href =>
    createElement("a")
    ->setAttr("href", "/" ++ href)
    ->setAttr("class", "link")
    ->setInnerText(Tablecloth.String.capitalize(href))
    ->on("click", e => {
        preventDefault(e);
        currentTarget(e)->getAttr("href")->Router.push;
      });

  /* Remove all children nodes */
  sidebarEl->removeChildrenNodes;

  ["trigonoparty", "particles"]
  |> List.iter(url => url |> makeLinkEl |> appendChild(sidebarEl));
};

let app = currentPath => {
  renderMainContent(currentPath);
  renderSidebar(currentPath);
};

Router.watch(app);
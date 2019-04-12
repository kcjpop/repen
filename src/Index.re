[%%debugger.chrome]

let renderMainContent = fun
  | ["trigonoparty"] => Trigonoparty.run()
  | [] => Trigonoparty.run()
  | _ => Js.log("Not found")
  ;

let renderSidebar = _ => {
  open Util.Dom;
  let sidebarEl = getElementById("js-sidebar")

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
    |> List.iter(url =>
      url
      |> makeLinkEl
      |> appendChild(sidebarEl)
    );
};

let app = currentPath => {
  renderMainContent(currentPath);
  renderSidebar(currentPath);
};

Router.watch(app);

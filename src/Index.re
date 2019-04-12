[%%debugger.chrome]

let renderMainContent = fun
  | ["trigonoparty"] => Trigonoparty.run()
  | [] => Trigonoparty.run()
  | _ => Js.log("Not found")
  ;

let renderSidebar = _ => {
  open Util.Dom;
  let sidebarEl = getElementById("js-sidebar")

  let makeLinkEl = href => createElement("a")
    ->setAttr("href", href)
    ->setAttr("class", "link")
    ->setInnerText(Tablecloth.String.capitalize(href));

    ["trigonoparty", "particles"]
    |> List.iter(url =>
      url
      |> makeLinkEl
      |> appendChild(sidebarEl)
    );
};

let app = () => {
  let currentPath = Router.path();
  renderMainContent(currentPath);
  renderSidebar(currentPath)
};

app();
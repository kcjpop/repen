[%%debugger.chrome]

let app = () => {
  let path = Router.path()
  switch path {
  | ["trigonoparty"] => Trigonoparty.run()
  | [] => Js.log("Home")
  | _ => Js.log("Not found")
  };
};

app();
[@bs.val] [@bs.scope "performance"] external now: unit => float = "";

let fps = ref(0.);
let lastDrawTime = ref(0.);
let lastFpsUpdate = ref(0.);

let calculate = () => {
  let n = now();
  if (n -. lastFpsUpdate^ >= 100.) {
    lastFpsUpdate := n;
    fps := 1000. /. (n -. lastDrawTime^);
  };

  lastDrawTime := n;
  fps^;
};
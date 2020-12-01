import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: [window.innerHeight, window.innerWidth],
});

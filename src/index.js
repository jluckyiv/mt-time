import "./main.css";
import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: localStorage.getItem("cache") || "",
});

app.ports.cache.subscribe(function (data) {
  localStorage.setItem("cache", JSON.stringify(data));
});

registerServiceWorker();

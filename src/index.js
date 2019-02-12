import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const readCache = function() {
  return localStorage.getItem("cache");
};

const app = Elm.Main.init({
  node: document.getElementById("root")
  // flags: readCache() || ""
});

app.ports.requestData.subscribe(function(data) {
  app.ports.receiveData.send(readCache());
});

app.ports.sendData.subscribe(function(data) {
  localStorage.setItem("cache", JSON.stringify(data));
});

registerServiceWorker();

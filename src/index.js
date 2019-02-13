import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const readCache = function(key) {
  switch (key) {
    case "prosecution":
      return JSON.parse(localStorage.getItem("cache")).prosecution;
    case "defense":
      return JSON.parse(localStorage.getItem("cache").defense);
    default:
      return JSON.parse(localStorage.getItem("cache"));
  }
};

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: readCache()
});

app.ports.requestData.subscribe(function(key) {
  app.ports.receiveData.send(readCache(key));
});

app.ports.sendData.subscribe(function(data) {
  console.log("data", data);
  localStorage.setItem("cache", JSON.stringify(data));
});

registerServiceWorker();

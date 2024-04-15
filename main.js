import { Elm } from "./src/Main.elm";

const app = Elm.Main.init();

app.ports.scrollIntoView.subscribe((args) => {
  const el = document.getElementById(args.id);
  const defaultArgs = { behavior: "smooth", block: "nearest", inline: "center" };
  el.scrollIntoView(Object.assign(defaultArgs, args));
});

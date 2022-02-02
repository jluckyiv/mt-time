# RMT Timer

This project creates a timer calculator for mock trial competitions.

Although it used to use [Create Elm App](https://github.com/halfzebra/create-elm-app), I moved to [Parcel.js](https://parceljs.org) and [elm-tooling-cli](https://elm-tooling.github.io/elm-tooling-cli/) to simplify dependencies.

I don't understand [Webpack](https://webpack.js.org). I wanted something with easier configuration and a faster compiler ([SWC](https://swc.rs)).

After cloning the repo, `pnpm install`.

## Netlify deploy settings

[This blog post](https://www.elian.codes/blog/09-27-21-using-pnpm-on-netlify/) gives a simple solution to using [pnpm](https://pnpm.io) with Netlify:

Set `Build command` to `pnpm build || ( npm install pnpm && pnpm build )`

Parcel.js builds to `./dist`, so set `Publish directory` accordingly.

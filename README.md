[![Dev Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://algebraicjulia.github.io/Semagrams.jl/dev)

# Semagrams.jl

A graphical editor for graph-like structures based on [Catlab](https://github.com/AlgebraicJulia/Catlab.jl).

Legacy version built with typescript is in the `legacy` branch, and will not receive updates; new version with scala is now in the `main` branch.

## How to run

The core of Semagrams is just a library; in order to make it do things, one needs to create an "app" that uses it. Currently, the only app that is being developed is a Petri net editor, though this will soon change.

In order to run the Petri net editor standalone, install [Mill](https://github.com/com-lihaoyi/mill) and [npm](https://www.npmjs.com/), and then in one terminal in `scala/` run `mill --watch apps.petri.fullLinkJS` and in another terminal in `scala/` run `npm run dev`. The second command should print out a url that you can click on. You may have to run `npm install` before running `npm run dev`.

You can look at `scala/build.sc` to figure out how to add another app, though you will have to change `scala/main.js` to point to the compiled js of the new file.

## Trailer:

https://www.youtube.com/watch?v=b1xiY9sznEo

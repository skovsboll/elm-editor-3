


![Screenshot of the HQL editor](res/screenshot.png)



## Why?

- Lightweight, no npm package dependencies
- Elm
- Need more?


## How?
Keep overlays in sync when scrolling:

- A transparent TextArea at the top
- Syntax highlighting layer
- Autocomplete layer (press ctrl+space)
- Error overlay (squigly lines)
- Diff gutter


## LSP support

First two message types are implemented (Down/Diagnostics and Up/DocumentChange). 
Help appreciated. See [messate-types-to-be-supported.txt](https://github.com/skovsboll/elm-editor-3/blob/main/src/Lsp/message-types-to-be-supported.txt)


## Developer getting started

Requirements

- Elm 0.19
- `brew install caddy` (a simple file http server) - or BYO
- `yarn`

You'll need two terminals:

1. `caddy file-server`
2. `yarn exec elm-watch hot`

Go to http://localhost

Hack!

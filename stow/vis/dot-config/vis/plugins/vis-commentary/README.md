## vis-commentary

`vis-commentary` aims to port Tim Pope's [vim-commentary](https://github.com/tpope/vim-commentary) to [vis](https://github.com/martanne/vis).

### Installation
Clone the repo to your vis plugins directory (`~/.config/vis/plugins`) and add this to your `visrc.lua`:
```
require("plugins/vis-commentary")
```

### Usage

| Keybind | Description |
|---------|-------------|
| `gcc`   | Toggle comment of the current line in NORMAL mode.|
| `gc`    | Toggle comment of the current selection in VISUAL mode. |

Should you find bugs or unsupported languages, please report them.

# Software notes

## Software list

### Authoring

- [Pandoc](https://pandoc.org/)
  ([Github](https://github.com/jgm/pandoc)):
  Universal markup converter
  - [Pantable](https://github.com/ickc/pantable):
    Pandoc filter to convert CSV tables to and from Pandoc Markdown
- [QPDF](http://qpdf.sourceforge.net/)
  ([Github](https://github.com/qpdf/qpdf)):
  Command-line tool for manipulating PDF files
- [Skim](https://skim-app.sourceforge.io/)
  ([SourceForge](https://sourceforge.net/projects/skim-app/)):
  Read and annotate PDF files
- [TeXLive](https://www.tug.org/texlive/):
  TeX-distribution. There is also a repackaged version for macOS,
  [MacTeX](http://www.tug.org/mactex/), with extra Mac-specific tools
- [Zettlr](https://www.zettlr.com/)
  ([Github](https://github.com/Zettlr/Zettlr)):
  Markdown editor, designed to be used with Pandoc

### Database

- [DBeaver](https://dbeaver.io/):
  JDBC SQL client
- [OctoSQL](https://github.com/cube2222/octosql):
  Join, analyze and transform data from multiple databases using SQL
- [Postico](https://eggerapps.at/postico/)
  PostgreSQL client
  (macOS)

### Development (General)

- [Code Search](https://github.com/google/codesearch):
  Go-based tool for indexing and searching with regexp on large
  codebases, install with

  ```shell
  go get github.com/google/codesearch/cmd/...
  ```

  and index a dirtree with `cindex` before searching with `cgrep` or
  `csearch` commands.
- [CotEditor](https://coteditor.com/)
  ([Github](https://github.com/coteditor/)):
  Like [Notepad++](https://notepad-plus-plus.org/) but for macOS.
  (macOS)
- [Docker](https://docs.docker.com/):
  Containerization platform
- [Emacs](https://www.gnu.org/software/emacs/):
  See [Emacs config
  repository](https://github.com/matheuristic/emacs-config)
- [Gitup](https://gitup.co/)
  ([Github](https://github.com/git-up/GitUp)):
  GUI Git interface
  (macOS)
- [Plan 9 from User Space](https://9fans.github.io/plan9port/)
  (plan9port):
  See [plan9port config
  repository](https://github.com/matheuristic/plan9port-config)
- [QXmlEdit](http://qxmledit.org/)
  ([Github](https://github.com/lbellonda/qxmledit)):
  XML editor

### Document extraction

- [Apache Tika](http://tika.apache.org/):
  Detect and extract metadata and text from a wide array of filetypes.
  Bindings are available for multiple languages like
  [Python](https://github.com/chrismattmann/tika-python) or
  [Go](https://github.com/google/go-tika)
- [Tesseract](https://github.com/tesseract-ocr/tesseract):
  OCR engine and command-line program used by many other tools
  including Apache Tika above

### Ebook

- [Calibre](https://calibre-ebook.com/)

### File management

- [Cyberduck](https://cyberduck.io/):
  Client for FTP, SFTP, WebDAV and multiple cloud storage providers
  (macOS, Windows)
- [Double Commander](https://doublecmd.sourceforge.io/)
  ([Github](https://github.com/doublecmd/doublecmd)):
  Cross-platform two-panel GUI file manager inspired by Total Commander
- [Marta](https://marta.sh/):
  Commercial two-panel GUI file manager
  (macOS)
- [Midnight Commander](https://midnight-commander.org/)
  ([Github](https://github.com/MidnightCommander/mc)):
  Cross-platform two-panel TUI file manager inspired by Total Commander,
- [Syncthing](https://syncthing.net/)
  ([Github](https://github.com/syncthing/syncthing)):
  Continuous file synchronization.
  For macOS, a native tray application is
  [available](https://github.com/syncthing/syncthing-macos)
  that bundles Syncthing with a wrapper that exposes functionality
  through a system tray icon

### Image creation and editing

- [GIMP](https://www.gimp.org/)
- [Inkscape](https://inkscape.org/)
- [Krita](https://krita.org/en/):
  Raster graphics editor
- [yEd](https://www.yworks.com/products/yed):
  Diagramming tool

### Input devices

- [MOS](https://mos.caldis.me/)
  ([Github](https://github.com/Caldis/Mos)):
  Smooth scrolling and set scroll direction independently for the mouse
  (macOS)
- [Unshaky](https://github.com/aahung/Unshaky):
  Works around double keypress issues for butterfly keyboards
  (macOS)

### News

- [NetNewsWire](https://netnewswire.com/)
  ([Github](https://github.com/Ranchero-Software/NetNewsWire)):
  RSS reader
  (macOS, iOS)
- [Pan](http://pan.rebelbase.com/):
  Usenet newsreader

### Note-taking and task-management

- [iThoughts](https://www.toketaware.com/):
  Commercial mind-mapping tool
- [Joplin](https://joplinapp.org/):
  Cross-platform note-taking app, alternative is jrnl or markdown
- [jrnl](https://jrnl.sh/en/stable/):
  Command-line tool for journaling, alternative is Joplin or markdown
- [LinkedIdeas](https://github.com/fespinoza/LinkedIdeas):
  Mind-mapping tool
  (macOS)
- Markdown files:
  Plain markdown files can be used to keep notes, for a journal this
  could be one file a day with a header that identifies the file, e.g.
  use a dirtree like `$HOME/journal/JOURNALNAME/YYYY-MM/YYYYMMDD.md`
  with contents like

  ```markdown
  # YYYYMMDD DAYOFWEEK

  ## HH:MM SUBJECT @TAG1 @TAG2

  DETAIL
  ```

  and the dirtree can be synced with Git or a cloud storage solution

- [Org mode](https://orgmode.org/):
  [Emacs](https://www.gnu.org/software/emacs/) major mode for
  note-taking, TODO list management, authoring and literate
  programming
- [Taskwarrior](https://taskwarrior.org/)
  Command-line tool for managing TODO list
- [Timewarrior](https://timewarrior.net/)
  Command-line tool for time-tracking, which can be integrated with
  Taskwarrior ([link](https://timewarrior.net/docs/taskwarrior/))

### Package managers

- [Miniforge](https://github.com/conda-forge/miniforge):
  Minimal installer for [Conda](https://conda.io/) pre-configured for
  [conda-forge](https://conda-forge.org/), with the Mambaforge variant
  recommended as it comes packaged with a Conda re-implementation
  [Mamba](https://github.com/mamba-org/mamba) that is much faster
- [MacPorts](https://www.macports.org/):
  Package manager for open-source software
  (macOS)

### Programming languages

- [Clojure](https://clojure.org/)
  - [Leiningen](https://leiningen.org/):
    Build automation and dependency management tool for Clojure
    projects
- [Go](https://golang.org/)
  - [golangci-lint](https://github.com/golangci/golangci-lint):
    Go linters aggregator with YAML-based configuration
- [Markdown](https://www.markdownguide.org/)
  - [markdownlint-cli](https://github.com/igorshubovych/markdownlint-cli):
    Command-line tool for linting Markdown using the
    [markdownlint](https://github.com/DavidAnson/markdownlint) library
  - [prettier](https://github.com/prettier/prettier):
    Code formatter for various languages, including CSS, GraphQL,
    Javascript, JSON, Markdown, Typescript, YAML, etc
  - [Textlint](https://textlint.github.io/):
    Text and markdown linting tool with multiple integrations
- [J](https://code.jsoftware.com/wiki/Main_Page)
- [Javascript](https://github.com/tc39/ecma262)
  - [Node Version Manager](https://github.com/nvm-sh/nvm):
    [Node.js](https://nodejs.org/en/) version manager for POSIX-compliant
    shells
- [Python](https://www.python.org/)
  - [pyinstaller](https://www.pyinstaller.org/)
    ([Github](https://github.com/pyinstaller/pyinstaller)):
    Package Python programs into a single executable file
  - [IPython](https://github.com/ipython/ipython):
    Fancier REPL command shell replacement
  - [nbterm](https://github.com/davidbrochart/nbterm):
    Terminal-base IDE for interactive Jupyter notebooks, see
    [here](https://blog.jupyter.org/nbterm-jupyter-notebooks-in-the-terminal-6a2b55d08b70)
    for more details
  - [JupyterLab](https://jupyter.org/):
    Web-based IDE for interactive Jupyter notebooks that support
    a variety of kernels that span a number of languages
  - [Jupyter Qt console](https://github.com/jupyter/qtconsole):
    Qt console for working with Jupyter kernels, provides a number of
    enhancements like inline figures, multi-line editing and tooltips
    compared to [IPython](https://github.com/ipython/ipython)
- [R](https://www.r-project.org/):
  On macOS, install using MacPorts with

  ```shell
  port install tk +quartz
  port install R +cairo +gcc11 +java +openblas +openmp +quartz +recommended +tcltk -x11
  ```

  where `+gcc11` should be changed to another GCC version as needed
  - [Radian](https://github.com/randy3k/radian):
    Fancier REPL command shell alternative
  - [RStudio](https://rstudio.com/)

### Remote desktop

- [Jump Desktop Connect](https://jumpdesktop.com/connect/):
  Remote desktop host software, required for client devices to connect
  to the host machine using [Jump Desktop](https://jumpdesktop.com/)
  using their
  [Fluid](https://support.jumpdesktop.com/hc/en-us/articles/216423983-General-Fluid-Remote-Desktop)
  protocol (macOS, Windows)
- [Royal TSX](https://royalapps.com/ts/mac/features):
  RDP and VNC client
  (macOS, Windows, iOS, Android)

### Shell

- [bat](https://github.com/sharkdp/bat):
  Command-line `cat` clone with syntax highlighting and Git integration
- [fzf](https://github.com/junegunn/fzf):
  Command-line fuzzy finder
- [Mosh](https://mosh.org/):
  SSH alternative that handles intermittent connectivity and persists
  connections when roaming across IP addresses
- [tmux](https://github.com/tmux/tmux):
  Terminal multiplexer, useful for managing and persisting remote
  sessions over Mosh or SSH
- [ripgrep](https://github.com/BurntSushi/ripgrep):
  Command-line search tool like `grep`, but usually much faster
- [rlwrap](https://github.com/hanslub42/rlwrap):
  `readline` wrapper to enable completion and history for any
  command-line tool taking keyboard input
- [z](https://github.com/rupa/z):
  Track frecent directories and jump to them in Bash and Zsh.
  Clone the repository somewhere and source the `z.sh` file in
  `$HOME/.bashrc` or `$HOME.zshrc`:

  ```shell
  . /path/to/z.sh
  ```

### Structured text and tabular data

- [D-Tale](https://github.com/man-group/dtale):
  Python-based webserver for visualizing Python Pandas data structures,
  install the `dtale` package with `pip` or if using Conda run

  ```
  conda install dtale -c conda-forge
  conda install -c plotly python-kaleido
  ```

  in the desired Conda environment (the second command is needed for
  "Export to PNG" for charts)
- [daff](https://paulfitz.github.io/daff/)
  ([Github](https://github.com/paulfitz/daff)):
  Diff but for tables
- [gron](https://github.com/tomnomnom/gron):
  Flattens JSON into discrete assignments that work better with `grep`
  and `sed`
- [jq](https://stedolan.github.io/jq/)
  ([Github](https://github.com/stedolan/jq)):
  Command-line JSON processor
- [Miller](https://miller.readthedocs.io/en/latest/)
  ([Github](https://github.com/johnkerl/miller)):
  Command-line utility for working with CSV, TSV and tabular JSON files.
  Has some similarities with `jq` and `xsv` but covers multiple formats
  and has more features at the cost of additional complexity
- [q](https://github.com/harelba/q):
  Command-line tool for running SQL directly on CSV files
- [tsv-utils](https://github.com/eBay/tsv-utils):
  Command-line tool for tabular data, similar to xsv but supposedly
  faster for large datasets
- [Visidata](https://www.visidata.org/)
  ([Github](https://github.com/saulpw/visidata)):
  Command-line tabular data multitool
- [xsv](https://github.com/BurntSushi/xsv):
  Command-line tool for indexing, slicing, analyzing, splitting and
  joining CSV files

### VPN

- [TunnelBlick](https://tunnelblick.net/):
  Open source GUI for OpenVPN
  (macOS)
- [WireGuard](https://www.wireguard.com/):
  Simple and high-performance VPN

### Web browsing

- [Chrome](https://www.google.com/chrome/):
  Google's web browser
- [Firefox](https://www.mozilla.org/en-US/firefox/browsers/):
  Mozilla's web browser, there is also an [Extended Release
  Support](https://www.mozilla.org/en-US/firefox/enterprise/)
- [Ka-Block!](http://kablock.com/)
  ([Github](https://github.com/dgraham/Ka-Block)):
  Safari extension for blocking ads and trackers
  (macOS, iOS)
- [Monolith](https://github.com/Y2Z/monolith.git):
  Save complete webpages to a single HTML file with embedded CSS,
  images and Javascript. Installable using HomeBrew or MacPorts on
  macOS. Pre-built binaries are available for Windows and Linux.

### Web development

- [Insomnia](https://insomnia.rest/):
  API client and design platform, supporting GraphQL, REST and gRPC.
  Interfaceable from the command-line using
  [Insomnia Inso](https://insomnia.rest/products/inso)
- [Postman](https://www.postman.com/):
  API client, CI/CD support with
  [Newman](https://github.com/postmanlabs/newman)
- [Prism](https://github.com/stoplightio/prism):
  HTTP mock server with behavior that can be specified from OpenAPI
  v2 (Swagger), OpenAPI v3 or Postman Collection files
- [mitmproxy](https://mitmproxy.org/):
  Interactive HTTPS proxy

### Window management

- [Rectangle](https://github.com/rxhanson/Rectangle):
  Move and resize windows using keyboard shortcuts and snap areas
  (macOS)

### Miscellaneous

- [cspell](https://github.com/streetsidesoftware/cspell):
  Code-aware spellchecker, install with

  ```shell
  npm install -g cspell
  ```

- [DevonThink Pro](https://www.devontechnologies.com/apps/devonthink):
  Commercial document management and search solution
  (macOS)
- [enchant](https://github.com/AbiWord/enchant):
  Wrapper for abstracting different spell checking libararies into a
  single interface, with support for personal word lists (one word per
  line) at paths `$ENCHANT_CONFIG_DIR/<lang>.dic` (for example,
  `$HOME/.config/enchant/en_US.dic` for the US English personal word
  list on Unix or Linux systems)
- [FontForge](https://fontforge.org/):
  Font editor
- [Hammerspoon](https://www.hammerspoon.org/):
  Enables scripting with [Lua](https://www.lua.org/) to interact with
  macOS APIs, for example it can be used to enable scrolling by moving
  the mouse when holding down the middle-button
  ([link](https://superuser.com/questions/303424/can-i-enable-scrolling-with-middle-button-drag-in-os-x))
  (macOS)
- [htop](https://htop.dev/)
  ([Github](https://github.com/htop-dev/htop)):
  TUI process viewer similar to `top` but with more features
- [InvoicePlan](https://www.invoiceplane.com/)
  ([Github](https://github.com/InvoicePlane/InvoicePlane)):
  Self-hosted invoice management application
- [pass](https://www.passwordstore.org/):
  Command-line password manager
- [pdfpc](https://github.com/pdfpc/pdfpc)
  ([Github](https://github.com/pdfpc/pdfpc)):
  Multi-monitor PDF presentation application
- [Platypus](https://github.com/sveinbjornt/Platypus)
  ([Github](https://github.com/sveinbjornt/Platypus)):
  Create macOS apps from command-line scripts
  (macOS)
- [PyCaret](https://pycaret.org/)
  ([Github](https://github.com/pycaret/pycaret)):
  Low-code Python-based machine learning library, best installed using
  `pip` in its own Conda or Python virtual environment
- [Sloth](https://github.com/sveinbjornt/Sloth)
  ([Github](https://github.com/sveinbjornt/Sloth)):
  Show open files, dirs, sockets and pipes
- [Stow](https://www.gnu.org/software/stow/):
  Symlink farm manager, useful for managing dotfiles
- [ueli](https://ueli.app/)
  ([Github](https://github.com/oliverschwendener/ueli)):
  Launcher like [Alfred](https://www.alfredapp.com/) but open-source.
  (macOS, Windows)
- [Zotero](https://www.zotero.org/):
  Reference management software to collect, organize, cite and share
  research material

## Mac notes

### Installing XCode command-line tools

```shell
sudo rm -rf /Library/Developer/CommandLineTools
sudo xcode-select --install
```

## Usage notes

### GnuPG

#### Extending key expirations

1. Find the expiring key ID using `gpg --list-keys` (the key ID comes
   after the slash)
2. Edit the key using `gpg --edit-key KEY_ID`
3. View key and subkeys with the `list` command, where selected
   keys/subkeys have and asterisk next to them
4. Select/unselect a key using `key N` where `N` is the number of the
   desired key in the list
5. The command `expire` will allow the interactive selection of a new
   expiry.
6. Repeat as needed (may not be needed if multiple keys are selected
   and their expirations extended as above)
7. Check new expirations using `list`
8. Save with the `save` command
9. Publish/disseminate the updated keys as appropriate

Run `help` while in the GPG shell for additional commands available.

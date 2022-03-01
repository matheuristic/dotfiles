# Software notes

## Software list

### Authoring

- [Asciidoctor](https://asciidoctor.org/):
  Asciidoc processor, supports direct conversion to PDF
  using an [extension](https://asciidoctor.org/docs/asciidoctor-pdf/)
  for converting to PDF; generally more pleasant for creating longer
  or more complex docs than Markdown; install with RubyGems as follows

  ```sh
  gem install asciidoctor
  gem install asciidoctor-pdf rghost rouge
  ```

  Compile to PDF using a command like

  ```sh
  asciidoctor -r asciidoctor-pdf -b pdf filename.asciidoc
  ```

  Also note that Kramdoc documents can be converted to Markdown using
  the `kramdown-asciidoc` gem
- [Manuskript](https://www.theologeek.ch/manuskript/)
  ([Github](https://github.com/olivierkes/manuskript)):
  Open-source tool for writers like
  [Scrivener](https://www.literatureandlatte.com/scrivener/)
- [Pandoc](https://pandoc.org/)
  ([Github](https://github.com/jgm/pandoc)):
  Universal markup converter; alternatives are
  [MultiMarkdown](https://github.com/fletcher/MultiMarkdown-6),
  [Kramdown](https://kramdown.gettalong.org/), and
  [cmark](https://github.com/commonmark/cmark)
  - [Pantable](https://github.com/ickc/pantable):
    Pandoc filter to convert CSV tables to and from Pandoc Markdown
- [QPDF](http://qpdf.sourceforge.net/)
  ([Github](https://github.com/qpdf/qpdf)):
  Command-line tool for manipulating PDF files
- [Scribus](https://www.scribus.net/)
  ([Mirror repo on Github](https://github.com/scribusproject/scribus)):
  Desktop publishing software (for laying out pages) like Affinity
  Publisher, InDesign or QuarkXPress
- [Tectonic](https://tectonic-typesetting.github.io/)
  ([Github](https://github.com/tectonic-typesetting/tectonic)):
  TeX/LaTeX engine, automatically pulls in packages from
  [CTAN](https://ctan.org/) as needed; an alternative to
  the more comprehensive [TeXLive](https://www.tug.org/texlive/),
  its macOS repackage [MacTeX](http://www.tug.org/mactex/), or the
  lightweight [TinyTeX](https://github.com/yihui/tinytex)
- [Twine](https://twinery.org/)
  ([Github](https://github.com/klembot/twinejs)):
  Tool for authoring interactive, non-linear stories
- [Zettlr](https://www.zettlr.com/)
  ([Github](https://github.com/Zettlr/Zettlr)):
  Markdown editor, designed to be used with Pandoc

### Data pipelines

- [Apache Airflow](https://airflow.apache.org/):
  Workflow/pipeline management tool; alternatives include
  [Prefect](https://github.com/PrefectHQ/prefect),
  [dbt](https://github.com/dbt-labs/dbt-core),
  [Snakemake](https://snakemake.readthedocs.io/en/stable/), or see
  [here](https://github.com/common-workflow-language/common-workflow-language/wiki/Existing-Workflow-systems)
- [Apache HOP](https://hop.apache.org/)
- [List of pipeline tools](https://github.com/pditommaso/awesome-pipeline)

### Data Science and Analytics

- [D-Tale](https://github.com/man-group/dtale):
  Python-based webserver for visualizing Python Pandas data structures,
  install the `dtale` package with `pip` or if using Conda run

  ```sh
  conda install dtale -c conda-forge
  conda install -c plotly python-kaleido
  ```

  in the desired Conda environment (the second command is needed for
  "Export to PNG" for charts)
- [JASP](https://jasp-stats.org/)
  ([Github](https://github.com/jasp-stats/jasp-desktop)):
  GUI program for statistical analysis, alternatives include
  [SOFA](http://www.sofastatistics.com/home.php),
  [PSPP](https://www.gnu.org/software/pspp/),
  or the commercial [Prism](https://www.graphpad.com/) and
  [Minitab](https://www.minitab.com/en-us/products/minitab/)
- [Jupyter](https://jupyter.org/):
  Collection of tools for interactive development across multiple
  languages, like the web-based interactive
  [Jupyter notebook](https://github.com/jupyter/notebook),
  the web-based IDE that can work on these notesbooks plus data and code
  [JupyterLab](https://github.com/jupyterlab/jupyterlab),
  its multi-user version
  [JupyterHub](https://github.com/jupyterhub/jupyterhub),
  or tooling to convert Jupyter notebooks into standalone webapps
  [Voilà](https://github.com/voila-dashboards/voila)
- [MLflow](https://mlflow.org/):
  Machine-learning model lifecycle management platform, in particular
  tracking experiments and logging artifacts
- [ROOT](https://root.cern/)
  ([Github](https://github.com/root-project/root)):
  CERN-developed framework for analyzing petabytes of data
- [Sweetviz](https://github.com/fbdesignpro/sweetviz):
  Python library to visualize and compare data sets
- [Veusz](https://veusz.github.io/)
  ([Github](https://github.com/veusz/veusz)):
  Scientific plotting application, alternatives are
  [SciDAVis](http://scidavis.sourceforge.net/)
  ([QtiPlot](https://www.qtiplot.com/) fork),
  [AlphaPlot](https://github.com/narunlifescience/AlphaPlot) (SciDAVis
  fork), [LabPlot](https://labplot.kde.org/),
  [TOPCAT](http://www.star.bris.ac.uk/~mbt/topcat/) (good with large
  and sparse datasets), [ROOT](https://root.cern/) (good with big
  data, limited functionality), or the commercial options
  [Origin/OriginPro](https://www.originlab.com/)

### Database

- [PostgreSQL](https://www.postgresql.org/):
  Open-source relational database
- [ClickHouse](https://github.com/ClickHouse/ClickHouse):
  High-performance time-series database; alternatives include
  [TimescaleDB](https://github.com/timescale/timescaledb)
  (PostgreSQL extension to better support time-series data)

### Database client

- [Altair](https://altair.sirmuel.design/)
  ([Github](https://github.com/altair-graphql/altair)):
  Cross-platform GraphQL client
- [Azure Data Studio](https://docs.microsoft.com/en-us/sql/azure-data-studio/)
  ([Github](https://github.com/microsoft/azuredatastudio)):
  Microsoft SQL Server, Azure SQL Database, PostgreSQL and Jupyter
  Notebook client
- [Beekeeper Studio](https://www.beekeeperstudio.io/)
  ([Github](https://github.com/beekeeper-studio/beekeeper-studio)):
  SQLite, MySQL, MariaDB, Postgres, CockroachDB, Microsoft SQL Server
  and Amazon Redshift client with a nicer interface than DBeaver but
  not as featureful
- [DBeaver](https://dbeaver.io/)
  ([Github](https://github.com/dbeaver/dbeaver)):
  JDBC SQL client, very full-featured but user interface is complex (a
  zip package is available from the website files
  [archive](https://dbeaver.io/files/) which requires Java be
  installed on the machine, e.g. via the `openjdk-11-jre` package or
  another version)
- [DbGate](https://dbgate.org/)
  ([Github](https://github.com/dbgate/dbgate)):
  Cross-platform database client, supports MySQL, PostgreSQL, SQL Server,
  MongoDB, SQLite, CSV files and others
- [DuckDB](https://duckdb.org/)
  ([Github](https://github.com/duckdb/duckdb)):
  In-process SQL database with APIs for Python, R and other languages,
  with Python support especially nice as it can work with Pandas
  DataFrames ([link](https://duckdb.org/2021/05/14/sql-on-pandas.html))
  so SQL queries can be run directly on Pandas DataFrames; an alternative
  with comparable Pandas support is
  [MonetDB/e-Python](https://github.com/MonetDBSolutions/MonetDBe-Python)
- [HeidiSQL](https://www.heidisql.com/)
  ([Github](https://github.com/HeidiSQL/HeidiSQL))
  MariaDB, MySQL, Microsoft SQL, PostgreSQL and SQLite client for Windows
- [Robo 3T](https://github.com/Studio3T/robomongo):
  MongoDB management tool, previously called Robomongo
- [OctoSQL](https://github.com/cube2222/octosql):
  Join, analyze and transform data from multiple databases using SQL
- [Postico](https://eggerapps.at/postico/):
  Commercial PostgreSQL client for macOS
- [RedisDesktopManager](https://github.com/uglide/RedisDesktopManager):
  Redis management tool, Linux binaries are downloadable but Windows
  and Mac binaries require either a
  [subscription](https://rdm.dev/pricing) or
  [compiling from source](https://docs.rdm.dev/en/latest/install/)
- [Sequel Ace](https://sequel-ace.com/)
  ([Github](https://github.com/Sequel-Ace/Sequel-Ace)):
  MySQL and MariaDB client for macOS

### Development (General)

- [asdf](https://asdf-vm.com/)
  ([Github](https://github.com/asdf-vm/asdf)):
  Project runtime version manager supporting multiple programming
  languages, can act as an alternative to
  [gvm](https://github.com/moovweb/gvm),
  [nvm](https://github.com/nvm-sh/nvm),
  [rbenv](https://github.com/rbenv/rbenv),
  [pyenv](https://github.com/pyenv/pyenv) and others
- [cflow](https://www.gnu.org/software/cflow/):
  Call graph generator for C.
  [pycflow2dot](https://github.com/johnyf/pycflow2dot) integrates
  `cflow` with the [Graphviz](http://www.graphviz.org/) `dot` tool to
  output the call graph to multiple formats
- [Code Search](https://github.com/google/codesearch):
  Go-based tool for indexing and searching with regexp on large
  codebases, install with

  ```sh
  go get github.com/google/codesearch/cmd/...
  ```

  and index a dirtree with `cindex` before searching with `cgrep` or
  `csearch` commands.
- [CodeCompass](https://codecompass.net/)
  ([Github](https://github.com/Ericsson/CodeCompass)):
  Cross-platform source explorer, supports C, C++ and Java (and Python
  if using the `pythonplugin` version)
- [Code2flow](https://github.com/scottrogowski/code2flow/):
  Call graph generator for dynamic languages, specifically Python,
  Javascript, Ruby and PHP; some examples of how to use follows

  ```sh
  # Zsh example, all Python files in dirtree
  code2flow **/**.py
  # Same as above, but targeting a specific function somemodule.func
  # and showing its callers and callees up to 3 calls away
  code2flow --target-function=somemodule.func \
    --upstream-depth=3 \
    --downstream-depth=3 \
    **/**.py
  ```

  (see `code2flow --help` for more options)
- [Cookiecutter](https://github.com/cookiecutter/cookiecutter):
  Command-line tool to create projects from project templates
- [Docker](https://docs.docker.com/):
  Containerization platform
- [Gitup](https://gitup.co/)
  ([Github](https://github.com/git-up/GitUp)):
  GUI Git interface for macOS
- [Meld](https://meldmerge.org/)
  ([Gitlab](https://gitlab.gnome.org/GNOME/meld)):
  GTK visual diff and merge tool
- [ngrok](https://ngrok.com/):
  Reverse proxy to expose a local web server to the internet (requires
  an account, [frp](https://github.com/fatedier/frp) and
  [localtunnel](https://github.com/localtunnel/localtunnel) are fully
  free versions)
- [onefetch](https://github.com/o2sh/onefetch):
  Command-line tool that displays project information and code
  statistics for a local Git repository
- [Plan 9 from User Space](https://9fans.github.io/plan9port/)
  (plan9port):
  See [plan9port config
  repository](https://github.com/matheuristic/plan9port-config)
- [scc](https://github.com/boyter/scc):
  Command line tool to count lines of code by language
- [task](https://taskfile.dev/)
  ([Github](https://github.com/go-task/task)):
  Task runner / build tool / simpler Make alternative

### Document extraction

- [Apache Tika](http://tika.apache.org/):
  Detect and extract metadata and text from a wide array of filetypes.
  Bindings are available for multiple languages like
  [Python](https://github.com/chrismattmann/tika-python) or
  [Go](https://github.com/google/go-tika)
- [Tesseract](https://github.com/tesseract-ocr/tesseract):
  OCR engine and command-line program used by many other tools
  including Apache Tika above

### Ebook and PDF reading and annotation

- [Calibre](https://calibre-ebook.com/)
- [MuPDF](https://mupdf.com/):
  Lightweight PDF reader, also supports XPS, EPUB and CBZ;
  [sioyek](https://github.com/ahrm/sioyek) is a reader
  built on top of this that provides additional features
  useful for reading technical books and research papers;
  an alternative is [Zathura](https://pwmt.org/projects/zathura/)
- [Skim](https://skim-app.sourceforge.io/)
  ([SourceForge](https://sourceforge.net/projects/skim-app/)):
  Read and annotate PDF files, macOS app
- [Xournal++](https://github.com/xournalpp/xournalpp):
  Cross-platform handwriting notetaking and PDF annotation tool

### File management

- [BorgBackup](https://www.borgbackup.org/)
  ([Github](https://github.com/borgbackup/borg)):
  Command-line deduplicating backup program supporting compression and
  encryption, with frontends available like
  [Vorta](https://vorta.borgbase.com/)
  ([Github](https://github.com/borgbase/vorta))
- [Cyberduck](https://cyberduck.io/):
  Client for FTP, SFTP, WebDAV and multiple cloud storage providers,
  available on macOS and Windows
- [dar](http://dar.linux.free.fr/):
  dar (or disk archive) is an archiving tool for Unix platforms.
  Supports encryption, and also supports recovery records if
  [par2](https://github.com/Parchive/par2cmdline/) is installed
- [Double Commander](https://doublecmd.sourceforge.io/)
  ([Github](https://github.com/doublecmd/doublecmd)):
  Cross-platform two-panel GUI file manager inspired by Total Commander
- [Marta](https://marta.sh/):
  Commercial two-panel GUI file manager for macOS
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
- [Timeshift](https://teejeetech.com/timeshift/)
  ([Github](https://github.com/teejee2008/timeshift)):
  System restore tool for Linux, similar to Time Machine in macOS or
  System Restore in Windows (note that this is targeted at backing up
  only system files/settings and excludes user files by default, so
  use it with another solution like BorgBackup+Vorta)

### Input devices

- [MOS](https://mos.caldis.me/)
  ([Github](https://github.com/Caldis/Mos)):
  Smooth scrolling and set scroll direction independently for the mouse in macOS
- [skhd](https://github.com/koekeishiya/skhd):
  Hotkey daemon for macOS
- [Unshaky](https://github.com/aahung/Unshaky):
  Works around double keypress issues for butterfly keyboards in macOS

### Media creation and editing

- [Ardour](https://ardour.org/):
  Multitrack audio recorder
- [Caire](https://github.com/esimov/caire):
  Command-line tool for content-aware image resizing
- [drawio-desktop](https://github.com/jgraph/drawio-desktop):
  Electron build of [diagrams.net](https://www.diagrams.net/),
  a web-based diagramming tool, for offline use
- [GIMP](https://www.gimp.org/):
  Raster graphics editor
- [Inkscape](https://inkscape.org/):
  Vector graphics editor
- [Kdenlive](https://kdenlive.org/en/):
  Video editor
- [LMMS](https://lmms.io/):
  Pattern-based music sequencer
- [Krita](https://krita.org/en/):
  Raster graphics editor
- [sfxr-qt](https://github.com/agateau/sfxr-qt):
  [Qt](https://www.qt.io/) port of
  [SFXR](http://www.drpetter.se/project_sfxr.html) for quickly
  producing sound effects for games (
  [JavaScript](https://github.com/chr15m/jsfxr) port available
  [online](https://sfxr.me/), and sfxr is also available as a built-in
  instrument in LMMS)
- [Synfig](https://www.synfig.org/)
  ([Github](https://github.com/synfig/synfig/)):
  Vector-based 2D animation software
- [yEd](https://www.yworks.com/products/yed):
  Diagramming tool

### News

- [NetNewsWire](https://netnewswire.com/)
  ([Github](https://github.com/Ranchero-Software/NetNewsWire)):
  RSS reader for macOS (and iOS)
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
  Mind-mapping tool for macOS
- Markdown files:
  Plain markdown files can be used to keep notes, for a journal this
  could be one file a day with a header that identifies the file, e.g.
  use a dirtree like `$HOME/journal/JOURNALNAME-YYYY.md`
  with contents like

  ```markdown
  # Journal YYYY
  ## YYYY-MM-DD DAYOFWEEK
  ### HH:MMXM-HH:MMXM SUBJECT #TAG1 #TAG2
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
- [Xournal++](https://xournalpp.github.io/)
  ([Github](https://github.com/xournalpp/xournalpp/)):
  Notetaking software with handwriting support

### Package managers

- [Flatpak](https://flatpak.org/):
  Utility for software packaging and deployment on Linux, with
  [Flathub](https://flathub.org/) as the de facto standard repository
  - [Flatseal](https://github.com/tchx84/Flatseal):
    GUI utility for managing Flatpak app permissions, available on
    [Flathub](https://flathub.org/apps/details/com.github.tchx84.Flatseal)
- [Homebrew](https://brew.sh/)
  ([Github](https://github.com/Homebrew/brew)):
  macOS package manager for open-source software, alternative to MacPorts
- [Miniforge](https://github.com/conda-forge/miniforge):
  Minimal installer for [Conda](https://conda.io/) pre-configured for
  [conda-forge](https://conda-forge.org/), with the Mambaforge variant
  recommended as it comes packaged with a Conda re-implementation
  [Mamba](https://github.com/mamba-org/mamba) that is much faster
- [MacPorts](https://www.macports.org/):
  macOS package manager for open-source software, alternative to Homebrew
- [Snap](https://snapcraft.io/):
  Utility for software packaging with deployment via a centralized
  repository, developed by [Canonical](https://canonical.com/)

### Presentation tools

- [Marp](https://marp.app/): Create presentations from Markdown documentations,
  usable via the [marp-cli](https://github.com/marp-team/marp-cli) command-line
  tool and [Marp for VSCode](https://github.com/marp-team/marp-vscode)

### Programming language tooling

- [Clojure](https://clojure.org/)
  - [clojure-lsp](https://github.com/clojure-lsp/clojure-lsp):
    [LSP](https://microsoft.github.io/language-server-protocol/)
    server for Clojure and Clojurescript
  - [Leiningen](https://leiningen.org/):
    Build automation and dependency management tool for Clojure
    projects
- [Common Lisp](https://common-lisp.net/):
  Tooling
  [survey](http://lisp-journey.gitlab.io/blog/state-of-the-common-lisp-ecosystem-2020/);
  [Review](https://sabracrolleton.github.io/json-review) of Common Lisp JSON
  libraries
  - [Roswell](https://roswell.github.io/)
    ([Github](https://github.com/roswell/roswell)):
    Common Lisp implementation manager, installer and script runner
  - [CLPM](https://gitlab.common-lisp.net/clpm/clpm):
    Common Lisp Project Manager, a
    [QuickLisp](https://www.quicklisp.org/)-compatible package manager that
    allows pinning project-local package versions
- [Coq](https://coq.inria.fr/)
  ([Github](https://github.com/coq/coq))
  - [Editor tooling](https://coq.inria.fr/user-interfaces.html)
- [Elixir](https://elixir-lang.org/)
  - [Credo](https://github.com/rrrene/credo):
    Static code analysis tool for projects with focus on teaching and
    code consistency
  - [Dialyxir](https://github.com/jeremyjh/dialyxir):
    Simplifies usage of the built-in
    [Dialyzer](https://www.erlang.org/doc/man/dialyzer.html) with the
    [Mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
    build tool
  - [ElixirLS](https://github.com/elixir-lsp/elixir-ls):
    LSP server for the Elixir programming language
- [Go](https://golang.org/)
  - [golangci-lint](https://github.com/golangci/golangci-lint):
    Go linters aggregator with YAML-based configuration
  - [gopls](https://pkg.go.dev/golang.org/x/tools/gopls):
    [LSP](https://microsoft.github.io/language-server-protocol/)
    server for the Go language
- [Markdown](https://www.markdownguide.org/)
  - [markdownlint-cli](https://github.com/igorshubovych/markdownlint-cli):
    Command-line tool for linting Markdown using the
    [markdownlint](https://github.com/DavidAnson/markdownlint) library
  - [mdformat](https://mdformat.readthedocs.io/en/stable/)
    ([Github](https://github.com/executablebooks/mdformat)):
    Markdown formatter. Supports
    [CommonMark](https://spec.commonmark.org/current/),
    [GFM](https://github.github.com/gfm/), and
    [MyST](https://myst-parser.readthedocs.io/en/latest/using/syntax.html)
  - [Textlint](https://textlint.github.io/):
    Text and markdown linting tool with multiple integrations
- [J](https://code.jsoftware.com/wiki/Main_Page)
  - [Jsource](https://github.com/jsoftware/jsource):
    GPL-licensed source code for the J engine
- [Javascript](https://github.com/tc39/ecma262)
  - [Node Version Manager](https://github.com/nvm-sh/nvm):
    [Node.js](https://nodejs.org/en/) version manager for POSIX-compliant
    shells
- [Julia](https://julialang.org/)
  - [Revise.jl](https://github.com/timholy/Revise.jl):
    Automatically reload code on modification, useful to keep Julia
    sessions running longer so less time is spent on compilation
  - [Pluto.jl](https://github.com/fonsp/Pluto.jl):
    Lightweight reactive Julia notebooks
- Multi-language
  - [DevSkim](https://github.com/microsoft/DevSkim):
    Security analysis tool for various languages
  - [prettier](https://github.com/prettier/prettier):
    Code formatter for various languages, including CSS, GraphQL,
    Javascript, JSON, Markdown, Typescript, YAML, etc
- [Python](https://www.python.org/)
  For development, it is recommended to use `miniforge` (see above) to
  manage environments.
  - [pyinstaller](https://www.pyinstaller.org/)
    ([Github](https://github.com/pyinstaller/pyinstaller)):
    Package Python programs into a single executable file
  - [IPython](https://github.com/ipython/ipython):
    Fancier REPL command shell replacement
  - [nbterm](https://github.com/davidbrochart/nbterm):
    Terminal-base IDE for interactive Jupyter notebooks, see
    [here](https://blog.jupyter.org/nbterm-jupyter-notebooks-in-the-terminal-6a2b55d08b70)
    for more details
  - [jedi-language-server](https://github.com/pappasam/jedi-language-server):
    [LSP](https://microsoft.github.io/language-server-protocol/)
    server for the [Jedi](https://github.com/pappasam/jedi-language-server)
    Python tool
  - [Pylint](https://pylint.org/)
    ([Github](https://github.com/PyCQA/pylint)):
    Linter
- [R](https://www.r-project.org/):
  On macOS, install using MacPorts with

  ```sh
  port install tk +quartz
  port install R +cairo +gcc11 +java +openblas +openmp +quartz +recommended \
      +tcltk -x11
  ```

  where `+gcc11` should be changed to another GCC version as needed.

  On Linux, to compile CRAN packages make sure to install the
  necessary compilers, libraries and development headers, for example

  ```sh
  sudo apt install build-essential libssl-dev libcurl4-openssl-dev libxml2-dev
  ```

  in Ubuntu (modify as needed for other distributions).

  For development, it is recommended to use
  [renv](https://rstudio.github.io/renv/)
  ([CRAN](https://cran.r-project.org/web/packages/renv/index.html),
  [Github](https://github.com/rstudio/renv/)) to manage environments
  - [Radian](https://github.com/randy3k/radian):
    Fancier REPL command shell alternative
  - [RStudio](https://rstudio.com/):
    IDE for R
    - [esquisse](https://github.com/dreamRs/esquisse):
      RStudio plug-in for interactive plot creation using ggplot2
- Shell ([Bash](https://www.gnu.org/software/bash/),
  [POSIX](https://www.grymoire.com/Unix/Sh.html),
  [KornShell](https://github.com/ksh93/ksh))
  - [ShellCheck](https://github.com/koalaman/shellcheck):
    Linter for shell scripts (sh, bash, ksh)
  - [shfmt](https://github.com/mvdan/sh):
    Formatter for shell scripts (sh, bash, ksh)

### Remote desktop

- [Jump Desktop Connect](https://jumpdesktop.com/connect/):
  Remote desktop host software, required for client devices to connect
  to the host machine using [Jump Desktop](https://jumpdesktop.com/)
  using their
  [Fluid](https://support.jumpdesktop.com/hc/en-us/articles/216423983-General-Fluid-Remote-Desktop)
  protocol, available for macOS and Windows
- [Remmina](https://remmina.org/)
  ([GitLab](https://gitlab.com/Remmina/Remmina)):
  Remote desktop client for POSIX systems
- [Royal TSX](https://royalapps.com/ts/mac/features):
  Cross-platform RDP and VNC client

### Shell

- [atop](https://github.com/Atoptool/atop):
  System resource monitor, runs as a daemon that logs process activity
- [bat](https://github.com/sharkdp/bat):
  Command-line `cat` clone with syntax highlighting and Git integration
- [btop](https://github.com/aristocratos/btop):
  System resource monitor, alternative to top, [htop](https://htop.dev/),
  [glances](https://nicolargo.github.io/glances/) and other interactive
  process viewers
- [Byobu](https://www.byobu.org/):
  Collection of scripts and utilities layered on top of GNU Screen and
  tmux (default) to enhance them and improve user experience
- [fkill](https://github.com/sindresorhus/fkill-cli):
  Command-line tool to interactively kill running user and system processes
- [forkstat](https://github.com/ColinIanKing/forkstat):
  Command-line program to log process forks, execs and exits (useful
  for tracking runaway processes)
- [fzf](https://github.com/junegunn/fzf):
  Command-line fuzzy finder
- [grex](https://github.com/pemistahl/grex):
  Command-line tool to generate regular expressions from user-provided
  test-cases
- [hollywood](https://github.com/dustinkirkland/hollywood):
  Hollywood technobabble in a Byobu session
- [Mosh](https://mosh.org/):
  SSH alternative that handles intermittent connectivity and persists
  connections when roaming across IP addresses
- [renameutils](https://www.nongnu.org/renameutils/):
  Collection of programs to make renaming of files easier, like `qmv`
  that allows editing file names with a text editor (GUI alternatives
  include [KRename](https://userbase.kde.org/KRename),
  [GPRename](http://gprename.sourceforge.net/) or
  [Szyszka](https://github.com/qarmin/szyszka))
- [ripgrep](https://github.com/BurntSushi/ripgrep):
  Command-line search tool like `grep`, but usually much faster
- [rlwrap](https://github.com/hanslub42/rlwrap):
  `readline` wrapper to enable completion and history for any
  command-line tool taking keyboard input
- [sttr](https://github.com/abhimanyu003/sttr):
  Command-line tool for various string operations
- [tmux](https://github.com/tmux/tmux):
  Terminal multiplexer, useful for managing and persisting remote
  sessions over Mosh or SSH
- [z](https://github.com/rupa/z):
  Track frecent directories and jump to them in Bash and Zsh.
  Clone the repository somewhere and source the `z.sh` file in
  `$HOME/.bashrc` or `$HOME.zshrc`:

  ```sh
  . /path/to/z.sh
  ```

### Structured text and tabular data

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
- [PandasGUI](https://github.com/adamerose/PandasGUI):
  Python GUI library for analyzing and visualizing Pandas DataFrames
- [q](https://github.com/harelba/q):
  Command-line tool for running SQL directly on CSV files
- [QXmlEdit](http://qxmledit.org/)
  ([Github](https://github.com/lbellonda/qxmledit)):
  XML editor
- [sc-im](https://github.com/andmarti1424/sc-im):
  Terminal spreadsheet program
- [tsv-utils](https://github.com/eBay/tsv-utils):
  Command-line tool for tabular data, similar to xsv but supposedly
  faster for large datasets
- [Visidata](https://www.visidata.org/)
  ([Github](https://github.com/saulpw/visidata)):
  Terminal tabular data multitool, supports sources loadable via Pandas using
  the `-f` option
- [xsv](https://github.com/BurntSushi/xsv):
  Command-line tool for indexing, slicing, analyzing, splitting and
  joining CSV files
- [yq](https://github.com/mikefarah/yq):
  Command-line YAML processor

### Text editors

- [CotEditor](https://coteditor.com/)
  ([Github](https://github.com/coteditor/)):
  Like [Notepad++](https://notepad-plus-plus.org/) but for macOS
- [Emacs](https://www.gnu.org/software/emacs/):
  See [Emacs config repository](https://github.com/matheuristic/emacs-config)
- [Micro](https://micro-editor.github.io/):
  Terminal text editor
- [Nano](https://www.nano-editor.org/):
  Lightweight terminal text editor
- [QOwnNotes](https://www.qownnotes.org/)
  ([Github](https://github.com/pbek/QOwnNotes)):
  Markdown editor with Nextcloud and ownCloud integration
- [Vim](https://www.vim.org/)/[Neovim](https://neovim.io/):
  Terminal text editor (GUI options available) that improves on vi
- [Zettlr](https://www.zettlr.com/)
  ([Github](https://github.com/Zettlr/Zettlr)):
  Markdown editor with [Zettelkasten](https://zettelkasten.de/posts/overview/)
  support

### Virtualization

- [UTM](https://github.com/utmapp/UTM):
  iOS and macOS tool for managing [QEMU](https://www.qemu.org/)
  virtual machines
- [virt-manager](https://virt-manager.org/)
  ([Github](https://github.com/virt-manager/virt-manager)):
  Linux desktop tool for managing QEMU/KVM virtual machines

### VPN

- [OpenVPN](https://openvpn.net/vpn-client/):
  OpenVPN client, note that Linux clients be should installed via the system
  package manager (see
  [here](https://community.openvpn.net/openvpn/wiki/OpenVPN3Linux));
  an alternative is the macOS-only [TunnelBlick](https://tunnelblick.net/)
  built on the older but still maintained OpenVPN 2 libraries
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
  Safari extension for blocking ads and trackers, for macOS and iOS
- [Monolith](https://github.com/Y2Z/monolith.git):
  Save complete webpages to a single HTML file with embedded CSS,
  images and Javascript. Installable using HomeBrew or MacPorts on
  macOS, and pre-built binaries are available for Windows and Linux

### Web development
  
- [jo](https://github.com/jpmens/jo):
  Small utility to create JSON objects on the command-line, useful
  for quick crafting of JSON inputs to pipe to APIs in the terminal
- [Killgrave](https://github.com/friendsofgo/killgrave):
  HTTP mock server via a command-line interface and config files
- [Mockoon](https://mockoon.com/)
  ([Github](https://github.com/mockoon/mockoon)):
  HTTP mock server
- [mitmproxy](https://mitmproxy.org/):
  Interactive HTTPS proxy
- [Postman](https://www.postman.com/):
  API client, CI/CD support with
  [Newman](https://github.com/postmanlabs/newman);
  alternatives are [Insomnia](https://insomnia.rest/) which has a
  command-line interface [Inso](https://insomnia.rest/products/inso),
  [httpie](https://httpie.io/) and [xh](https://github.com/ducaale/xh)
  with the latter two being more CLI-based
- [Prism](https://github.com/stoplightio/prism):
  HTTP mock server with behavior that can be specified from OpenAPI
  v2 (Swagger), OpenAPI v3 or Postman Collection files
- [ttyplot](https://github.com/tenox7/ttyplot):
  Real-time plotting tool in the terminal using stdin as data input

### Window management

- [Rectangle](https://github.com/rxhanson/Rectangle):
  Move and resize windows using keyboard shortcuts and snap areas in macOS

### Miscellaneous

- [Al Dente](https://github.com/davidwernhart/AlDente):
  macOS tool to limit battery charging (e.g. keeping charge percentage at or
  below 80% can help prolong battery life), requires a helper application that
  can be installed and removed from the tool's settings
- [Anki](https://apps.ankiweb.net/):
  Flashcards software
- [Aspell](http://aspell.net/)
  ([Github](https://github.com/GNUAspell/aspell)):
  Spell checker designed as a replacement for
  [Ispell](https://www.gnu.org/software/ispell/ispell.html)
- [ClamAV](https://www.clamav.net/)
  ([Github](https://github.com/Cisco-Talos/clamav)):
  Open-source antivirus engine, with frontends available like
  [ClamTk](https://gitlab.com/dave_m/clamtk)
- [cspell](https://github.com/streetsidesoftware/cspell):
  Code-aware spell checker, install with

  ```sh
  npm install -g cspell
  ```

- [DevonThink Pro](https://www.devontechnologies.com/apps/devonthink):
  Commercial document management and search solution for macOS
- [Diagon](https://github.com/ArthurSonzogni/Diagon):
  Command-line tool for transforming Markdown-style expressions into
  ASCII art, [webapp](https://github.com/ArthurSonzogni/Diagon) and
  [snap](https://snapcraft.io/diagon) available
- [enchant](https://github.com/AbiWord/enchant):
  Wrapper for abstracting different spell checking libararies into a
  single interface, with support for personal word lists (one word per
  line) at paths `$ENCHANT_CONFIG_DIR/<lang>.dic` (for example,
  `$HOME/.config/enchant/en_US.dic` for the US English personal word
  list on Unix or Linux systems)
- [FontForge](https://fontforge.org/):
  Font editor
- [Hammerspoon](https://www.hammerspoon.org/):
  Enables macOS scripting with [Lua](https://www.lua.org/) to interact with
  system APIs, for example it can be used to enable scrolling by moving
  the mouse when holding down the middle-button
  ([link](https://superuser.com/questions/303424/can-i-enable-scrolling-with-middle-button-drag-in-os-x))
- [htop](https://htop.dev/)
  ([Github](https://github.com/htop-dev/htop)):
  TUI process viewer similar to `top` but with more features
- [InvoicePlan](https://www.invoiceplane.com/)
  ([Github](https://github.com/InvoicePlane/InvoicePlane)):
  Self-hosted invoice management application
- [LanguageTool](https://languagetool.org/):
  Style and grammar checker, standalone Java version downloadable
  [here](https://languagetool.org/download/) with snapshots available
  [here](https://internal1.languagetool.org/snapshots/) (the latest
  version is always downloadable via this
  [link](https://languagetool.org/download/LanguageTool-stable.zip)),
  [augmentable](https://dev.languagetool.org/finding-errors-using-n-gram-data.html)
  with [n-gram](https://languagetool.org/download/ngram-data/) data
- [Magic Wormhole](https://magic-wormhole.readthedocs.io/)
  ([Github](https://github.com/magic-wormhole/magic-wormhole)):
  Command-line tool and library for sending files from one computer to
  another (note that this tool requires
  [mailbox](https://github.com/magic-wormhole/magic-wormhole-mailbox-server)
  and a
  [transit relay](https://github.com/magic-wormhole/magic-wormhole-transit-relay)
  servers, which by default uses ones hosted by the project)
- [Netron](https://github.com/lutzroeder/netron):
  Neural network, deep learning and machine learning model viewer
- [Nuspell](https://nuspell.github.io/)
  ([Github](https://github.com/nuspell/nuspell)):
  Spell checker designed as a modern alternative to
  [Hunspell](http://hunspell.github.io/)
  ([Github](https://github.com/hunspell/hunspell))
- [OmegaT](https://omegat.org/):
  Translation memory tool
- [pass](https://www.passwordstore.org/):
  Command-line password manager
- [pdfpc](https://github.com/pdfpc/pdfpc)
  ([Github](https://github.com/pdfpc/pdfpc)):
  Multi-monitor PDF presentation application
- [Penpot](https://github.com/penpot/penpot):
  Open source design and prototyping platform like Sketch and Figma
- [Platypus](https://github.com/sveinbjornt/Platypus)
  ([Github](https://github.com/sveinbjornt/Platypus)):
  Create macOS apps from command-line scripts
- [rgb-tui](https://github.com/ArthurSonzogni/rgb-tui):
  Terminal color-picker
- [ScummVM](https://www.scummvm.org/)
  ([Github](https://github.com/scummvm/scummvm)):
  Program to run certain classical adventure and role-playing games
  provided the games' data files are available
- [slides](https://github.com/maaslalani/slides):
  Terminal-based tool for presentations using Markdown files, with
  different slides separated by `---` lines
- [Sloth](https://sveinbjorn.org/sloth)
  ([Github](https://github.com/sveinbjornt/Sloth)):
  macOS application to show open files, dirs, sockets and pipes
- [Stacer](https://github.com/oguzhaninan/Stacer):
  Linux system optimizer and monitoring GUI tool
- [Stow](https://www.gnu.org/software/stow/):
  Symlink farm manager, useful for managing dotfiles
- [Sweet Home 3D](https://www.sweethome3d.com/)
  ([Sourceforge](https://sourceforge.net/projects/sweethome3d/)):
  Open-source interior design application, also available as an online
  [webapp](https://www.sweethome3d.com/SweetHome3DOnlineManager.jsp)
- [Tizonia](https://tizonia.org/)
  ([Github](https://github.com/tizonia/tizonia-openmax-il)):
  Command-line cloud music player
- [TomatoBar](https://github.com/ivoronin/TomatoBar):
  macOS menu bar Pomodoro timer
- [ueli](https://ueli.app/)
  ([Github](https://github.com/oliverschwendener/ueli)):
  Launcher like [Alfred](https://www.alfredapp.com/) but open-source,
  available for macOS and Windows
- [Zotero](https://www.zotero.org/):
  Reference management software to collect, organize, cite and share
  research material

## Linux notes

Some notes also apply to BSD systems.

### Linux development environment in ChromeOS

Linux containers and VMs for ChromeOS are made available via the
[Crostini](https://chromium.googlesource.com/chromiumos/docs/+/HEAD/containers_and_vms.md)
framework.

More information can also be found on
[Reddit](https://www.reddit.com/r/Crostini/).

- **Enabling**: Go to
  `Settings > Developers (under Advanced) > Linux Development Environment`
  and enable
- **Terminal settings**: `Ctrl-Shift-p` in the Linux terminal
- **Interacting with the Wayland clipboard**: Copying and pasting in
  and out of the Crostini container via the Wayland clipboard in
  ChromeOS (Wayland is the display manager) can be done using
  [wl-clipboard](https://github.com/bugaevc/wl-clipboard) (see
  [link](https://www.reddit.com/r/chromeos/comments/kf3fxo/linux_beta_penguin_how_to_copy_file_contents_from))
  version `2.0.0` or greater (note that the Crostini terminal has
  native support for the Wayland clipboard, in that selecting a region
  will copy to the clipboard and `Ctrl-Shift-v` will paste from the
  clipboard)
- **Flatpak issues**: Flatpak may need security nesting enabled to run
  properly. If not enabled, errors may surface like the following.

  ```console
  $ flatpak run org.some.program
  bwrap: Can't mount proc on /newroot/proc: Operation not permitted
  error: ldconfig failed, exit status 256
  ```

  To enable security nesting, in ChromeOS do `Ctrl-Alt-t` to open
  a crosh terminal, run

  ```sh
  vsh termina
  lxc config set penguin security.nesting true
  ```

  and restart the computer so the new setting takes effect.

## Mac notes

### Basic CLI development environment using conda

Conda can provide a basic CLI dev environment without XCode or its command-line
tools installed. Some conda packages for this:

- `bat`
- `clang`
- `gawk`
- `git`
- `go`
- `htop`
- `postgresql`
- `python` and many packages in [PyPI](https://pypi.org/)
- `r-base` and some [CRAN](https://cran.r-project.org/) packages
- `ripgrep`
- `sed`
- `vim`
- `visidata`

Notes:

- R packages begin with an `r-` prefix but only popular packages are available
- Conda has an `emacs` package, but go with one of the standalone Emacs
  applications ([Emacs for Mac OSX](https://emacsformacosx.com/) or the
  [emacs-mac port](https://github.com/railwaycat/homebrew-emacsmacport)) as they
  are better integrated with the system

#### Installation

Mambaforge. Assumes `$HOME/.local/bin` is in `$PATH` and `$HOME/.zshrc` exists.

```sh
# Install mambaforge for faster operations
# $ chmod +x Mambaforge-MacOSX-arm64.sh
# $ ./Mambaforge-MacOSX-arm64.sh                     # interactive install
# $ eval "$(~/mambaforge/bin/conda shell.zsh hook)"  # modify path as needed
# $ conda init
# $ conda config --set auto_activate_base false
# $ mamba init
# Create basic tools environment rather than polluting base environment
mamba create -n myenv
mamba activate myenv
mamba install bat git htop ripgrep tmux
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/myenv/bin/bat .
ln -s $HOME/mambaforge/envs/myenv/bin/git .
ln -s $HOME/mambaforge/envs/myenv/bin/htop .
ln -s $HOME/mambaforge/envs/myenv/bin/rg .
ln -s $HOME/mambaforge/envs/myenv/bin/tmux .
mamba deactivate
# Create new environments as needed for each project, e.g.
# > mamba create -n some-project python=3.9
```

#### LaTeX tools setup

Use Tectonic for LaTeX compilation.

As of Jan 2022, there are no Pandoc binaries packaged for Darwin
aarch64 (although Homebrew offers an option), so alternatives are
listed below.

```sh
mamba create -n latextools
mamba activate latextools
mamba install tectonic
mamba deactivate
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/latextools/bin/tectonic .
```

#### Ruby tools setup

Setup useful Ruby tools.

- Asciidoctor for processing Asciidoc files, with extensions for
  conversion to PDF, PDF optimization and syntax highlighting
- Kramdown for processing Markdown files (of the Kramdown flavor), as
  well as a converter to Asciidoc

```sh
mamba create -n rbtools
mamba activate rbtools
mamba install ruby
gem install asciidoctor
gem install asciidoctor-pdf rghost rouge
gem install kramdown kramdown-asciidoc
mamba deactivate
```

Create the following files with the given contents.

`$HOME/.local/bin/asciidoctor`:

```sh
#!/bin/zsh
source $HOME/mambaforge/etc/profile.d/conda.sh
source $HOME/mambaforge/etc/profile.d/mamba.sh
mamba activate rbtools
asciidoctor "$@"
```

`$HOME/.local/bin/kramdown`:

```sh
#!/bin/zsh
source $HOME/mambaforge/etc/profile.d/conda.sh
source $HOME/mambaforge/etc/profile.d/mamba.sh
mamba activate rbtools
kramdown "$@"
```

`$HOME/.local/bin/kramdoc`:

```sh
#!/bin/zsh
source $HOME/mambaforge/etc/profile.d/conda.sh
source $HOME/mambaforge/etc/profile.d/mamba.sh
mamba activate rbtools
kramdoc "$@"
```

Make the files executable so they are easy to run on the command-line.

```sh
cd $HOME/.local/bin
chmod +x asciidoctor
chmod +x kramdown
chmod +x kramdoc
```

#### Python development setup

Environment-independent Python development tools.

```sh
mamba create -n pydevtools python=3.9
mamba activate pydevtools
mamba install jedi-language-server black
mamba deactivate
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/pyenv/jedi-language-server
ln -s $HOME/mambaforge/envs/pyenv/black
```

#### Installing GPU ML libraries for darwin-aarch64 machines

Metal-enabled [Tensorflow](https://www.tensorflow.org/)
([link](https://developer.apple.com/metal/tensorflow-plugin/)),
currently supports Python `3.8` and `3.9`.

Note that `tensorflow-deps` versions follow base Tensorflow versions,
so if using Tensorflow `2.6.X`, install `tensorflow-deps==2.6.0`.

```sh
mamba create -n tfenv python=3.8
mamba activate tfenv
conda config --env --add channels apple
mamba install -c apple tensorflow-deps
pip install tensorflow-metal
pip install tensorflow-macos
```

Optional data.

```sh
pip install tensorflow-datasets
```

Check that tensorflow is using the GPU.
The following should return a non-empty list.

```python
import tensorflow as tf
tf.config.list_physical_devices("GPU")
```

Upgrading Metal-enabled Tensorflow by uninstalling and re-installing.
Note that it is usually better to create a new environment and
installing from scratch there instead.

```sh
mamba activate tfenv
pip uninstall tensorflow-macos
pip uninstall tensorflow-metal
mamba install -c apple tensorflow-deps --force-reinstall
pip install tensorflow-metal
pip install tensorflow-macos 
```

### Installing XCode command-line tools

```sh
sudo rm -rf /Library/Developer/CommandLineTools
sudo xcode-select --install
```

### Running virtual machines with UTM on M1 Macs

Notes for using virtual machines via [UTM](https://mac.getutm.app/)
([Github](https://github.com/utmapp/UTM)) on M1 macOS machines.

#### Usage notes

- `Ctrl-Option` toggles "capture mouse cursor" (if using Emacs in the
  virtual machine, it is better to enable "Use Command+Opt for input
  capture/release" to change the `Ctrl-Option` binding to
  `Command-Option` instead in the UTM app preferences)
- The default networking mode for VMs is shared networking, in which
  the host machine acts as a virtual router for the virtual machine.
  Under this networking mode, run `ip addr` in the virtual machine to
  find the IP address of the virtual machine (which should look like
  `192.168.XXX.YYY`, and can used to ssh from the host into the VM).
  Using bridged mode is similar, except IP address allocation is
  deferred to the upstream router instead of using the host machine.
  For more info, see [link](https://kb.parallels.com/4948).
- If networking in the VM stops working, sometimes it is because the
  network configuration becomes invalid (e.g. if the MAC address
  changes). To temporarily re-acquire an IP address, first figure out
  the interface using `ip addr` (the name is something like `enp0s10`)
  and run `sudo dhclient <INTERFACE>` to (re-)acquire an IP address. A
  more permanent solution involves installing the `dhcpcd5` package
  which is a DHCP client daemon via `sudo apt install dhcpcd5` (for
  more info about this utility, see
  [here](https://roy.marples.name/projects/dhcpcd/)). See
  [here](https://unix.stackexchange.com/questions/534184/network-not-working-in-a-qemu-kvm-virtual-machine-running-arch-linux)
  for more info about this topic in general (dicussion is for
  ArchLinux but the concepts are similar)

#### Useful links

- [UTM Wiki](https://github.com/utmapp/UTM/wiki)
- [Install Ubuntu ARM64 on Apple M1](https://github.com/utmapp/UTM/wiki/Install%20Ubuntu%20ARM64%20on%20Apple%20M1)
- [UTM Linux VM tips](https://github.com/utmapp/UTM/wiki/Useful-tips#linux)
- [OpenVPN 3 Linux](https://community.openvpn.net/openvpn/wiki/OpenVPN3Linux)

#### Debian install example

This assumes a fresh install of Debian (i.e., download the Debian
aarch64 ISO and use that to install a new VM in UTM).

```sh
apt install git
apt install spice-vdagent spice-webdavd
systemctl enable spice-vdagentd
apt install flatpak gnome-software-plugin-flatpak
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub org.gnu.emacs
flatpak install flathub org.jaspstats.JASP
flatpak install flathub org.chromium.Chromium
apt install openjdk-11-jre
```

Disable sleep and suspend by creating a file
`/etc/systemd/sleep.conf.d/nosuspend.conf` with the following
contents.

```ini
[Sleep]
AllowSuspend=no
AllowHibernation=no
AllowSuspendThenHibernate=no
AllowHybridSleep=no
```

Install OpenVPN 3 (replace `bullseye` with the appropriate distro per
[this](https://community.openvpn.net/openvpn/wiki/OpenVPN3Linux#DebianUbuntu)
table).

```sh
apt install curl stow
apt install apt-transport-https
curl -fsSL https://swupdate.openvpn.net/repos/openvpn-repo-pkg-key.pub | gpg --dearmor > /etc/apt/trusted.gpg.d/openvpn-repo-pkg-keyring.gpg
curl -fsSL https://swupdate.openvpn.net/community/openvpn3/repos/openvpn3-bullseye.list >/etc/apt/sources.list.d/openvpn3.list
apt update
apt install openvpn3
```

Other software to download:

- [btop](https://github.com/aristocratos/btop)
- [Iosevka](https://github.com/be5invis/Iosevka) font (at least Aile,
  regular if needed)
- [DBeaver](https://github.com/dbeaver/dbeaver) (community edition,
  aarch64-nojdk, make a copy of `dbeaver-ce.desktop` edited with the
  correct paths and save it to `$HOME/.local/share/applications/`)
- [Mambaforge](https://github.com/conda-forge/miniforge) (aarch64)

Config changes:

- In `Default Applications`, change the default browser to Chromium

#### Enabling retina resolution for Macbooks

Install `spice-vdagent` and `spice-webdavd` using the system package manager.

Make the following modifications to the VM settings.

> Edit > Display > Resolution > Enable "Fit to screen" and
> "Retina mode"

Modify UTM preferences (Command-,) so native solution is always used.

> Scaling > Enable "Always use native (HiDPI) resolution"

Run the virtual machine at fullscreen, and in the guest OS scale all
UI elements to 200% (the following is for a system running GNOME).

> Settings app > Displays > Scale > Set to 200%

#### Shared drives

Make the following modifications to the VM settings.

> Check "Enable Directory Sharing" in Edit > Sharing > Shared Directory

Attach a shared directory to the VM in the UTM admin window.

In the running VM session, the shared directory is available as a
WebDAV service at `http://localhost:9843` with no credentials needed.

This can be attached as a network drive in GNOME Files. Once the
network drive is attached, it is accessible either through a GNOME
application or the filesystem at some mounted directory within
`${XDG_RUNTIME}/gvfs/` (`$XDG_RUNTIME` is usually `/run/user/<USERID>`
and the mounted directory name is something like
`dav+sd:host=Spice%2520client%2520folder._webdav._tcp.local`).

Alternatively, it can be mounted to a directory using instructions
listed
[here](https://github.com/utmapp/UTM/wiki/Useful-tips#mounting-webdav-shares)
(note that for an M1 machine, install the `davfs2` package rather than
the `dav2fs` package).

## GnuPG

### Exporting ASCII-armored keys as QR codes

First run `gpg --list-keys --with-subkey-fingerprint` to list the keys plus
subkeys along with their fingerprints. Identify the encrypting subkey, if one
exists, by the usage flag `[E]` (otherwise, create one or use the master key),
and note its fingerprint.

The subkey and the public key can be exported in ASCII-armored format to QR
code images as below (modifying appropriately `<KEY_EMAIL>` and
`<SUBKEY_FINGERPRINT_WITHOUT_SPACES>`).

```sh
mkdir -p /tmp/keys-export && cd $_
# export public key to pubkey-??.qr QR code image files
gpg --export --armor <KEY_EMAIL> > pub.key
split -C 2500 pub.key pubkey-
for file in pubkey-??; do <"$file" qrencode -s 3 -d 150 -o "$file".qr; done
# export secret subkey to subkey-??.qr QR code image files
gpg --export --armor <SUBKEY_FINGERPRINT_WITHOUT_SPACES> > sub.key
split -C 2500 sub.key subkey-
for file in subkey-??; do <"$file" qrencode -s 3 -d 150 -o "$file".qr; done
```

The `*.qr` files can be viewed in a web browser, and scanned in as desired.
Delete the `/tmp/keys-export/` folder when done.

Adapted from [here](https://jherrlin.github.io/posts/emacs-gnupg-and-pass/).

### Extending key expirations

1. Find the expiring key ID using `gpg --list-keys` (the key ID comes
   after the slash)
1. Edit the key using `gpg --edit-key KEY_ID`
1. View key and subkeys with the `list` command, where selected
   keys/subkeys have and asterisk next to them
1. Select/unselect a key using `key N` where `N` is the number of the
   desired key in the list
1. The command `expire` will allow the interactive selection of a new
   expiry.
1. Repeat as needed (may not be needed if multiple keys are selected
   and their expirations extended as above)
1. Check new expirations using `list`
1. Save with the `save` command
1. Publish/disseminate the updated keys as appropriate

Run `help` while in the GPG shell for additional commands available.

## Git

### Credentials using .netrc file

Many command-line tools integrate with `.netrc` files. It is possible
to use this together with GnuPG to create an encrypted version
`.netrc.gpg` and use it with Git.

Emacs
[auth-source](https://www.gnu.org/software/emacs/manual/html_mono/auth.html)
users may want to use `$HOME/.authinfo` instead of `$HOME/.netrc`, in
which case references to `$HOME/.netrc` and `$HOME/.netrc.gpg` below
should be changed to `$HOME/.authinfo` and `$HOME/.authinfo.gpg`
respectively.


1. Get `git-credential-netrc` and put it somewhere on `$PATH`.

   ```sh
   mkdir -p $HOME/packages/git-credential-netrc
   cd $HOME/packages/git-credential-netrc
   wget https://raw.githubusercontent.com/git/git/master/contrib/credential/netrc/git-credential-netrc.perl
   chmod +x git-credential-netrc.perl
   cd $HOME/.local/bin
   ln -s $HOME/packages/git-credential-netrc/git-credential-netrc.perl git-credential-netrc
   ```

1. Create a `$HOME/.netrc` file with credentials as follows.

   ```text
   machine some.server.com login some_login password some_password
   machine another.server.com login another_login password another_password
   ```

1. Create or import a GPG key (not shown here). Create an encrypted
   version of the credentials file `$HOME/.netrc.gpg` and delete the
   unencrypted version.

   ```sh
   gpg -e -r the_recipient $HOME/.netrc
   rm $HOME/.netrc
   ```

1. Configure git to use the encrypted credentials locally in a
   repository. Change `--local` to `--global` to use the encrypted
   credentials globally.

   ```sh
   git config --local credential.helper "netrc -f ~/.netrc.gpg -v"
   ```

   Adapted from
   [here](https://grahamlopez.org/git/git_credentials.html) and
   [here](https://andrearichiardi.com/blog/posts/git-credential-netrc.html).

**NOTES**:

- Requires Perl with the Git module installed.
- For Github users, this approach is good for storing tokens but using
  SSH keys is also fine if granular permissioning is not required.

### Syncing a fork with upstream

To sync a fork with upstream:

1. Configure a remote that points to the upstream repository.

   ```sh
   git remote add upstream \
     https://github.com/ORIGINAL_OWNER/ORIGINAL_REPOSITORY.git
   ```

   Check that the remote is configured properly using `git remote -v`.

   For more info, see the following
   [link](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/configuring-a-remote-for-a-fork).

1. Fetch and merge upstream commits.

   ```sh
   git fetch upstream
   git checkout master
   git merge upstream/master
   ```

   For more info, see the following
   [link](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/syncing-a-fork).

Note that newer repositories have their primary branch names as `main`
instead of `master`.

# Software notes

## Open-source software

- Audio and video creation and editing
  - [Ardour](https://ardour.org/) or
    [Zrythm](https://www.zrythm.org/):
    Digital audio workstation; Ardour works with
    [ReaPlugs](https://www.reaper.fm/reaplugs/) VST plugins; Ardour
    seems more suited to audio recording and engineering, while Zrythm
    seems more suited to MIDI electronic music production
  - [Kdenlive](https://kdenlive.org/en/):
    Video editor
  - [LMMS](https://lmms.io/):
    Music sequencer
  - [sfxr-qt](https://github.com/agateau/sfxr-qt):
    SFX creator for games; Qt port of
    [SFXR](http://www.drpetter.se/project_sfxr.html);
    [JavaScript](https://github.com/chr15m/jsfxr) port available
    [online](https://sfxr.me/); also built into LMMS
  - [Synfig](https://github.com/synfig/synfig/):
    2D vector animation
- Backup
  - [BorgBackup](https://www.borgbackup.org/):
    File backup tool; frontends available like
    [Vorta](https://vorta.borgbase.com/)
  - [dar](http://dar.linux.free.fr/):
    File archiving, supports recovery records using
    [par2](https://github.com/Parchive/par2cmdline/)
  - [Timeshift](https://teejeetech.com/timeshift/):
    Like Windows System Restore but for Linux
- Containerization
  - [ctop](https://github.com/bcicen/ctop):
    `top` for containers
  - [Docker](https://docs.docker.com/)
- Data analytics apps
  - [JASP](https://jasp-stats.org/):
    GUI statistical analysis tool
  - [Netron](https://github.com/lutzroeder/netron):
    Neural network, deep learning and machine learning model viewer
  - [PSPP](https://www.gnu.org/software/pspp/):
    Open-source SPSS alternative
  - [Veusz](https://veusz.github.io/):
    Scientific plotting and graphing
- Data pipelines
  - [Apache Airflow](https://airflow.apache.org/):
    Scheduling workflows
  - [Apache HOP](https://hop.apache.org/):
    Fork of [Kettle/PDI](https://github.com/pentaho/pentaho-kettle)
  - [Snakemake](https://snakemake.readthedocs.io/en/stable/) or
    [Nextflow](https://www.nextflow.io/)
    Run batch workflows
- Database clients
  - [Altair](https://altair.sirmuel.design/):
    GraphQL client
  - [DBeaver](https://dbeaver.io/):
    Client for any database that has a jDBC driver
  - [DataStation](https://github.com/multiprocessio/datastation):
    Data IDE
  - [DbGate](https://dbgate.org/):
    Client for MySQL, PostgreSQL, SQL Server, MongoDB, Redis, SQLite,
    Amazon Redshift, CockroachDB, MariaDB databases
  - [Jailer](https://github.com/Wisser/Jailer):
    Subset databases and browse relational data
  - [OctoSQL](https://github.com/cube2222/octosql):
    Make queries that can span MySQL, PostgreSQL databases along with
    CSV and JSON
  - [pgcli](https://www.pgcli.com/):
    PostgreSQL command-line client
  - [q](https://github.com/harelba/q):
    Command-line tool for running SQL directly on CSV files
  - [RedisDesktopManager](https://github.com/uglide/RedisDesktopManager):
    Redis client
  - [Sequel Ace](https://github.com/Sequel-Ace/Sequel-Ace):
    MySQL, MariaDB client; macOS
- Databases
  - [ClickHouse](https://github.com/ClickHouse/ClickHouse):
    Time-series DB
  - [PostgreSQL](https://www.postgresql.org/):
    Relational DB; use
    [TimescaleDB](https://github.com/timescale/timescaledb)
    plugin to better support time-series
- Desktop publishing
  - [Scribus](https://www.scribus.net/)
- Diagramming and image editing
  - [ASCIIFlow](https://github.com/lewish/asciiflow):
    Web-based ASCII drawing application. Can be run locally
  - [Caire](https://github.com/esimov/caire):
    Smart image resizing
  - [drawio-desktop](https://github.com/jgraph/drawio-desktop) or
    [yEd](https://www.yworks.com/products/yed):
    Vector diagramming; drawio-desktop is the Electron build of
    [diagrams.net](https://www.diagrams.net/)
  - [GIMP](https://www.gimp.org/) or
    [Krita](https://krita.org/en/):
    Raster graphics editor
  - [Inkscape](https://inkscape.org/):
    Vector graphics editor
  - [LinkedIdeas](https://github.com/fespinoza/LinkedIdeas):
    Mind-mapping tool
  - [Pikchr](https://pikchr.org/) or
    [PlantUML](https://plantuml.com/):
    Markup language for diagramming, standalone or supported by
    Markdown processers like Pandoc via plugins
- Digital design
  - [Penpot](https://github.com/penpot/penpot):
    Open source design and prototyping platform like Sketch and Figma
- Document conversion and rendering
  - [Asciidoctor](https://asciidoctor.org/):
    Asciidoc processor, has an
    [extension](https://asciidoctor.org/docs/asciidoctor-pdf/)
    for PDF generation
  - [groff](https://www.gnu.org/software/groff/):
    Typesetting system that can output PS, PDF, HTML or DVI;
    a lightweight alternative to (La)TeX but less popular
  - [hred](https://github.com/danburzo/hred):
    Command-line HTML (and XML) to JSON converter
  - [Kramdown](https://kramdown.gettalong.org/):
    Markdown processor, has an
    [Asciidoc converter](https://github.com/asciidoctor/kramdown-asciidoc)
  - [Pandoc](https://pandoc.org/):
    Document converter, can also process Markdown
  - [shot-scraper](https://github.com/simonw/shot-scraper):
    Command-line utility for taking partial or full screenshots of
    websites
  - [Tectonic](https://tectonic-typesetting.github.io/) or
    [TeXLive](https://www.tug.org/texlive/) or
    [TinyTeX](https://github.com/yihui/tinytex):
    (La)TeX processor
  - [WeasyPrint](https://github.com/Kozea/WeasyPrint) or
    [percollate](https://github.com/danburzo/percollate):
    Convert HTML to PDF files; percollate processes the page for
    readability before conversion; the file can also be directly
    opened in a browser and printed to PDF
- Document text extraction
  - [Apache Tika](http://tika.apache.org/):
    [Go](https://github.com/google/go-tika)
    [Python](https://github.com/chrismattmann/tika-python) bindings
    available
  - [Tesseract](https://github.com/tesseract-ocr/tesseract)
- Ebook readers and library managers
  - [Calibre](https://calibre-ebook.com/)
  - [epr](https://github.com/wustho/epr) or
    [epy](https://github.com/wustho/epy):
    TUI ebook reader; epr supports only EPUB; epy is a fork of epy
    with support for more formats like MOBI and AZW3, and adds
    features like bookmarks, integration with external dictionaries,
    and inline formatting
  - [epub2txt2](https://github.com/kevinboone/epub2txt2):
    Extract text from EPUB
  - [Foliate](https://johnfactotum.github.io/foliate/)
- Email
  - [aerc](https://git.sr.ht/~rjarry/aerc):
    TUI email client with [notmuch](https://notmuchmail.org/) support.
    See the appropriate section below for how to set up Notmuch,
    [Lieer](https://github.com/gauteh/lieer) and aerc for Gmail usage.
  - [Apple Mail](https://support.apple.com/mail) or
    [Evolution](https://wiki.gnome.org/Apps/Evolution) or
    [Thunderbird](https://www.thunderbird.net/):
    Thunderbird is a cross-platform graphical email client and has a
    soft fork [Betterbird](https://www.betterbird.eu/) that applies
    patches atop the ESR version; Apple Mail (macOS) and Evolution
    (Linux) are platform-specific but are better integrated with the
    OS and have better support for Microsoft Exchange and Office 365
- File management
  - [Cryptomator](https://cryptomator.org/)
    ([Github](https://github.com/cryptomator)):
    Provides client-side encryption of local or cloud data (relevant
    sync software needs to be installed on the machine), surfacing it
    as a virtual drive; cross-platform, including iOS and Android;
    compatible with first-party cloud storage provider software and
    its vaults are interoperable with Mountain Duck vaults; not
    compatible with `rclone mount`
  - [Cyberduck](https://cyberduck.io/):
    Cross-platform remote FTP, SFTP, WebDAV, cloud storage; has a CLI
    tool [duck.sh](https://duck.sh/)
  - [Double Commander](https://doublecmd.sourceforge.io/):
    GUI Midnight Commander clone
  - [Magic Wormhole](https://github.com/magic-wormhole/magic-wormhole):
    Command-line tool and library for sending files from one computer
    to another; note that this tool requires
    [mailbox](https://github.com/magic-wormhole/magic-wormhole-mailbox-server)
    and a
    [transit relay](https://github.com/magic-wormhole/magic-wormhole-transit-relay)
    server, which by default uses one hosted by the project
  - [Midnight Commander](https://midnight-commander.org/) or:
    [nnn](https://github.com/jarun/nnn) or
    [lf](https://github.com/gokcehan/lf) or
    [broot](https://github.com/Canop/broot):
    Terminal file manager
  - [rclone](https://rclone.org/)
    ([Github](https://github.com/rclone/rclone)):
    Like `rsync` but for cloud storage; `rclone mount` can be used to
    mount cloud storage to a filesystem mount point
  - [rdfind](https://github.com/pauldreik/rdfind) or
    [fdupes](https://github.com/adrianlopezroche/fdupes):
    Command-line tool to find duplicate files
  - [renameutils](https://www.nongnu.org/renameutils/) or
    [f2](https://github.com/ayoisaiah/f2) or
    [KRename](https://userbase.kde.org/KRename) or
    [GPRename](http://gprename.sourceforge.net/) or
    [Szyszka](https://github.com/qarmin/szyszka):
    Rename lots of files easily
  - [Stow](https://www.gnu.org/software/stow/):
    Symlink farm manager, useful for managing dotfiles
  - [Syncthing](https://syncthing.net/):
    Continous file synchronization; macOS
    [frontend](https://github.com/syncthing/syncthing-macos)
    available
  - [tree](http://mama.indstate.edu/users/ice/tree/):
    Command-line tool to list files in subdir tree depth-indented
- Newsreaders
  - [NetNewsWire](https://github.com/Ranchero-Software/NetNewsWire):
    RSS reader; iOS and macOS
  - [Pan](http://pan.rebelbase.com/):
    Usenet reader
- Package/runtime managers
  - [asdf](https://asdf-vm.com/):
    Language runtime manager
  - [Flatpak](https://flatpak.org/):
    Application manager; [Flathub](https://flathub.org/) is the de
    facto repository; for easier permissioning control install
    [Flathub](https://flathub.org/apps/details/com.github.tchx84.Flatseal)
  - [MacPorts](https://www.macports.org/) or
    [Spack](https://spack.io/) or
    [Homebrew](https://brew.sh/):
    macOS package manager for open-source software
  - [Miniforge](https://github.com/conda-forge/miniforge):
    [Conda](https://conda.io/) pre-configured for
    [conda-forge](https://conda-forge.org/); the Mambaforge variant
    recommended as it comes with the Conda drop-in replacement
    [Mamba](https://github.com/mamba-org/mamba) that is much faster
- PDF readers and tools
  - [diffpdf](https://tracker.debian.org/pkg/diffpdf)
    ([source](http://www.qtrac.eu/diffpdf-foss.html)):
    Diff two PDF files, Can be used with `git` by setting
    `git config --global difftool.diffpdf.cmd 'diffpdf "$LOCAL" "$REMOTE"'`
    and `git config --global alias.diffpdf "difftool -t diffpdf"` and
    running e.g. `git diffpdf somecommit:somefile.pdf somefile.pdf`;
    Linux
  - [MuPDF](https://mupdf.com/):
    PDF reader
  - [QPDF](https://github.com/qpdf/qpdf) or
    [pdfcpu](https://pdfcpu.io/)
    ([Github](https://github.com/pdfcpu/pdfcpu)) or
    [MuTool](https://mupdf.com/docs/mutool.html)
    (part of [MuPDF](https://mupdf.com/), may be packaged separately
    like in Debian where it is in the `mupdf-tools` package):
    PDF transformations and processing
  - [sioyek](https://github.com/ahrm/sioyek) or
    [Zathura](https://pwmt.org/projects/zathura/):
    PDF reader for research and technical PDFs
  - [Skim](https://skim-app.sourceforge.io/):
    PDF reader and annotator; macOS; the built-in Apple
    [Preview](https://support.apple.com/guide/preview/welcome/mac) on
    macOS is usually good enough
  - [Xournal++](https://github.com/xournalpp/xournalpp):
    Handwriting notebook with PDF annotation support
- Presentations
  - [Marp](https://marp.app/):
    Markdown-based presentations; use via the command-line tool
    [marp-cli](https://github.com/marp-team/marp-cli)
  - [pdfpc](https://github.com/pdfpc/pdfpc):
    Multi-monitor PDF presentations
  - [slides](https://github.com/maaslalani/slides):
    Terminal-based presentations using Markdown files
- Project (codebase) tools
  - [Cookiecutter](https://github.com/cookiecutter/cookiecutter)
    Project templates
  - [DevSkim](https://github.com/microsoft/DevSkim) or
    [Semgrep](https://github.com/returntocorp/semgrep):
    Code security linter for multiple languages, with a command-line
    interface; ones for specific languages or tools also exist
    (e.g., [C++](https://github.com/david-a-wheeler/flawfinder),
    [Go](https://github.com/securego/gosec),
    [Kubernetes](https://github.com/controlplaneio/kubesec),
    [Ruby on Rails](https://github.com/presidentbeef/brakeman/),
    [Terraform](https://github.com/aquasecurity/tfsec)); there are
    also [other](https://github.com/Microsoft/ApplicationInspector)
    [tools](https://github.com/googleprojectzero/weggli) that help
    analyze what code does instead of looking for specific code smells
  - [EditorConfig](https://editorconfig.org/)
    ([Github](https://github.com/editorconfig/)):
    Define formatting conventions for project code and text files,
    with multiple implementations and editor plugins to apply them
    (install base command with `apt install editorconfig` on Debian or
    Ubuntu, `port install editorconfig-core-c` via MacPorts on macOS)
  - [gitg](https://gitlab.gnome.org/GNOME/gitg) or
    [Gitup](https://github.com/git-up/GitUp) or
    [TortoiseGit](https://tortoisegit.org/):
    [Git](https://git-scm.com/) GUI client; gitg is Linux-only (and is
    on Flathub), Gitup is macOS-only, TortoiseGit is Windows-only
  - [gogs](https://gogs.io/) or
    [Soft Serve](https://github.com/charmbracelet/soft-serve):
    Self-hosted Git server
  - [kondo](https://github.com/tbillington/kondo):
    Clean unneeded files like build artifacts from project directories
  - [Kythe](https://kythe.io/)
    ([Github](https://github.com/kythe/kythe)):
    Pluggable system for building code tools, comes with indexer
    implementations for C++, Go and Java and other tools; see
    [link](https://european-lisp-symposium.org/static/2020/godbout-slides.pdf)
  - [onefetch](https://github.com/o2sh/onefetch):
    Git repository information and statistics
  - [ruplacer](https://github.com/your-tools/ruplacer):
    Find and replace recursively, like a combo of `find` and `sed -i`
  - [scc](https://github.com/boyter/scc) or
    [tokei](https://github.com/XAMPPRocky/tokei):
    Count lines of code by language
  - [task](https://github.com/go-task/task):
    Easier-to-use [Make](https://www.gnu.org/software/make/)
    alternative
- Remote login and desktop
  - [Mosh](https://mosh.org/):
    Robust SSH alternative
  - [Remmina](https://remmina.org/):
    Remote desktop client for POSIX systems
- Search
  - [Code Search](https://github.com/google/codesearch) or
    [hound](https://github.com/hound-search/hound) or
    [zoekt](https://github.com/sourcegraph/zoekt):
    Text search engine, good for source code; codesearch and hound are
    based on [this paper](https://swtch.com/~rsc/regexp/regexp4.html);
    codebase and zoekt have CLI interfaces, while hound and zoekt
    (using zoekt-webserver) have web interfaces; hound and zoekt are
    more featureful, e.g., works with various Git hosts; see the
    **Working with large codebases** section below for a good use case
  - [Meilisearch](https://github.com/meilisearch/meilisearch) or
    [ElasticSearch](https://github.com/elastic/elasticsearch) or
    [Toshi](https://github.com/toshi-search/Toshi):
    Search engine server; ElasticSearch and Toshi are designed for
    general-purpose search, while Meilisearch focuses on simpler
    searches (algorithm is inverse-index to Levenshtein automaton
    for handling typos to bucket sort for ranking documents)
  - [fd](https://github.com/sharkdp/fd):
    Simpler command-line alternative to `find`
  - [fzf](https://github.com/junegunn/fzf):
    Command-line fuzzy finder
  - [pdfgrep](https://pdfgrep.org/):
    Command-line tool for searching text in PDF files
  - [ripgrep](https://github.com/BurntSushi/ripgrep):
    `grep` alternative, extend to more file formats with
    [ripgrep-all](https://github.com/phiresky/ripgrep-all)
- Shell enhancements
  - [tldr](https://github.com/tldr-pages/tldr) or
    [navi](https://github.com/denisidoro/navi):
    Interactive command references on the command-line; tldr has
    alternative client implementations
    [tealdeer](https://github.com/dbrgn/tealdeer) or
    [outfieldr](https://gitlab.com/ve-nt/outfieldr) that run much
    faster than the usual Node.js and Python clients
  - [z](https://github.com/rupa/z) or
    [zoxide](https://github.com/ajeetdsouza/zoxide):
    Frecent directories; `z` is a plugin for Bash and Zsh, whereas
    `zoxide` is more of a replacement for the `cd` command
- System administration
  - [angle-grinder](https://github.com/rcoh/angle-grinder)
    (log analyzer)
  - [atop](https://github.com/Atoptool/atop):
    System resource monitor, runs as a daemon that logs process
    activity to disk
  - [bandwhich](https://github.com/imsnif/bandwhich):
    Network utilization by process, connection, remote IP, hostname,
    and so on
  - [dua](https://github.com/Byron/dua-cli) or
    [dust](https://github.com/bootandy/dust):
    Alternative to `du` for checking disk usage. dua also makes
    deletion of unwanted data easy
  - [duf](https://github.com/muesli/duf) or
    [lfs](https://github.com/Canop/lfs):
    Command-line alternative to `df` with nicer user interface
  - [fkill](https://github.com/sindresorhus/fkill-cli):
    Command-line tool to interactively kill running user and system
    procs
  - [forkstat](https://github.com/ColinIanKing/forkstat):
    Command-line program to log process forks, execs and exits;
    useful for tracking runaway processes
  - [htop](https://github.com/htop-dev/htop) or
    [zenith](https://github.com/bvaisvil/zenith) or
    [bottom](https://github.com/ClementTsang/bottom):
    System resource monitor, alternative to `top`; note that dumb
    terminals or logging ncurses should not be used, in which case do
    `top -b` to run `top` in batch mode (or `top -b -n NUMBER` to
    limit the number of iterations to `NUMBER`)
  - [lnav](https://lnav.org/)
    ([Github](https://github.com/tstack/lnav)):
    Log file viewer supporting multiple file formats and compression
    types, and can consolidate multiple log files into a single view
  - [Sloth](https://sveinbjorn.org/sloth):
    macOS application to show open files, dirs, sockets and pipes
  - [Stacer](https://github.com/oguzhaninan/Stacer):
    Linux system optimizer and monitoring GUI tool
  - [eBPF/bcc](https://github.com/iovisor/bcc) or
    [strace](https://strace.io/):
    Process debugging;
    eBPF/bcc [tutorial](https://ish-ar.io/python-ebpf-tracing/)
  - [hyperfine](https://github.com/sharkdp/hyperfine):
    Command-line benchmarking tool that supports multiple and warmup
    runs, export of results to various formats, etc; basically a
    featureful alternative to the standard `time` command
- Text editors and integrated development environments
  - [Acme](https://en.wikipedia.org/wiki/Acme_(text_editor)):
    Mouse-driven GUI text editor from
    [Plan 9](https://en.wikipedia.org/wiki/Plan_9_from_Bell_Labs),
    ported to Linux and macOS
    ([link](https://9fans.github.io/plan9port/),
    [Github](https://github.com/9fans/plan9port))
  - [Emacs](https://www.gnu.org/software/emacs/):
    Extensible TUI and GUI text editor, macOS builds available from
    [here](https://emacsformacosx.com/) or
    [here](https://github.com/railwaycat/homebrew-emacsmacport/releases)
    or install via some package manager like
    MacPorts (e.g., `port install emacs-mac-app +nativecomp` which
    compiles a native GUI Emacs application, the native-comp variant,
    at `/Applications/MacPorts/EmacsMac.app` or at
    `/Users/Username/Applications/MacPorts/EmacsMac.app`)
  - [Helix](https://helix-editor.com/):
    TUI editor like Kakoune, but designed to have many editor features
    like [LSP](https://microsoft.github.io/language-server-protocol/)
    support built into the editor
  - [Kakoune](https://kakoune.org/):
    TUI editor like vi, but implements an object-verb command model
    rather than vi's verb-object command-model; designed to call out
    to the shell for most features beyond basic editing
  - [Lite XL](https://lite-xl.com/)
    ([Github](https://github.com/lite-xl/lite-xl)) or
    [Textadapt](https://orbitalquark.github.io/textadept/)
    ([Github](https://github.com/orbitalquark/textadept)):
    Cross-platform lightweight GUI text editor extensible using Lua
  - [Vim](https://www.vim.org/) or
    [Neovim](https://neovim.io/):
    TUI text editor based on [vi](https://en.wikipedia.org/wiki/Vi);
    Vim can be found pre-installed on many systems; Neovim is a
    refactor of Vim; other more lightweight clones of vi exist, like
    [OpenVI](https://github.com/johnsonjh/OpenVi),
    [neatvi](https://github.com/aligrudi/neatvi),
    [nextvi](https://github.com/kyx0r/nextvi), or
    [nvi2](https://github.com/lichray/nvi2), (see
    [here](https://mattwidmann.net/notes/the-nvi-text-editor/),
    [here](https://mattwidmann.net/notes/configuring-the-defaults-of-nvi/)
    and [here](https://mattwidmann.net/notes/modern-nvi-mappings/) for
    more info; easily compilable only on BSDs and macOS), but these
    lack a good number of features added in vim and Neovim
  - [Vile](https://invisible-island.net/vile/):
    Vi Like Emacs (vile) combines vi modal editing with multiple
    buffer and multiple window features of Emacs; bindings are a mix
    of the two (for example, `i` enters insert mode from command mode
    while `C-x o` switches focused window)
  - [Vis](https://github.com/martanne/vis):
    TUI text editor combining vi modal editing with
    [Sam](https://en.wikipedia.org/wiki/Sam_(text_editor))'s
    structural regular expressions and command language
  - [Visual Studio Code](https://code.visualstudio.com/) or
    [VSCodium](https://vscodium.com/):
    GUI IDE; VSCodium is a build of VSCode that is free of tracking and
    Microsoft branding, but note that some Microsoft plugins like pylance
    (Python LSP server) does not support it
- Text tools (general)
  - [Aspell](http://aspell.net/) or
    [Nuspell](https://nuspell.github.io/):
    Spell checker
  - [bat](https://github.com/sharkdp/bat):
    Command-line `cat` clone with syntax highlighting and Git integration
  - [cspell](https://github.com/streetsidesoftware/cspell):
    Code-aware spell checker
  - [daff](https://paulfitz.github.io/daff/):
    Like `diff` but for tables
  - [delta](https://github.com/dandavison/delta):
    `diff` alternative
  - [enchant](https://github.com/AbiWord/enchant):
    Wrapper for abstracting different spell checking libraries into a
    single interface, with support for personal word lists (one word per
    line) at paths `$ENCHANT_CONFIG_DIR/<lang>.dic` (for example,
    `$HOME/.config/enchant/en_US.dic` for the US English personal word
    list on Unix or Linux systems)
  - [FIGlet](http://www.figlet.org/)
    ([Github](https://github.com/cmatsuoka/figlet)):
    Convert text to large ASCII word art, additional fonts are
    available [here](https://github.com/cmatsuoka/figlet-fonts)
  - [hexyl](https://github.com/sharkdp/hexyl):
    Command-line hex viewer
  - [LanguageTool](https://languagetool.org/):
    Style and grammar checker; standalone Java version downloadable
    [here](https://languagetool.org/download/), snapshots available
    [here](https://internal1.languagetool.org/snapshots/) (the link
    [link](https://languagetool.org/download/LanguageTool-stable.zip)
    has the latest version), and is
    [augmentable](https://dev.languagetool.org/finding-errors-using-n-gram-data.html)
    with [n-gram](https://languagetool.org/download/ngram-data/) data
  - [FileMerge](https://developer.apple.com/xcode/features/) or
    [Meld](https://meldmerge.org/) or
    [kdiff3](https://apps.kde.org/kdiff3/) or
    [tkdiff](https://sourceforge.net/projects/tkdiff/) or
    [xxdiff](https://github.com/blais/xxdiff):
    GUI `diff` alternative; Meld supports Windows and Linux and has a
    [macOS port](https://github.com/yousseb/meld), kdiff3 is
    Linux-only, xxdiff is lightweight but does not support Unicode,
    FileMerge comes with the macOS XCode IDE and can be called from
    the command-line using `opendiff`, tkdiff is a somewhat easier to
    set up and lightweight option on macOS (see _Mac Notes_ >
    _Graphical diff and merge tool_); these also diff directories
  - [par](http://www.nicemice.net/par/):
    Paragraph reformatter, like a smarter version of `fmt` from GNU
    [coreutils](https://www.gnu.org/software/coreutils/)
  - [sttr](https://github.com/abhimanyu003/sttr):
    Command-line tool for string operations
  - [uni](https://github.com/arp242/uni) or
    [chars](https://github.com/antifuchs/chars):
    Command-line tool for querying Unicode characters
- Text tools (structured)
  - [fq](https://github.com/wader/fq):
    Like `jq` but for binary formats
  - [ghostwriter](https://github.com/wereturtle/ghostwriter):
    Graphical Markdown editor; Windows and Linux (also on Flathub)
  - [gron](https://github.com/tomnomnom/gron):
    Flattens JSON into discrete assignments that work better
    with `grep` and `sed`
  - [htmlq](https://github.com/mgdm/htmlq) or
    [cascadia](https://github.com/suntong/cascadia):
    Like grep for HTML but using CSS selectors
  - [jq](https://stedolan.github.io/jq/):
    Command-line JSON processor
  - [jless](https://jless.io/):
    Ncurses command-line JSON viewer
  - [jo](https://github.com/jpmens/jo):
    Command-line utility for creating JSON objects
  - [Miller](https://github.com/johnkerl/miller) or
    [qsv](https://github.com/jqnatividad/qsv)
    (fork of [xsv](https://github.com/BurntSushi/xsv)) or
    [tsv-utils](https://github.com/eBay/tsv-utils):
    Command-line tool for working with CSV files;
    tsv-utils also supports TSV files, and Miller also supports TSV
    and tabular JSON files; Miller is more flexible, qsv is easier
    to use, tsv-utils is supposedly faster for large datasets.
  - [QXmlEdit](http://qxmledit.org/):
    XML editor
  - [Tad](https://www.tadviewer.com/)
    ([Github](https://github.com/antonycourtney/tad)):
    GUI tabular data viewer for CSV, Parquet, SQLite, and DuckDB files
  - [Visidata](https://www.visidata.org/) or
    [CSView](https://github.com/wfxr/csview):
    TUI tabular data viewer; Visidata is more of a multitool, in that
    it allows for editing and supports any source loadable via Pandas
    using the `-f` option
  - [yq](https://github.com/mikefarah/yq):
    Command-line YAML processor
- User experience and interface (graphical)
  - [LinearMouse](https://github.com/linearmouse/linearmouse) or
    [Mos](https://github.com/Caldis/Mos) or
    [Scroll Reverser](https://github.com/pilotmoon/Scroll-Reverser):
    Mouse enhancements like reverse scrolling, linear scrolling,
    acceleration, etc, for external mice on macOS
  - [Maccy](https://maccy.app/)
    ([Github](https://github.com/p0deje/Maccy)):
    Clipboard manager; macOS
  - [Rectangle](https://github.com/rxhanson/Rectangle):
    Move and resize windows using keyboard shortcuts and snap areas
    in macOS; as an alternative, three of the more useful keyboard
    shortcuts in Rectangle can be replicated using _Preferences_ >
    _Keyboard_ > _Keyboard Shortcuts_ > _App Shortcuts_ and clicking
    on `+`, keeping `All Applications`, inserting as the Menu Title
    action `Move Window to Left Side of Screen` and setting the
    shortcut key as desired (`Ctrl-Option-Left` is recommended), and
    repeating for other actions `Move Window to Right Side of Screen`
    (recommend `Ctrl-Option-Right` here), as well as `Zoom` (recommend
    using `Ctrl-Option-Return` here), **or** hover over the green `+`
    button on the top-left corner of the window and press `Option`
    which allows mouse selection of the actions which does not require
    keyboard shortcut configuration
  - [shottr](https://shottr.cc/):
    Screenshot app; macOS
  - [skhd](https://github.com/koekeishiya/skhd):
    Hotkey daemon; macOS
  - [stats](https://github.com/exelban/stats):
    Menu bar system monitor; macOS
  - [Textinator](https://github.com/RhetTbull/textinator):
    Detect text in screenshots and copy it to the clipboard; macOS
  - [ueli](https://ueli.app/):
    Launcher like [Alfred](https://www.alfredapp.com/) but open-source
    and available for Windows in addition to macOS
  - [Unshaky](https://github.com/aahung/Unshaky):
    Double keypress workaround for butterfly keyboards; macOS
- User experience and interface (text)
  - [direnv](https://github.com/direnv/direnv) or
    [shadowenv](https://github.com/Shopify/shadowenv):
    Load and unload env vars based on location; for `direnv`, config
    files go into `$XDG_CONFIG_HOME/direnv/` and allowed directory
    environment files are recorded in `$XDG_DATA_HOME/allow/`
  - [hollywood](https://github.com/dustinkirkland/hollywood):
    Hollywood technobabble in a Byobu session
  - [parallel](https://www.gnu.org/software/parallel/):
    Command-line tool for executing jobs in parallel
  - [pv](http://www.ivarch.com/programs/pv.shtml):
    Like `cat` but prints progress to stderr
  - [rlwrap](https://github.com/hanslub42/rlwrap):
    `readline` wrapper to enable completion and history for any
    command-line tool taking keyboard input
  - [screen](https://www.gnu.org/software/screen/) or
    [tmux](https://github.com/tmux/tmux) or
    [Zellij](https://zellij.dev/):
    Terminal multiplexer, useful for managing and persisting remote
    sessions over Mosh or SSH; generally, tmux is recommended over
    screen unless there is a need to access a serial console (using
    `screen /dev/somedevice` where `somedevice` is the device point
    surfaced after connecting the serial port; for more info, see
    [link](https://old.reddit.com/r/archlinux/comments/d41c1w/screen_vs_tmux/f0dj9rn/)
  - [ttyplot](https://github.com/tenox7/ttyplot):
    Real-time plotting tool in the terminal using stdin as data input
- Virtualization
  - [UTM](https://github.com/utmapp/UTM):
    iOS and macOS tool for managing [QEMU](https://www.qemu.org/)
    virtual machines
  - [virt-manager](https://virt-manager.org/):
    Linux desktop tool for managing QEMU/KVM virtual machines
- VPN
  - [innernet](https://github.com/tonarino/innernet) or
    [nebula](https://github.com/slackhq/nebula):
    Overlay networking tool for creating a private network, like
    [Tailscale](https://tailscale.com/)
  - [OpenVPN](https://openvpn.net/vpn-client/)
    or [TunnelBlick](https://tunnelblick.net/):
    OpenVPN client, note that Linux clients be should installed via
    the system package manager (see
    [here](https://community.openvpn.net/openvpn/wiki/OpenVPN3Linux));
    TunnelBlick is macOS-only and built on the older but still
    maintained OpenVPN 2 libraries
  - [WireGuard](https://www.wireguard.com/):
    Simple and high-performance VPN
- Web browsing
  - [Amfora](https://github.com/makeworld-the-better-one/amfora) or
    [Bombadillo](https://bombadillo.colorfield.space/)
    ([tildegit](https://tildegit.org/sloum/bombadillo)) or
    [gcat](https://github.com/aaronjanse/gcat):
    Gemini TUI clients, available on Debian or compile using
    Go via `go install github.com/makeworld-the-better-one/amfora` or
    `go install tildegit.org/sloum/bombadillo@latest`; Bombadillo
    is also a Gopher client; use gcat in Acme or dumb terminals
  - [Ka-Block!](http://kablock.com/)
    Safari extension for blocking ads and trackers; iOS and macOS
  - [Lagrange](https://gmi.skyjake.fi/lagrange/)
    ([Github](https://github.com/skyjake/lagrange)) or
    [Kristall](https://kristall.random-projects.net/)
    ([Github](https://github.com/MasterQ32/kristall)):
    Gemini GUI client; Kristall also supports Gopher
  - [Monolith](https://github.com/Y2Z/monolith.git):
    Save complete webpages to a single HTML file with embedded CSS,
    images and Javascript
- Web development
  - [dog](https://github.com/ogham/dog):
    Alternative to `dig`
  - [Hurl](https://hurl.dev/):
    Command-line tool for running HTTP requests defined in a text file
  - [frp](https://github.com/fatedier/frp):
    Reverse proxy, like [ngrok](https://ngrok.com/)
  - [httpie](https://httpie.io/) or
    [xh](https://github.com/ducaale/xh):
    Command-line API client; much lighter weight than
    [Postman](https://www.postman.com/)
  - [Mockoon](https://github.com/mockoon/mockoon):
    HTTP mock server
  - [mitmproxy](https://mitmproxy.org/):
    Interactive HTTPS proxy
  - [Prism](https://github.com/stoplightio/prism):
    HTTP mock server with behavior that can be specified from OpenAPI
    v2 (Swagger), OpenAPI v3 or Postman Collection files
- Word processing
  - [Manuskript](https://www.theologeek.ch/manuskript/):
    Word processor for writers like Scrivener
  - [TwineJS](https://github.com/klembot/twinejs):
    Tool for authoring interactive, non-linear stories
- Other
  - [age](https://github.com/FiloSottile/age) or
    [eureka](https://github.com/mimoo/eureka):
    Simple command-line encryption/decryption tool; eureka is more
    limited (no key files; generates a token to be used by recipient);
    or just use GnuPG, either without public key cryptography by
    running `gpg -o FILE.gpg -c FILE` to encrypt a file with a
    symmetric cipher using a passphrase where the encrypted file can
    later be decrypted with `gpg -d FILE.gpg` and the same passphrase,
    or with public key cryptography by encrypting a file using a
    recipient's public key and `gpg -r EMAIL -o FILE.gpg -e FILE`
    where the encrypted file can later be decrypted with the
    recipient's private key and `gpg -d <file>.gpg`; usually better
    to use something like Cryptomator if not encrypting to share
  - [Al Dente](https://github.com/davidwernhart/AlDente):
    macOS tool to limit battery charging (i.e., the claim is keeping
    charge percentage at or below 80% can help prolong battery life),
    however this may not be necessary if "Optimized Battery Charging"
    is sufficient for usage needs
  - [Anki](https://apps.ankiweb.net/):
    Flashcards software
  - [ClamAV](https://www.clamav.net/):
    Open-source antivirus engine, with frontends available like
    [ClamTk](https://gitlab.com/dave_m/clamtk)
  - [Diagon](https://github.com/ArthurSonzogni/Diagon):
    Command-line tool for transforming Markdown-style expressions into
    ASCII art, [webapp](https://github.com/ArthurSonzogni/Diagon) and
    [snap](https://snapcraft.io/diagon) available
  - [diffoscope](https://diffoscope.org/)
    ([Salsa](https://salsa.debian.org/reproducible-builds/diffoscope)):
    Like `diff` but supports archives and directories in addition to
    files, and can output to formats like HTML, JSON, Markdown and
    reStructuredText in addition to text; as a pure Python package, it
    is installable via `pip` from [PyPI](https://pypi.org/)
  - [FontForge](https://fontforge.org/)
    ([Github](https://github.com/fontforge/fontforge)):
    Open-source cross-platform scriptable font editor
  - [grex](https://github.com/pemistahl/grex):
    Generate regex from test cases
  - [Hammerspoon](https://www.hammerspoon.org/):
    Use [Lua](https://www.lua.org/) for macOS scripts that can call
    system APIs, for example middle-click-move mouse to scroll
    ([link](https://superuser.com/questions/303424/can-i-enable-scrolling-with-middle-button-drag-in-os-x))
  - [ivy](https://github.com/robpike/ivy):
    APL-like calculator ([docs](https://pkg.go.dev/robpike.io/ivy)),
    install with `go install robpike.io/ivy@latest` (requires Go)
  - [hyperfine](https://github.com/sharkdp/hyperfine):
    Benchmark shell commands
  - [Joplin](https://joplinapp.org/):
    Note-taking application with sync support; cross-platform
  - [Mathics](https://mathics.org/)
    ([Github](https://github.com/Mathics3)):
    Open-source alternative to
    [Mathematica](https://www.wolfram.com/mathematica/)
  - [OmegaT](https://omegat.org/):
    Translation memory tool
  - [pass](https://www.passwordstore.org/):
    Command-line password manager; if using MacPorts on macOS but
    using XCode command-line tools Git, install dependencies `tree`,
    `util-linux` and `qrencode` with MacPorts, and install `pass` from
    source specifying the `PREFIX` environment variable as appropriate
    (e.g., `PREFIX=$HOME/.local make install`), then modifying the
    installed `lib/password-store/platform.sh` file (e.g.,
    `$HOME/.local/lib/password-store/platform.sh`) so that the line
    `GETOPT=...` points directly to the MacPorts-installed `getopt`
    (e.g., `GETOPT="/Users/$(whoami)/macports/bin/getopt"`); for more,
    see [link](https://gist.github.com/abtrout/d64fb11ad6f9f49fa325)
  - [pastel](https://github.com/sharkdp/pastel) or
    [rgb-tui](https://github.com/ArthurSonzogni/rgb-tui):
    Terminal color picker
  - [Platypus](https://github.com/sveinbjornt/Platypus):
    macOS tool for wrapping command-line programs into a
    macOS application bundle
  - [sc-im](https://github.com/andmarti1424/sc-im)
    Terminal spreadsheet program
  - [Stellarium](https://stellarium.org/)
    ([Github](https://github.com/Stellarium/stellarium)):
    Open-source desktop planetarium
  - [Sweet Home 3D](https://www.sweethome3d.com/):
    Open-source interior design application, also available as a
    [webapp](https://www.sweethome3d.com/SweetHome3DOnlineManager.jsp)
  - [Taskwarrior](https://taskwarrior.org/) and
    [Timewarrior](https://timewarrior.net/):
    Todo list management (Taskwarrior) and time-tracking (Timewarrior)
  - [entr](https://github.com/eradman/entr) or
    [fswatch](https://github.com/emcrisostomo/fswatch) or
    [watchdog](https://github.com/gorakhargosh/watchdog) or
    [watchexec](https://github.com/watchexec/watchexec) or
    [watchfiles](https://github.com/samuelcolvin/watchfiles) or
    [Watch](https://pkg.go.dev/9fans.net/go/acme/Watch) or
    [Watchman](https://github.com/facebook/watchman):
    Run command when files change; entr, watchexec and watchfiles are
    somewhat easier to use; fswatch is more a file watcher but can be
    piped to xargs to run commands specific to the changed file(s);
    Watch is Acme editor-specific; and Watchman has a client-server
    architecture and is designed as more of a per-user system service;
    watchdog is mainly a library but has an optional tool watchmedo
    (install `watchdog[watchmedo]` via pip) similar to watchexec
  - [Zotero](https://www.zotero.org/):
    Reference management software to collect, organize, cite and
    share research material

## Commercial software

- Diagramming
  - [iThoughts](https://www.toketaware.com/):
    Mind-mapping tool
  - [Monodraw](https://monodraw.helftone.com/):
    ASCII art editor; macOS
- File management
  - [DevonThink Pro](https://www.devontechnologies.com/apps/devonthink):
    Document management and search solution; macOS
  - [Mountain Duck](https://mountainduck.io/):
    Like `rclone mount` but with a nicer user interface and its vaults
    are interoperable with Cryptomator vaults
- Integrated development environments
  - [DataGrip](https://www.jetbrains.com/datagrip/):
    Database admin-oriented IDE
  - [DataSpell](https://www.jetbrains.com/dataspell/):
    Data science-oriented IDE
  - [PyCharm](https://www.jetbrains.com/pycharm/):
    Python programming-oriented IDE; has open-source community edition
  - [RStudio](https://rstudio.com/):
    IDE for R; has open-source edition
- Project (codebase) tools
  - [Sublime Merge](https://www.sublimemerge.com/):
    GUI Git client for macOS
- Remote login and desktop
  - [Jump Desktop](https://jumpdesktop.com/):
    Remote desktop software, connect to host systems that have
    [Jump Desktop Connect](https://jumpdesktop.com/connect/)
    installed; macOS and Windows
  - [Royal TSX](https://royalapps.com/ts/):
    Cross-platform RDP and VNC client; has free version
- Text tools (general)
  - [Beyond Compare](https://www.scootersoftware.com/):
    Compare files and folders
  - [Kaleidoscope](https://kaleidoscope.app/):
    Diff text and images
- Word processing
  - [Scrivener](https://www.literatureandlatte.com/scrivener/overview):
    Word processor for authoring books and screenplays

## Directory structure

An opinionated directory structure setup:

- Locally installed software (usually C or C++ source compiled and
  installed using `make` or `cmake`) use the `$HOME/.local` prefix.
- Projects go into `src` and have their directory path provide any
  relevant information about the project, e.g., this repository's
  local path would be `$HOME/src/github.com/matheuristic/dotfiles`
  while a non-version-controlled project could be at the
  `$HOME/src/someprojectname` directory.
- Vendored or third-party packages typically go into `vendor`, e.g.,
  [qsv](https://github.com/jqnatividad/qsv) downloads can be put into
  `$HOME/vendor/github.com/jqnatividad/qsv` and its binaries
  for versions `X.Y.Z` can go into the
  `$HOME/vendor/github.com/jqnatividad/qsv/X.Y.Z` directory.
- Otherwise, use the directory structure appropriate for the operating
  system or installed software configuration.

## Standard directories for application configuration, data, etc

The different OSes have guidelines on where applications should store
app-related files (cache, configuration, data, logs, etc) on the
system. These are either official (Apple for macOS), or defined by a
working group (like XDG for Linux and Unix).

For Linux, many software applications store their configuration
and data according to the XDG base directory specification
([link](https://specifications.freedesktop.org/basedir-spec/latest/)).
This specification allows the user to define storage locations using
environment variables. The following are some environment variables,
what they are for, and their defaults when unspecified:

- `$XDG_CACHE_HOME`: cache, defaults to `~/.cache`
- `$XDG_CONFIG_HOME`: config, defaults to `~/.config`
- `$XDG_CONFIG_DIRS`: fallback config, defaults to `/etc/xdg`
- `$XDG_DATA_HOME`: data, defaults to `~/.local/share`
- `$XDG_DATA_DIRS`: fallback data, defaults to
  `/usr/local/share/:/usr/share/`
- `$XDG_RUNTIME_DIR`: runtime file, defaults to No default, some apps
  default to `run/user/UID`
- `$XDG_STATE_HOME`: state files, logs, history, defaults to
  `~/.local/state`, this is a more recent addition (see
  [link](https://www.reddit.com/r/linux/comments/ny34vs/new_xdg_state_home_in_xdg_base_directory_spec/))

On macOS, Apple provides app config and data storage guidelines
([link](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html#//apple_ref/doc/uid/TP40010672-CH2-SW1)), outlining directories where
each app may create its own directory for storing its config and
data files.

- `~/Library/Application Support`: App data files except user docs
- `~/Library/Caches`: App cache
- `~/Library/Frameworks`: Cross-app frameworks
- `~/Library/Preferences`: App-specific preference files that are
  **created by macOS APIs**; note that preferences not created through
  macOS APIs should go in `~/Library/Application Support` instead

Not all apps follow XDG or Apple's guidelines on the two systems,
often for convenience or legacy reasons. For example, Vim by default
uses `~/.vimrc` for the user config file on both Linux and macOS.

## Conda

Other package managers can exist alongside the operating system's
(APT for Debian, the App Store for macOS, and so on). Conda is one of
these, and can be set up without the need to be a superuser.

### Anaconda, Miniconda, Miniforge, Mambaforge

[Conda](https://conda.io/) is a package and environment manager useful
for developing and deploying applications leverage a repository of
pre-built packages.

[Anaconda](https://anaconda.org/) is a full-fat distribution with
many default packages installed, and is created and maintained by
[Anaconda, Inc](https://www.anaconda.com/).

[Miniconda](https://docs.conda.io/en/latest/miniconda.html) is a
minimal version of Anaconda that only contains the basic set of
package and environment management commands.

[Miniforge](https://github.com/conda-forge/miniforge) is a variant
of Miniconda that uses the [conda-forge](https://conda-forge.org/)
channel by default.

[Mambaforge](https://github.com/conda-forge/miniforge#mambaforge)
is a Miniforge variant that has installed by default
[Mamba](https://github.com/mamba-org/mamba) which is a fast
re-implementation of the conda package manager.

It is usually best to install Miniconda, Miniforge or Mambaforge
because of the smaller disk footprint since they don't come
pre-installed with a wide array of packages.

It is recommended that any code development, and even tools where
multiple versions need to exist on the system, be installed into
specific environments (whether using Conda or some other tool).

### Workflow examples

An example workflow is shown here, with some useful commands. See the
[documentation](https://docs.conda.io/en/latest/) for more details on
how to use the package manager.

Using `conda` here but `mamba` supports the same parameters.

```sh
# Create a new environment using a specific channel-only
conda create -n envname -c bioforge --override-channels
# Active the environment
conda activate envname
# Add another channel (repository) for the environment
conda config --env --add channels anaconda
# Install a specific version of a package
conda install python=3.8
# Install a package from a specific repository
conda install -c bioforge pandas
# Update all packages
conda update --all
# Export environment with prefix path and build versions to YAML
conda env export > envname.yml
# Same as above without prefix paths and build versions
conda env export --no-builds | grep -v -e '^prefix' > envname-basic.yml
# Same as above but only showing explicitly requested packages
conda env export --from-history | grep -v -e '^prefix' > envname-manual.yml
# Deactivate environment
conda deactivate
# Recreate an environment from some environment file
conda env create -n newenvname -f envname.yml
```

### Python pip interaction with Conda environments

Python's [pip](https://pip.pypa.io/en/stable/) package installer will
correctly install packages from [PyPI](https://pypi.org/) into the
active Conda environment. Packages installed using a `pip` binary
from the environment (check using `which pip`, should be installed
automatically if `python` is installed in an environment as in the
workflow example above) should also be tracked automatically by Conda.

Similarly, installing Python packages in editable
mode (same as setuptool's development mode, see
[link](https://setuptools.readthedocs.io/en/latest/setuptools.html#dev
elopment-mode)), i.e.,

```sh
pip install -e .
```

is run from the package repository directory with `setup.py`, should
also links into the active environment's Python `site-packages` dir.

## Linux notes

Some notes also apply to BSD systems.

### APT usage tips

Usage tips for [APT](https://en.wikipedia.org/wiki/APT_(software))
which is the default package manager for
[Debian](https://www.debian.org/) and [Ubuntu](https://ubuntu.com/).

To figure out why a package `some-package-name` is installed, run:

```sh
apt-cache rdepends \
    --no-{suggests,conflicts,breaks,replaces,enhances} \
    --installed --recurse \
    some-package-name
```

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
- **Opening URLS in the host Chrome browser**: To open a given URL in
  the host Chrome browser, use `garcon-url-handler`, e.g.
  `garcon-url-handler https://www.google.com -new-window` opens
  the Google homepage in a new browser window (for more details, see
  [link](https://support.google.com/chromebook/thread/102840796?hl=en&msgid=102977439))
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

### Conda

Mambaforge. Assumes `$HOME/.local/bin` is in `$PATH` and Bash is the
user shell (and therefore also that `$HOME/.bashrc` exists).

```sh
# Install mambaforge for faster operations
chmod +x Mambaforge-Linux-x86_64.sh
./Mambaforge-Linux-x86_64.sh                      # interactive install
eval "$(~/mambaforge/bin/conda shell.bash hook)"  # modify path as needed
conda init
conda config --set auto_activate_base false
mamba init
# Create new environments as needed for each project, e.g.
# $ mamba create -n some-project python=3.9
```

Some tools (like `bat`, `htop`, etc) have newer versions in conda-forge
than what is available in the system package manager. If desired,
install these in a development environment and symlink them to a
directory in `$PATH` (below assumes `$HOME/.local/bin` is in `$PATH`).

```sh
# Create a named environment rather than polluting base
mamba create -n tools
mamba activate tools
mamba install bat black htop mdformat pandoc ripgrep
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/tools/bin/bat
ln -s $HOME/mambaforge/envs/tools/bin/black
ln -s $HOME/mambaforge/envs/tools/bin/htop
ln -s $HOME/mambaforge/envs/tools/bin/mdformat
ln -s $HOME/mambaforge/envs/tools/bin/pandoc
ln -s $HOME/mambaforge/envs/tools/bin/rg
mamba deactivate
```

## Mac notes

### Configuring macOS settings

Some recommended customizations:

- Go to **Finder** > **Preferences** > **Advanced** >
  Set **When performing a search:** to **Search the Current Folder**
  (sets the default search location in Finder to the current folder)
- Go to **Finder** > **Preferences** > **Advanced** >
  Enable **Show all filename extensions**
  (make file listings in Finder be more consistent in what is shown)
- Go to **Finder** > **View** > **Customize toolbar** >
  Drag the **Path** icon to the toolbar
  (ease navigation to parent or ancestor folder)
- Go to **Finder** > Do _Command-Shift-period_ in a Finder window
  (toggles showing hidden files, persists across sessions)
- Go to **System settings** > **General** > **About** >
  Modify **Name** to the desired one for the machine

These and more configuration options can also be set using the
command-line, see [these](https://gist.github.com/uson1x/2275613)
[links](https://gist.github.com/iamdanre/551a6a68ce0fc3c9cea0ad53e32d5690).

### Alternative web browsers

Besides built-in Safari, other browsers may be useful or preferred:

- [Google Chrome](https://www.google.com/chrome/)
  (some websites are only fully compatible with this)
- [Firefox](https://www.mozilla.org/en-US/firefox/new/)
  (for compatibility with more powerful extensions, see
  [link](https://blog.mozilla.org/addons/2022/05/18/manifest-v3-in-firefox-recap-next-steps/))

### Installing XCode command-line tools

```sh
sudo rm -rf /Library/Developer/CommandLineTools
sudo xcode-select --install
```

Attempting to run commands like `/usr/bin/clang` or `/usr/bin/clang`
will also prompt the user about whether the system should download and
install the command-line developer tools.

### Installing MacPorts without root privileges

Make sure the XCode command-line developer tools are installed.

In a nutshell, download the latest MacPorts source release from
[Github](https://github.com/macports/macports-base/releases), verify
its checksum against the `MacPorts-<VERSION>.chk.txt` file, extract
it, configure the installation directory and indicate root privileges
should not be used, compile it, install it, configure the user shell
`$PATH` and `$MANPATH`, and sync the ports tree.

```sh
mkdir -p ${HOME}/vendor/github.com/macports/macports-base # change dir as desired
cd ${HOME}/vendor/github.com/macports/macports-base
export MACPORTSVERSION=2.8.0   # change as needed
export PREFIX=${HOME}/macports # change as desired
curl -LO https://github.com/macports/macports-base/releases/download/v${MACPORTSVERSION}/MacPorts-${MACPORTSVERSION}.tar.bz2
curl -LO https://github.com/macports/macports-base/releases/download/v${MACPORTSVERSION}/MacPorts-${MACPORTSVERSION}.chk.txt
shasum -a256 MacPorts-${MACPORTSVERSION}.tar.bz2  # ... and verify against the entry in MacPorts-${MACPORTSVERSION}.chk.txt
tar xjf MacPorts-${MACPORTSVERSION}.tar.bz2
cd MacPorts-${MACPORTSVERSION}
./configure --with-no-root-privileges --prefix=${PREFIX}
make -s -j `sysctl -n machdep.cpu.core_count`
make -s -j `sysctl -n machdep.cpu.core_count` install
cat >> ${HOME}/.zshrc <<EOF
export PATH=${PREFIX}/bin:${PREFIX}/sbin:\$PATH
export MANPATH=${PREFIX}/share/man:\$MANPATH
EOF
source ${HOME}/.zshrc
port selfupdate
```

Recommended tools to install (not listed are those that need to be
compiled using the Go, Haskell or Rust toolchain like `bat` or
`pandoc` or `ripgrep`; they can be compiled using MacPorts, using a
separate environment, or their binaries installed directly using
conda as detailed in the next section):

- `gawk` (GNU
  [AWK](https://www.gnu.org/software/gawk/manual/gawk.html))
- `gnupg2`
- `gsed` (GNU [sed](https://www.gnu.org/software/sed/))
- `htop`
- `mosh`
- `stow`
- `tree`

```sh
port -N install gawk gnupg2 gsed mosh stow tree
```

If installing `gnupg2` above, it is recommended to change the
`pinentry` symlink to `pinentry-tty` instead of the default
`pinentry-ncurses`. For more info, see the _Using TTY pinentry_
subsection of the _GnuPG_ section.

Useful `port` commands ("ports" are the tool's name for packages):

```sh
port selfupdate                 # update the ports tree
port -y install somepkgname     # dry-run install of somepkgname
port installed inactive         # list inactive ports
port uninstall inactive         # uninstall inactive ports
port echo leaves                # list leaves (auto-installed ports that are no longer dependencies)
port uninstall leaves           # uninstall leaves
port installed requested        # list manually installed port
port unsetrequested somepkgname # mark manually installed port as auto-installed
```

To update the MacPorts version, just recompile and reinstall the new
version.

Note that on a major macOS version upgrade, it is probably best to
wipe the whole MacPorts install, recompile and reinstall the
previously installed requested ports. For more info, see this
[link](https://trac.macports.org/wiki/Migration).

### Graphical diff and merge tool

There are a number of commercial graphical diff and merge tools
easily installable in macOS, but open-source options all require
a bit of work:

- `opendiff`/`FileMerge`: requires XCode IDE (not command-line tools)
  which requires a lot of disk space be installed.
- `tkdiff`: typically installed via Homebrew (requires root privileges
  to set up) or manually installed (MacPorts version is pretty old),
  requires [Tk](https://www.tcl.tk/).
- `xxdiff`: typically installed via Homebrew (requires root privileges
  to set up) or MacPorts, and requires Qt which has many build
  dependencies because of if not installing pre-built binaries.

In terms of the recommended setup, if the XCode IDE is installed just
use `opendiff`. Otherwise, if using the MacPorts package manager,
install Quartz Tk and download the latest tkdiff source which is an
executable Tcl script. The following shell commands installs Quartz Tk
using MacPorts (the macOS system version is rather old), downloads the
tkdiff v5.6 into `$HOME/vendor/sourceforge.net/tkdiff` and symlinks
the `tkdiff` script into `$HOME/.local/bin` (assumed to be `$PATH`).

  ```sh
  port -N install tk +quartz # via MacPorts, skip if already installed
  export TKDIFFVERSION=5.6   # change as appropriate
  mkdir -p $HOME/vendor/sourceforge.net/tkdiff
  cd $HOME/vendor/sourceforge.net/tkdiff
  curl -L https://sourceforge.net/projects/tkdiff/files/tkdiff/${TKDIFFVERSION}/tkdiff-${TKDIFFVERSION}.zip/download > tkdiff-${TKDIFFVERSION}.zip
  unzip tkdiff-${TKDIFFVERSION}.zip
  rm -f latest && ln -s tkdiff-${TKDIFFVERSION} latest
  cd ~/.local/bin
  ln -s $HOME/vendor/sourceforge.net/tkdiff/latest/tkdiff
  ```

Graphical diff and merge tools can also be used with diffing and
merging in Git. This can be configured as follows (the example here
uses `tkdiff` globally, modify as appropriate; the last config option
is so that `git difftool --dir-diff` works properly with `tkdiff`):

```sh
git config --global merge.tool tkdiff
git config --global diff.tool tkdiff
git config --global --add difftool.prompt false
git config --global difftool.tkdiff.cmd 'tkdiff -R "$LOCAL" "$REMOTE"'
```

Also, as an aside, Git automatically creates `*.orig` backup files
created while resolving merges, which can leave a number of these
files lying around after merges are completed. To configure Git so
that they are removed automatically as files are successfully merged,
do the following:

```sh
git config --global mergetool.keepBackup false
```

### Basic CLI development tools using conda

Some general tools (these ones have heavier build dependencies,
so just use the conda binaries instead of compiling with MacPorts):

- `bat`
- `pandoc`
  (as of 2022-11-15, only the `anaconda` channel has a working Apple
  silicon version so install from there, see _Pandoc setup_ below)
- `ripgrep`

Some data tools:

- `postgresql`
- `visidata`

And some programming languages:

- `go`
- `python` and many packages in [PyPI](https://pypi.org/)
- `r-base` and some [CRAN](https://cran.r-project.org/) packages
- `sbcl`

Notes:

- R packages begin with an `r-` prefix but only popular packages are
  available and these should not be mixed with the packages installed
  using R's `package.install` mechanism.
- Conda has an `emacs` package, but go with one of the standalone
  Emacs applications ([Emacs for Mac OSX](https://emacsformacosx.com/)
  [emacs-mac](https://github.com/railwaycat/homebrew-emacsmacport)
  port) as they are better integrated with the system.
- Conda can provide a basic CLI dev environment without XCode or its
  command-line tools installed. However, it is recommended to install at
  least the command-line tools.

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
mamba create -n tools
mamba activate tools
cd $HOME/.local/bin
for cmd in bat ripgrep; do
    mamba install -y "$cmd"
    ln -s "$HOME/mambaforge/envs/tools/bin/$cmd"
done
mamba deactivate
# Create new environments as needed for each project, e.g.
# > mamba create -n some-project python=3.9
```

#### LaTeX tools setup

Use Tectonic for LaTeX compilation.

```sh
mamba create -n latextools
mamba activate latextools
mamba install tectonic
mamba deactivate
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/latextools/bin/tectonic
```

To support compiling PDF files from Markdown files, setup
either Pandoc or the Kramdown-Asciidoc toolchain as described
in the next sections.

#### groff setup

GNU roff (groff) is a typesetting system useful as a lightweight
alternative to LaTeX.

There are no groff binaries packaged for macOS arm64 in the
conda-forge repository, but binaries are available from the Anaconda
repository.

```sh
mamba create -n groff
mamba activate groff
conda config --env --add channels anaconda
conda install groff
mamba deactivate
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/groff/bin/groff
```

#### Pandoc setup (recommended)

Pandoc is a very useful tool for converting between different document
formats (e.g., Markdown to HTML, or Markdown to LaTeX).

There are no Pandoc binaries packaged for macOS arm64 in the
conda-forge repository, but binaries are available from the Anaconda
conda repository.

```sh
mamba create -n pandoc
mamba activate pandoc
conda config --env --add channels anaconda
conda install pandoc
mamba deactivate
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/pandoc/bin/pandoc
```

#### Kramdown-Asciidoc toolchain setup (alternative to Pandoc)

Setup a useful Ruby toolchain for compiling Markdown files to LaTeX.

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
mamba create -n pytools python=3.9
mamba activate pytools
mamba install black
mamba install python-lsp-server
mamba deactivate
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/pytools/black
ln -s $HOME/mambaforge/envs/pytools/pylsp
```

#### Installing GPU ML libraries for darwin-aarch64 machines

**PyTorch**

PyTorch v1.12 and later natively supports GPU-acceleration for Apple
silicon GPUs, so just install as usual (via conda shown below, change
Python version as appropriate).

```sh
mamba create -n pytorchenv python=3.9
mamba activate pytorchenv
conda config --env --add channels pytorch
mamba install -c pytorch pytorch torchvision torchaudio
```

**Tensorflow**


Metal-enabled [Tensorflow](https://www.tensorflow.org/)
([link](https://developer.apple.com/metal/tensorflow-plugin/)),
currently supports Python `3.8`, `3.9` and `3.10`.

Note that `tensorflow-deps` versions follow base Tensorflow versions,
so if using Tensorflow `2.6.X`, install `tensorflow-deps==2.6.0`.

```sh
mamba create -n tfenv python=3.9
mamba activate tfenv
conda config --env --add channels apple
mamba install tensorflow
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

To install [tensorflow-text](https://github.com/tensorflow/text) with
Metal-support, see
[here](https://github.com/sun1638650145/Libraries-and-Extensions-for-TensorFlow-for-Apple-Silicon)
for compile instructions and pre-compiled wheels, and see
[here](https://github.com/tensorflow/text/pull/756) for more info.

### Installing and using Spack (alternative to MacPort)

Adapted from:
https://spack.readthedocs.io/en/latest/getting_started.html#installation

Spack is another package managers that does not require root
permissions by default. (Conda and MacPorts, detailed above, can also
set up without root privileges).

```sh
git clone -c feature.manyFiles=true https://github.com/spack/spack.git ~/spack
# Following is for Bash/ZSH, see install instructions for other shells
. ~/spack/share/spack/setup-env.sh
# if needed, use a Python install other than the system one, for example
#   export SPACK_PYTHON=$HOME/mambaforge/envs/pyenv/bin/python
# see https://github.com/spack/spack/pull/31792
# and https://github.com/spack/spack/issues/31735
```

Modify the shell config file to set up the environment setup when the
shell starts. For Bash (`$HOME/.bashrc`) or ZSH (`$HOME/.zshrc`):

```sh
if [ -d $HOME/spack ]; then
  . $HOME/spack/share/spack/setup-env.sh
  # Can use a Python install other than the system one, for example
  #   export SPACK_PYTHON=$HOME/mambaforge/envs/pyenv/bin/python
fi
```

It is best practice to install packages within an environment, e.g.:

```sh
spack env create default
spack env activate default
spack install libvterm libtool cmake
spack env deactivate
```

To use some environment by default (like the `default` environment
created above), load it in the shell config startup, e.g. the above
block in Bash and ZSH config example could be changed to:

```sh
if [ -d $HOME/spack ]; then
  . $HOME/spack/share/spack/setup-env.sh
  # Can use a Python install other than the system one, for example
  #   export SPACK_PYTHON=$HOME/mambaforge/envs/pyenv/bin/python
  spack env activate default
fi
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

- [zenith](https://github.com/bvaisvil/zenith)
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

### Using TTY pinentry

If using a console pinentry for entering the GnuPG password in the
terminal, it can be better to use TTY pinentry rather than ncurses
pinentry. Configure this as follows:

- The default `pinentry` is usually symlinked to `pinentry-ncurses`
  which has issues being run from other ncurses programs like Vim.
  Change the symlink `pinentry` so it points to `pinentry-tty`. For example,
  suppose if using `pinentry` from MacPorts installed to `$HOME/macports`:

  ```sh
  cd $HOME/macports/bin
  rm pinentry && ln -s pinentry-tty pinentry
  ```

- Set the `$GPG_TTY` environment variable in the shell configuration to the
  value of `tty`, else errors like `Inappropriate ioctl for device` might be
  thrown by GnuPG. For example, add the following line to `$HOME/.zshrc` (if
  Zsh is the user shell) or `$HOME/.bashrc` (if Bash is the user shell):

  ```sh
  export GPG_TTY=$(tty)
  ```

References:

- ["inappropriate ioctl for device"](https://github.com/keybase/keybase-issues/issues/2798)

### Migrating keys

A simple way to migrate GnuPG keys is to copy over the GnuPG data
directory (this preserves all keys and the the trust database) at
`C:/Documents and Settings/Username/Application Data/GnuPG` on Windows
systems (replace `Username` as appropriate) or at `$HOME/.gnupg` on
Linux/macOS systems.

References:

- [Moving/Copying your PGP Keys](https://www.phildev.net/pgp/gpg_moving_keys.html)

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

1. Get `git-credential-netrc` and save it to a directory in `$PATH`
   (below assumes `$HOME/.local/bin` is in `$PATH`).

   ```sh
   cd $HOME/.local/bin
   curl -L https://raw.githubusercontent.com/git/git/master/contrib/credential/netrc/git-credential-netrc.perl > git-credential-netrc
   chmod +x git-credential-netrc
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

1. **If using Git from the XCode command-line tools on macOS**, add
   bundled Perl modules to `$PERL5LIB` or `$PERLLIB`, i.e., add the
   following line to `$HOME/.bashrc` (if Bash is the user shell) or
   `$HOME/.zshrc` (if Zsh is the user shell):

   ```sh
   export PERL5LIB=/Library/Developer/CommandLineTools/usr/share/git-core/perl:$PERL5LIB
   ```

**NOTES**:

- Requires the Perl modules bundled with Git be in the Perl library
  path. On Linux, this is typically automatic when Git is installed
  using the system package manager. On macOS, follow steps above if
  using XCode command-line tools Git.
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

### Rebasing to squash commits

1. Rebase interactively from a base commit using Git's `rebase`
   subcommand (e.g., `git rebase -i HEAD~3` bases on 3 commits ago).

1. The command will spawn the default editor (`$EDITOR`) where commits
   since the specified base can be squashed or picked. Note that the
   earliest commit which is at the top has to be set to `pick`.

1. After the file is saved and closed, the local repo is rebased.

1. To push to the remote repository, it may be necessary to do
   `git push -u origin --force-with-lease` rather than the usual
   `git push -u origin` command.

### Pruning branches no longer on the remote repository

1. `git fetch --prune` to prune references no longer on remote.

1. `git branch -vv` to show local branches, where the pruned branches
   will have a `gone` tag.

1. Run `git branch -d <branch>` on each pruned branch to delete it
   (use `git branch -D` if branch isn't fully merged but unneeded).

For more info, see the following StackOverflow
[link](https://stackoverflow.com/questions/7726949/remove-tracking-branches-no-longer-on-remote).

### Reconciliation after accidentally using `mv` instead of `git mv`

Suppose a version controlled file `a.txt` in a Git repository is to be
moved to `b.txt`. Normally, that should be done using:

```sh
git mv a.txt b.txt
```

However, supposed that was done using `mv` instead. The following
steps show how to reconcile the Git repository with the new filenames
(basically `git rm` the old filename and `git add` the new filename):

```sh
mv a.txt b.txt
git rm a.txt
git add b.txt
```

## Using Notmuch, Lieer and aerc for GMail usage

[Notmuch](https://notmuchmail.org/) is a backend system for indexing,
tagging and searching emails.

GMail has an IMAP interface, but that can be clunky and it is really
geared toward OAuth2 clients. Notmuch can support GMail via OAuth2 by
using [lieer](https://github.com/gauteh/lieer) to push and pull email
and labels from GMail, storing them in a maildir and sychronizing
labels with a Notmuch database.

There are a number of front-ends that can be used with Notmuch.
[aerc](https://git.sr.ht/~rjarry/aerc) is a relatively simple but
flexible one to use.

### Installing Notmuch and Lieer

The following instructions show how to set up Notmuch, Lieer and
aerc for use with GMail, in a Conda environment to avoid packaging
conflicts with other software on the system.

Environment variables for the build process (assume the
`$HOME/packages` directory exists):

```sh
export NOTMUCHDIR=$HOME/packages/notmuch
export LIEERDIR=$HOME/packages/lieer
```

Create Conda environment for Notmuch Python bindings and Lieer (using
[mambaforge](https://github.com/conda-forge/miniforge#mambaforge)),
and activate it:

```sh
CONDAENVIRON=email
mamba create -n $CONDAENVIRON python=3.9
mamba activate $CONDAENVIRON
```

Compile Notmuch bindings:

```sh
sudo apt install libxapian-dev libgmime-3.0-dev libtalloc-dev zlib1g-dev python3-sphinx texinfo install-info
git clone git://git.notmuchmail.org/git/notmuch "$NOTMUCHDIR"
cd "$NOTMUCHDIR"
./configure --prefix=$HOME/.local
make
make install
```

Build Notmuch Python bindings and Lieer:

```sh
cd "$NOTMUCHDIR/bindings/python"
pip install .
git clone https://github.com/gauteh/lieer.git "$LIEERDIR"
cd "$LIEERDIR"
pip install .
```

Create wrapper scripts to call Lieer binaries `gmi` from outside the
Conda environment:

```sh
cat > $HOME/.local/bin/gmi <<EOF
#!/bin/bash

# Wrapper script for running gmi

source "$HOME/mambaforge/etc/profile.d/conda.sh"
conda activate $CONDAENVIRON

LD_LIBRARY_PATH=$NOTMUCHDIR/lib gmi "\$@"
EOF
chmod +x $HOME/.local/bin/gmi
```

Ok to deactivate the Conda environment now:

```sh
mamba deactivate
```

Create a basic Notmuch configuration file at `$HOME/.notmuch-config` using
a setup wizard:

```sh
notmuch setup
```

Edit the `$HOME/.notmuch-config` configuration to reflect the following
(no need to add any tags to unread emails since GMail already adds the
`unread` tag automatically):

```text
[new]
tags=
ignore=/.*[.](json|lock|bak)$/
```

Create a mail directory and initialize the Notmuch database:

```sh
mkdir -p $HOME/.mail
cd $HOME/.mail
notmuch new
```

### Set up an application project that can use the GMail API

Create a Google developer OAuth client ID and download its secrets
file. The exact flow keeps changing, but core steps as of writing are
as follows:

Go to the Google developer console
([link](https://console.developers.google.com/flows/enableapi?apiid=gmail))
to create a new GMail project and API credentials for it.

Note that the pages mentioned below are in the **APIs & Services**
section.

Enable the Gmail API (go to **Enabled APIs & Services** then click
**+ ENABLE APIS AND SERVICES** then choose Gmail API). For what data
will be processed, "User data" can be chosen.

Set up the consent screen (go to **OAuth consent screen**). For user
type, choose **Internal** if a GSuite user, else **External**. Give
the application a name.

Create the OAuth client ID (go to **Credentials**, then click **+
CREATE CREDENTIALS**, then **OAuth client ID**). Choose *Desktop app*
as the application type and name the application.

Complete the setup, then download the OAuth client ID secret in JSON
format.

The rest of the instructions assume the client secret file is
downloaded as the `$HOME/Downloads/client_secret_CLIENTID.json` file
(where `CLIENTID` should be the acutal client ID).

### Setting up the GMail account for Notmuch and Lieer

Suppose a GMail user with email address `username@emailserver.com` is
to be added (modify appropriately).

Create a subdirectory for the user in the mail directory.

```sh
cd $HOME/.mail
mkdir username@emailserver.com
```

Copy the client secret file to the local mailbox directory (i.e.,
`$HOME/.mail/username@emailserver.com/`) and use it to authenticate
(replace `CLIENTID` with the actual client ID):

```sh
cd $HOME/.mail/username@emailserver.com
cp -a $HOME/Downloads/client_secret_CLIENTID.json .
gmi init -c client_secret_CLIENTID.json username@emailserver.com
```

This pops up a web flow in the browser to authenticate. Now retrieve email:

```sh
gmi pull
```

**Note**: If there is a need to change client IDs or reset the client
secret, it is possible to reauthenticate using a new client secret
file:

```sh
gmi auth -f -c new_client_secret_CLIENTID.json
```

Back to setting up the GMail account in Notmuch.

Configure a hook that runs `gmi pull` when `notmuch new` is called:

```sh
mkdir -p $HOME/.mail/.notmuch/hooks
cat > $HOME/.mail/.notmuch/hooks/pre-new <<EOF
#/bin/sh
cd $HOME/.mail/username@emailserver.com
$HOME/.local/bin/gmi sync
EOF
chmod +x $HOME/.mail/.notmuch/hooks/pre-new
```

After this setup, `notmuch new` should do two-way sync of mail and
tags between the local database and GMail.

Set up a convenience wrapper `gmi-send` for `gmi send` to send mail
with this account:

```sh
cat > $HOME/.local/bin/gmi-send <<EOF
!/bin/bash

# Wrapper script for running "gmi send"
$HOME/.local/bin/gmi send --quiet -t -C $HOME/.mail/username@emailserver.com "\$@"
EOF
chmod +x $HOME/.local/bin/gmi-send
```

If there are multiple accounts, this can be repeated with
the change that lines of the following sort should be
added to the hook file `$HOME/.mail/.notmuch/hook/pre-new`

```sh
cd $HOME/.mail/anotherusername@anotheremailserver.com
$HOME/.local/bin/gmi sync
```

and there should be different `gmi-send` wrappers `gmi-send-NAME1`,
`gmi-send-NAME2`, etc (change `NAME1`, `NAME2`, etc appropriately),
for the different accounts instead of just one `gmi-send`.

### Installing and setting up aerc

Make sure the [Go](https://go.dev/) compiler is installed (download
from the Go website, or use a package manager like APT or MacPorts).

Compile aerc and install it to a separate directory (best to do so
because it needs to be run with specific library paths set):

```sh
export AERCSRC=$HOME/packages/aerc
export AERCTGT=$HOME/aerc
mkdir -p "$AERCTGT"
git clone https://git.sr.ht/~rjarry/aerc "$AERCSRC"
cd "$AERCSRC"
LIBRARY_PATH="$NOTMUCHDIR/lib" CPATH="$NOTMUCHDIR/lib" GOFLAGS=-tags=notmuch PREFIX=$AERCTGT make install
```

Set up configuration files for aerc.

Make the aerc config directory as needed, and copy the general
settings template to it.

```sh
mkdir -p $HOME/.config/aerc
cp -a $AERCTGT/usr/share/aerc/aerc.conf $HOME/.config/aerc/
```

Modify `$HOME/.config/aerc/aerc.conf` to reflect the following,
which uses Notmuch to populate the address book using previous email
correspondents:

```text
# ...
[compose]
# ...
address-book-cmd='notmuch address "%s"'
# ...
```

Account configuration (a junk `postpone` setting is specified since
postponing emails, i.e. adding drafts, does not work with Lieer, see
[link](https://github.com/gauteh/lieer/issues/64)):

```sh
cat > $HOME/.config/aerc/accounts.conf <<EOF
[GMail]
source         = notmuch://$HOME/.mail/
check-mail-cmd = notmuch new
outgoing       = $HOME/.local/bin/gmi-send
query-map      = $HOME/.config/aerc/map.conf
default        = Inbox
from           = User name goes here <username@emailserver.com>
postpone       = Drafts-DOESNOTWORKFORNOTMUCH
EOF
```

"Folder" mapping for the account in aerc:

```sh
cat > $HOME/.config/aerc/map.conf <<EOF
All=
Drafts=tag:draft
Inbox=tag:inbox and not tag:archived and not tag:deleted
Sent=tag:sent
Starred=tag:flagged
Trash=tag:deleted
EOF
```

For keybindings, copy over the default bindings...

```sh
cp -a $AERCTGT/usr/share/aerc/binds.conf $HOME/.config/aerc/
```

Modify `$HOME/.config/aerc/binds.conf` to reflect the following, which
changes the message deletion bindings to tag messages appropriately
and disables the default continue draft composition binding:

```text
# ...
[messages]
# ...
# d = :prompt 'Really delete this message?' 'delete-message'<Enter>
# D = :delete<Enter>
# A = :archive flat<Enter>
d = :modify-labels -inbox +deleted<Enter>
D = :modify-labels +inbox -deleted<Enter>
u = :modify-labels +unread<Enter>
U = :modify-labels -unread<Enter>
s = :modify-labels +flagged<Enter>
S = :modify-labels -flagged<Enter>
a = :modify-labels -inbox<Enter>
A = :modify-labels +inbox<Enter>
# ...
<C-r> = :check-mail<Enter>
[messages:folder=Drafts]
# <Enter> = :recall<Enter>  # disabled, lieer doesn't work well with GMail drafts
# ...
[view]
# ...
# S = :save<Enter>
W = :save<space>
| = :pipe<space>
# D = :delete<Enter>
# A = :archive flat<Enter>
d = :modify-labels -inbox +deleted<Enter>:close<Enter>
D = :modify-labels +inbox -deleted<Enter>
u = :modify-labels +unread<Enter>
U = :modify-labels -unread<Enter>
s = :modify-labels +flagged<Enter>
S = :modify-labels -flagged<Enter>
a = :modify-labels -inbox<Enter>:close<Enter>
A = :modify-labels +inbox<Enter>
# ...
```

Create a wrapper for running `aerc` with the appropriate libraries on
the path:

```sh
cat > $HOME/.local/bin/aerc <<EOF
#!/bin/sh

LD_LIBRARY_PATH=$NOTMUCHDIR/lib $AERCTGT/bin/aerc "\$@"
EOF
```

Calling `aerc` on the command line should now use the wrapper
(assuming `$HOME/.local/bin` is a directory in `$PATH`).

If using [Sourcehut](https://sr.ht/), collaboration is primarily over
email as of writing, so some additional bindings to better facilitate
the `git send-email` workflow will be useful. See
[link](https://drewdevault.com/2022/07/25/Code-review-with-aerc.html).

Note that the above instructions is for a single email account,
and the setup for multiple email accounts would have changes.

## Working with large codebases

Grep (and ripgrep) can be used to search in moderately-sized codebases
but does not scale well to large ones. There are instead indexers and
index searching tools that should be used instead for those scenarios.
These include Code Search, hound, zoekt, Xapian, and so on. Usage of
Code Search, one of the simpler options, is illustrated here.

### Installing and using Code Search

To install Code Search, make sure Go is installed and run

```sh
go install -v github.com/google/codesearch/cmd/...@latest
```

Code Search includes three command-line tools. These are:

- `cgrep`: Like `grep` but with `pcregrep`-like regexp syntax. Not discussed here, use it like one would `grep`.

- `cindex`: Code indexer.

- `csearch`: Index searcher.

`cindex` is used to index files in given directories (includes files
in their subdirectories), e.g.

```sh
cindex /path/to/repository1 /path/to/repository2  # and so on...
```

To reindex currently indexed directories, just run `cindex` without any arguments.

```sh
cindex
```

To search the index, use `csearch`. Usage examples:

```sh
# Line containing 'foo' or 'Foo' across all indexed files
csearch '[Ff]oo'
# Lines in a given repo starting with 'func ', show line number
csearch -n -f '^/path/to/repo' '^func '
# Lines containing any case-insensitive variation of 'bar'
csearch -i 'bar'
```

There is also `cgrep` which can be used just like `grep`, the only
difference being the regexp syntax it uses is PCRE. It is useful
when searching unindexed files and maintaining regexp consistency
with `csearch` usage. For example, a small set of test files can be
constructed and `cgrep` used to iterate on regexps for a query before
it is run on a huge indexed codebase.

```console
$ cat 'a^2 + b^2' > sometestfile.txt
$ grep 'a^2' sometestfile.txt
a^2 + b^2
$ cgrep 'a^2' sometestfile.txt
$ cgrep 'a\^2' sometestfile.txt
sometestfile.txt:a^2 + b^2
```

For more usage info on these three commands, run them with `--help`
option, i.e., `cgrep --help`, `cindex --help` and `csearch --help`.

### Code Search index file and how to use multiple indexes

The location of the index is controlled by `$CSEARCHINDEX`
(environment variable). When `$CSEARCHINDEX` is not specified, the
file `~/.csearchindex` is used as the index (and is created if it does
not exist). This means the default behavior is that all indexing is
into a single global file.

To have an index per codebase or per group of codebases, `direnv` or
`shadowenv` can be used to set `$CSEARCHINDEX` automatically when
entering or leaving specific directories. For example, assuming
`direnv` is installed, a codebase-specific `$CSEARCHINDEX` can be
configured to be automatically set and unset when entering and leaving
that codebase's directory.

First, install `direnv` or `shadowenv` if needed. E.g., for `direnv`:

```sh
# If needed, install direnv and hook it into the shell, see
# https://direnv.net/docs/installation.html
# Following installs with Go and assumes shell is Zsh
go install github.com/direnv/direnv@latest
cat >>$HOME/.zshrc <<EOF
# direnv
eval "$(direnv hook zsh)"
EOF
source $HOME/.zshrc
```

Codebase-specific setup of direnv to automatically switch the index
when entering and leaving the code directory can be done as follows
(also provided as the `utils/.local/csearch-setup` script):

```sh
# Go to code directory
cd /path/to/repository/
# Setup direnv config for this codebase
cat >>.envrc <<EOF
export CSEARCHINDEX=$PWD/.csearchindex
EOF
direnv allow .
# Index the codebase
cindex $PWD
# Run if a Git repository to avoid syncing index and direnv config
cat >>.gitignore <<EOF
# direnv config
/.envrc
# Code search index
/.csearchindex
EOF
```

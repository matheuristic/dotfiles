# Software notes

## Open-source software

- Audio and video creation and editing
  - [Ardour](https://ardour.org/):
    Multitrack audio recorder, works
    with [ReaPlugs](https://www.reaper.fm/reaplugs/) VST plugins
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
  - [Foliate](https://johnfactotum.github.io/foliate/)
- File management
  - [Cyberduck](https://cyberduck.io/):
    Remote FTP, SFTP, WebDAV, cloud storage; macOS and Windows
  - [Double Commander](https://doublecmd.sourceforge.io/):
    GUI Midnight Commander clone
  - [Magic Wormhole](https://github.com/magic-wormhole/magic-wormhole):
    Command-line tool and library for sending files from one computer to
    another; note that this tool requires
    [mailbox](https://github.com/magic-wormhole/magic-wormhole-mailbox-server)
    and a
    [transit relay](https://github.com/magic-wormhole/magic-wormhole-transit-relay)
    server, which by default uses one hosted by the project
  - [Midnight Commander](https://midnight-commander.org/) or:
    [nnn](https://github.com/jarun/nnn) or
    [lf](https://github.com/gokcehan/lf) or
    [broot](https://github.com/Canop/broot):
    Terminal file manager
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
  - [MuPDF](https://mupdf.com/):
    PDF reader
  - [QPDF](https://github.com/qpdf/qpdf):
    PDF transformations
  - [sioyek](https://github.com/ahrm/sioyek) or
    [Zathura](https://pwmt.org/projects/zathura/):
    PDF reader for research and technical PDFs
  - [Skim](https://skim-app.sourceforge.io/):
    PDF reader and annotator; macOS
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
  - [gitg](https://gitlab.gnome.org/GNOME/gitg) or
    [Gitup](https://github.com/git-up/GitUp) or
    [TortoiseGit](https://tortoisegit.org/):
    Git GUI client; gitg is Linux-only and installable via Flathub,
    Gitup is macOS-only, TortoiseGit is Windows-only
  - [gogs](https://gogs.io/) or
    [Soft Serve](https://github.com/charmbracelet/soft-serve):
    Self-hosted Git server
  - [onefetch](https://github.com/o2sh/onefetch):
    Git repository information and statistics
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
  - [Code Search](https://github.com/google/codesearch):
    Index with `cindex` then search with `cgrep` or `csearch`
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
    Network utilization by process, connection, remote IP, hostname, etc
  - [dua](https://github.com/Byron/dua-cli):
    Check disk usage and delete unwanted data
  - [duf](https://github.com/muesli/duf) or
    [lfs](https://github.com/Canop/lfs):
    Command-line alternative to `df` with nicer user interface
  - [fkill](https://github.com/sindresorhus/fkill-cli):
    Command-line tool to interactively kill running user and system procs
  - [forkstat](https://github.com/ColinIanKing/forkstat):
    Command-line program to log process forks, execs and exits;
    useful for tracking runaway processes
  - [htop](https://github.com/htop-dev/htop) or
    [bottom](https://github.com/ClementTsang/bottom):
    System resource monitor, alternative to `top`
  - [Sloth](https://sveinbjorn.org/sloth):
    macOS application to show open files, dirs, sockets and pipes
  - [Stacer](https://github.com/oguzhaninan/Stacer):
    Linux system optimizer and monitoring GUI tool
  - [eBPF/bcc](https://github.com/iovisor/bcc) or
    [strace](https://strace.io/):
    Process debugging;
    eBPF/bcc [tutorial](https://ish-ar.io/python-ebpf-tracing/)
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
  - [LanguageTool](https://languagetool.org/):
    Style and grammar checker; standalone Java version downloadable
    [here](https://languagetool.org/download/) with snapshots available
    [here](https://internal1.languagetool.org/snapshots/) (the link
    [link](https://languagetool.org/download/LanguageTool-stable.zip)
    has the latest version), which is
    [augmentable](https://dev.languagetool.org/finding-errors-using-n-gram-data.html)
    with [n-gram](https://languagetool.org/download/ngram-data/) data
  - [Meld](https://meldmerge.org/) or
    [kdiff3](https://apps.kde.org/kdiff3/) or
    [xxdiff](https://github.com/blais/xxdiff):
    GUI `diff` alternative; Meld supports Windows and Linux, which has
    also a [macOS port](https://github.com/yousseb/meld), kdiff3
    is Linux-only, xxdiff is lightweight but does not support Unicode
  - [sttr](https://github.com/abhimanyu003/sttr):
    Command-line tool for string operations
- Text tools (structured)
  - [fq](https://github.com/wader/fq):
    Like `jq` but for binary formats
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
  - [Visidata](https://www.visidata.org/):
    Terminal tabular data multitool, supports sources loadable via
    Pandas using the `-f` option
  - [yq](https://github.com/mikefarah/yq):
    Command-line YAML processor
- User experience and interface (graphical)
  - [LinearMouse](https://github.com/linearmouse/linearmouse) or
    [Mos](https://github.com/Caldis/Mos):
    Mouse enhancements like reverse scrolling, linear scrolling,
    acceleration, etc, for external mice on macOS
  - [skhd](https://github.com/koekeishiya/skhd):
    Hotkey daemon; macOS
  - [Rectangle](https://github.com/rxhanson/Rectangle):
    Move and resize windows using keyboard shortcuts and snap areas;
    macOS
  - [ueli](https://ueli.app/):
    Launcher like [Alfred](https://www.alfredapp.com/) but open-source
    and available for Windows in addition to macOS
  - [Unshaky](https://github.com/aahung/Unshaky):
    Double keypress workaround for butterfly keyboards; macOS
- User experience and interface (text)
  - [direnv](https://github.com/direnv/direnv) or
    [shadowenv](https://github.com/Shopify/shadowenv):
    Load and unload env vars based on location
  - [entr](https://github.com/eradman/entr/):
    Run command when files change
  - [hollywood](https://github.com/dustinkirkland/hollywood):
    Hollywood technobabble in a Byobu session
  - [parallel](https://www.gnu.org/software/parallel/):
    Command-line tool for executing jobs in parallel
  - [pv](http://www.ivarch.com/programs/pv.shtml):
    Like `cat` but prints progress to stderr
  - [rlwrap](https://github.com/hanslub42/rlwrap):
    `readline` wrapper to enable completion and history for any
    command-line tool taking keyboard input
  - [tmux](https://github.com/tmux/tmux):
    Terminal multiplexer, useful for managing and persisting remote
    sessions over Mosh or SSH
  - [ttyplot](https://github.com/tenox7/ttyplot):
    Real-time plotting tool in the terminal using stdin as data input
- Virtualization
  - [UTM](https://github.com/utmapp/UTM):
    iOS and macOS tool for managing [QEMU](https://www.qemu.org/)
    virtual machines
  - [virt-manager](https://virt-manager.org/):
    Linux desktop tool for managing QEMU/KVM virtual machines
- VPN
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
  - [Ka-Block!](http://kablock.com/)
    Safari extension for blocking ads and trackers; iOS and macOS
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
    limited (no key files; generates a token to be used by recipient)
  - [Al Dente](https://github.com/davidwernhart/AlDente):
    macOS tool to limit battery charging (e.g. keeping charge
    percentage at or below 80% can help prolong battery life)
  - [Anki](https://apps.ankiweb.net/):
    Flashcards software
  - [ClamAV](https://www.clamav.net/):
    Open-source antivirus engine, with frontends available like
    [ClamTk](https://gitlab.com/dave_m/clamtk)
  - [Diagon](https://github.com/ArthurSonzogni/Diagon):
    Command-line tool for transforming Markdown-style expressions into
    ASCII art, [webapp](https://github.com/ArthurSonzogni/Diagon) and
    [snap](https://snapcraft.io/diagon) available
  - [grex](https://github.com/pemistahl/grex):
    Generate regex from test cases
  - [Hammerspoon](https://www.hammerspoon.org/):
    Use [Lua](https://www.lua.org/) for macOS scripts that can call
    system APIs, for example middle-click-move mouse to scroll
    ([link](https://superuser.com/questions/303424/can-i-enable-scrolling-with-middle-button-drag-in-os-x))
  - [hyperfine](https://github.com/sharkdp/hyperfine):
    Benchmark shell commands
  - [Joplin](https://joplinapp.org/):
    Note-taking application with sync support; cross-platform
  - [OmegaT](https://omegat.org/):
    Translation memory tool
  - [pass](https://www.passwordstore.org/):
    Command-line password manager
  - [rgb-tui](https://github.com/ArthurSonzogni/rgb-tui):
    Terminal color-picker
  - [sc-im](https://github.com/andmarti1424/sc-im)
    Terminal spreadsheet program
  - [Sweet Home 3D](https://www.sweethome3d.com/):
    Open-source interior design application, also available as an online
    [webapp](https://www.sweethome3d.com/SweetHome3DOnlineManager.jsp)
  - [Taskwarrior](https://taskwarrior.org/) and
    [Timewarrior](https://timewarrior.net/):
    Todo list management (Taskwarrior) and time-tracking (Timewarrior)
  - [Watch](https://pkg.go.dev/9fans.net/go/acme/Watch):
    Run command when files change, Acme editor-specific
  - [Zotero](https://www.zotero.org/):
    Reference management software to collect, organize, cite and share
    research material

## Commercial software

- Diagramming
  - [iThoughts](https://www.toketaware.com/):
    Mind-mapping tool
  - [Monodraw](https://monodraw.helftone.com/):
    ASCII art editor; macOS
- File management
  - [DevonThink Pro](https://www.devontechnologies.com/apps/devonthink):
    Document management and search solution; macOS
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
- Word processing
  - [Scrivener](https://www.literatureandlatte.com/scrivener/overview):
    Word processor for authoring books and screenplays

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
mamba create -n devtools
mamba activate devtools
mamba install bat black htop mdformat pandoc ripgrep
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/devtools/bin/bat
ln -s $HOME/mambaforge/envs/devtools/bin/black
ln -s $HOME/mambaforge/envs/devtools/bin/htop
ln -s $HOME/mambaforge/envs/devtools/bin/mdformat
ln -s $HOME/mambaforge/envs/devtools/bin/pandoc
ln -s $HOME/mambaforge/envs/devtools/bin/rg
mamba deactivate
```

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
mamba create -n devtools
mamba activate devtools
mamba install bat git htop ripgrep tmux
cd $HOME/.local/bin
ln -s $HOME/mambaforge/envs/devtools/bin/bat .
ln -s $HOME/mambaforge/envs/devtools/bin/git .
ln -s $HOME/mambaforge/envs/devtools/bin/htop .
ln -s $HOME/mambaforge/envs/devtools/bin/rg .
ln -s $HOME/mambaforge/envs/devtools/bin/tmux .
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

To install [tensorflow-text](https://github.com/tensorflow/text) with
Metal-support, see
[here](https://github.com/sun1638650145/Libraries-and-Extensions-for-TensorFlow-for-Apple-Silicon)
for compile instructions and pre-compiled wheels, and see
[here](https://github.com/tensorflow/text/pull/756) for more info.

### Installing XCode command-line tools

```sh
sudo rm -rf /Library/Developer/CommandLineTools
sudo xcode-select --install
```

### Installing and using Spack

Adapted from:
https://spack.readthedocs.io/en/latest/getting_started.html#installation

Spack is one of few package managers that does not require root
permissions by default.

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

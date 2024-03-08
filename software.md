# Software notes

## Open-source software

- Audio and video creation and editing
  - [Ardour](https://ardour.org/) or
    [Zrythm](https://www.zrythm.org/):
    Digital audio workstation; Ardour works with
    [ReaPlugs](https://www.reaper.fm/reaplugs/) VST plugins; Ardour
    seems more suited to audio recording and engineering, while Zrythm
    seems more suited to MIDI electronic music production
  - [Blackhole](https://existential.audio/blackhole/):
    Route audio between apps (e.g., to record internal audio), macOS
  - [Blender](https://www.blender.org/)
    ([Github](https://github.com/blender/blender)):
    3D creation suite covering the full 3D animation pipeline
  - [Handbrake](https://handbrake.fr/):
    Video encoder
  - [Kdenlive](https://kdenlive.org/en/):
    Video editor; alternative is [ShotCut](https://shotcut.org/)
  - [LMMS](https://lmms.io/):
    Music sequencer
  - [LosslessCut](https://github.com/mifi/lossless-cut):
    Tool for cutting/trimming audio/video files without re-encoding
  - [MediaInfo](https://mediaarea.net/en/MediaInfo)
    ([Github](https://github.com/MediaArea/MediaInfo)):
    Tool for showing technical and tag data of audio and video files
  - [Natron](https://natrongithub.github.io/)
    ([Github](https://github.com/NatronGitHub/Natron)):
    Cross-platform video compositing software, like commercial
    [Nuke](https://www.foundry.com/products/nuke-family/nuke) by The
    Foundry and Adobe After Effects, or freeware DaVinci Resolve
    [Fusion](https://www.blackmagicdesign.com/products/davinciresolve/fusion)
  - [sfxr-qt](https://github.com/agateau/sfxr-qt):
    SFX creator for games; Qt port of
    [SFXR](http://www.drpetter.se/project_sfxr.html);
    [JavaScript](https://github.com/chr15m/jsfxr) port available
    [online](https://sfxr.me/); also built into LMMS
  - [Synfig](https://github.com/synfig/synfig/):
    2D vector animation
  - [Tooll 3](https://github.com/still-scene/t3):
    Tool for creating real-time motion graphics for Windows
  - [Ultimate Vocal Remover GUI](https://github.com/Anjok07/ultimatevocalremovergui):
    Tool for using open-source deep learning models (e.g.,
    [Demucs](https://github.com/facebookresearch/demucs) and
    [MDX-NET](https://github.com/kuielab/mdx-net)) to isolate
    vocals from audio tracks; a donation-supported web interface
    [MVSEP](https://mvsep.com/) is also available
  - [xACT](http://xact.scottcbrown.org/):
    Audio format converter, macOS
- Audio and video playback and streaming
  - [NDI Tools](https://www.ndi.tv/tools/):
    Software for low-latency broadcasting over LAN by taking any video
    source, like a webcam, video capture card or desktop, and making
    that an NDI source accessible by other computers on the network
  - [OBS Studio](https://obsproject.com/):
    Software for broadcasting video streams with
    [very](https://obsproject.com/forum/resources/downstream-keyer.1254/)
    [many](https://obsproject.com/forum/resources/closed-captioning-via-google-speech-recognition.833/)
    [different](https://sammisolutions.itch.io/sammi)
    [plugins](https://github.com/Xaymar/obs-StreamFX/wiki)
  - [VLC](https://www.videolan.org/vlc/):
    Media player
- Backup
  - [BorgBackup](https://www.borgbackup.org/):
    File backup tool; frontends available like
    [Vorta](https://vorta.borgbase.com/)
  - [dar](http://dar.linux.free.fr/):
    File archiving, supports recovery records using
    [par2](https://github.com/Parchive/par2cmdline/)
  - [Timeshift](https://github.com/linuxmint/timeshift):
    Like Windows System Restore but for Linux
- Calendar and task management
  - [Remind](https://dianne.skoll.ca/projects/remind/)
    ([Git repository](https://salsa.debian.org/dskoll/remind)):
    Scriptable calendar and alarm program, with
    [CalDAV](https://github.com/jspricke/remind-caldav) (Google) and
    [iCalendar](https://github.com/jspricke/python-remind) integration
    along with CLI ([link1](https://sr.ht/~mlaparie/remint/),
    [link2](https://gitlab.com/wyrd-calendar/wyrd)) and GUI
    (`tkremind` that comes with Remind) front-ends
  - [Taskwarrior](https://taskwarrior.org/) and
    [Timewarrior](https://timewarrior.net/):
    Todo list management (Taskwarrior) and time-tracking
    (Timewarrior); [syncall](https://github.com/bergercookie/syncall)
    can be used to sync Taskwarrior tasks with various services like
    Asana, Google Calendar, Google Keep and so on
- Compilers and linkers
  - [Mold](https://github.com/rui314/mold):
    Drop-in replacement for Linux linkers on various architectures;
    for macOS support, there is a commercial version
    [sold](https://github.com/bluewhalesystems/sold)
- Containerization
  - [colima](https://github.com/abiosoft/colima) or
    [Rancher Desktop](https://rancherdesktop.io/)
    ([Github](https://github.com/rancher-sandbox/rancher-desktop/)):
    Minimal-setup container runtimes on macOS with support for Docker,
    containerd, or Kubernetes; uses [lima](https://lima-vm.io/), which
    is like WSL2 for macOS, under the hood; alternative to Docker
    Desktop; for more info, see
    [link](https://www.cncf.io/blog/2023/02/02/docker-on-macos-is-slow-and-how-to-fix-it/)
  - [ctop](https://github.com/bcicen/ctop):
    `top` for containers
  - [Docker](https://docs.docker.com/)
    [Desktop](https://docs.docker.com/desktop/install/mac-install/)
- Data analytics
  - [JASP](https://jasp-stats.org/):
    GUI statistical analysis tool
  - [Netron](https://github.com/lutzroeder/netron):
    Neural network, deep learning and machine learning model viewer
  - [Paraview](https://www.paraview.org/)
    ([Github](https://github.com/Kitware/ParaView)):
    Cross-platform tool for scientific visualization
  - [PSPP](https://www.gnu.org/software/pspp/):
    Open-source SPSS alternative
  - [Veusz](https://veusz.github.io/):
    Scientific plotting and graphing
- Data pipeline
  - [Apache Airflow](https://airflow.apache.org/):
    Scheduling workflows
  - [Apache HOP](https://hop.apache.org/):
    Fork of [Kettle/PDI](https://github.com/pentaho/pentaho-kettle)
  - [Snakemake](https://snakemake.readthedocs.io/en/stable/) or
    [Nextflow](https://www.nextflow.io/):
    Run batch workflows
- Database
  - [BaseX](https://basex.org/):
    XML database and XQuery processor; useful for analyzing or
    processing many or huge XML files
    ([example](https://yobriefca.se/blog/2014/05/17/a-brief-overview-of-basex/))
  - [ClickHouse](https://github.com/ClickHouse/ClickHouse):
    Time-series DB, can also run queries on local or
    remote files without installing a database server using
    [clickhouse-local](https://clickhouse.com/docs/en/operations/utilities/clickhouse-local)
  - [Litestream](https://litestream.io/)
    ([Github](https://github.com/benbjohnson/litestream)) and
    [LiteFS](https://github.com/superfly/litefs):
    Not a DB, but SQLite tools for streaming backup of a SQLite DB to
    cloud storage (Litestream) and replicating SQLite databases across
    a cluster using a FUSE-based filesystem (LiteFS)
  - [MongoDB](https://www.mongodb.com/)
    ([Github](https://github.com/mongodb/mongo)):
    NoSQL database
  - [PostgreSQL](https://www.postgresql.org/):
    Relational DB; use the
    [TimescaleDB](https://github.com/timescale/timescaledb)
    plugin to better support time-series, or the
    [Citus](https://github.com/citusdata/citus) plugin
    for schema-based sharding
  - [rqlite](https://rqlite.io/)
    ([Github](https://github.com/rqlite/rqlite)):
    Distributed DB using SQLite as its storage engine
  - [TigerBeetle](https://tigerbeetle.com/)
    ([Github](https://github.com/tigerbeetledb/tigerbeetle)):
    Distributed high-throughput fault-tolerant database targeting
    financial accounting use cases; long-term plans are to extract the
    core into a library applicable to any state machine business logic
    (see [here](https://news.ycombinator.com/item?id=32788795) and
    [here](https://news.ycombinator.com/item?id=32787324))
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
  - [Mathesar](https://mathesar.org/)
    ([Github](https://github.com/centerofci/mathesar)):
    PostgreSQL web client with a spreadsheet-like web interface
  - [OctoSQL](https://github.com/cube2222/octosql):
    Make queries that can span MySQL, PostgreSQL databases along with
    CSV and JSON
  - [pgcli](https://www.pgcli.com/):
    PostgreSQL command-line client
  - [q](https://github.com/harelba/q) or
    [columnq](https://github.com/roapi/roapi/tree/main/columnq-cli) or
    [clickhouse-local](https://clickhouse.com/docs/en/operations/utilities/clickhouse-local) or
    [glaredb](https://github.com/GlareDB/glaredb):
    Command-line tool for running SQL directly on CSV and other
    tabular files; or use sqlite3
    ([link](https://til.simonwillison.net/sqlite/one-line-csv-operations))
  - [RedisDesktopManager](https://github.com/uglide/RedisDesktopManager):
    Redis client
  - [SchemaCrawler](https://www.schemacrawler.com/)
    ([Github](https://github.com/schemacrawler/SchemaCrawler)):
    Database schema discovery and comprehension tool; supports schema
    [search](https://www.schemacrawler.com/schemacrawler-grep.html),
    [diagramming](https://www.schemacrawler.com/diagramming.html) and
    [scripting](https://www.schemacrawler.com/scripting.html) on any
    database with a JDBC driver
  - [Sequel Ace](https://github.com/Sequel-Ace/Sequel-Ace):
    MySQL, MariaDB client; macOS
- Desktop publishing
  - [Scribus](https://www.scribus.net/)
- Diagramming and image editing
  - [ASCIIFlow](https://asciiflow.com/)
    ([Github](https://github.com/lewish/asciiflow)):
    Web-based ASCII drawing application, which can be run locally
    with [Bazel](https://bazel.build/) (install the Bazel launcher
    [Bazelisk](https://github.com/bazelbuild/bazelisk) to pick
    the right Bazel version; for live reloading, install and use
    [ibazel](https://github.com/bazelbuild/bazel-watcher));
    commercial Monodraw is more full-featured; an
    alternative is [MonoSketch](https://monosketch.io/)
    ([Github](https://github.com/tuanchauict/MonoSketch))
  - [Caire](https://github.com/esimov/caire):
    Smart image resizing
  - [Diagon](https://github.com/ArthurSonzogni/Diagon):
    Command-line tool for transforming Markdown-style expressions into
    ASCII art, [webapp](https://github.com/ArthurSonzogni/Diagon) and
    [snap](https://snapcraft.io/diagon) available
  - [Diagrams](https://diagrams.mingrammer.com/)
    ([Github](https://github.com/mingrammer/diagrams)):
    Create cloud system architecture diagrams using Python code
  - [ditaa](https://github.com/stathissideris/ditaa) or
    [goat](https://github.com/blampe/goat):
    Command-line utility to convert ASCII diagrams to PNG or SVG;
    an alternative is the webapp
    [svgbob](http://ivanceras.github.io/svgbob-editor/)
    ([Github](https://github.com/ivanceras/svgbob))
  - [drawio-desktop](https://github.com/jgraph/drawio-desktop) or
    [yEd](https://www.yworks.com/products/yed):
    Vector diagramming; drawio-desktop is the Electron build of
    [diagrams.net](https://www.diagrams.net/); alternatives are
    [Excalidraw](https://excalidraw.com/)
    ([Github](https://github.com/excalidraw/excalidraw))
  - [GIMP](https://www.gimp.org/) or
    [Krita](https://krita.org/en/):
    Raster graphics editor
  - [Inkscape](https://inkscape.org/):
    Vector graphics editor
  - [LinkedIdeas](https://github.com/fespinoza/LinkedIdeas):
    Mind-mapping tool
  - [Markmap](https://markmap.js.org/)
    ([Github](https://github.com/markmap/markmap)):
    Visualize Markdown as a mind-map; also available as a
    [Webapp](https://markmap.js.org/repl) or VSCode
    [plugin](https://marketplace.visualstudio.com/items?itemName=gera2ld.markmap-vscode)
  - [Moebius](https://blocktronics.github.io/moebius/)
    ([Github](https://github.com/blocktronics/moebius)) or
    [Pablodraw](https://picoe.ca/products/pablodraw/)
    ([Github](https://github.com/cwensley/pablodraw)):
    Cross-platform ANSI and ASCII art editor
  - [Pikchr](https://pikchr.org/) or
    [PlantUML](https://plantuml.com/) or
    [Mermaid](https://mermaid.js.org/):
    Markup language for diagramming, standalone or supported by
    Markdown processers like Pandoc via plugins
  - [UMLet](https://github.com/umlet/umlet)
    ([Github](https://github.com/umlet/umlet),
    [Webapp](https://www.umletino.com/), VSCode
    [plugin](https://marketplace.visualstudio.com/items?itemName=TheUMLetTeam.umlet)):
    Java GUI tool for drawing UML diagrams; alternative is
    [Gaphor](https://gaphor.org/)
    ([Github](https://github.com/gaphor/gaphor))
- Digital design
  - [Penpot](https://github.com/penpot/penpot):
    Open source design and prototyping platform like Sketch and Figma
- Ebook authoring
  - [Sigil](https://sigil-ebook.com/)
    ([Github](https://github.com/Sigil-Ebook/Sigil)):
    Cross-platform ebook editor supporting EPUB
- Ebook reader or library manager
  - [Alexandria](https://github.com/btpf/Alexandria):
    Cross-platform Tauri- and epub.js-based ebook reader
    (Windows and Linux currently supported, macOS planned)
  - [Calibre](https://calibre-ebook.com/)
    ([Github](https://github.com/kovidgoyal/calibre)):
    Ebook manager supporting an array of formats, with many plugins
  - [epr](https://github.com/wustho/epr) or
    [epy](https://github.com/wustho/epy):
    TUI ebook reader; epr supports only EPUB; epy is a fork of epy
    with support for more formats like MOBI and AZW3, and adds
    features like bookmarks, integration with external dictionaries,
    and inline formatting
  - [epub2txt2](https://github.com/kevinboone/epub2txt2):
    Extract text from EPUB
  - [Foliate](https://johnfactotum.github.io/foliate/):
    Linux ebook viewer
  - [MuPDF](https://mupdf.com/):
    See _PDF reader or transformer_
  - [Thorium Reader](https://www.edrlab.org/software/thorium-reader/)
    ([Github](https://github.com/edrlab/thorium-reader)):
    Cross-platform Electron ebook reading app
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
  - [FUSE](https://en.wikipedia.org/wiki/Filesystem_in_Userspace):
    Lets users mount filesystems without superuser privileges; usually
    installed as a dependency by Linux package managers, for example,
    `apt install rclone` on Debian will also pull in `fuse` or `fuse3`
    as a suggested package; on macOS, install
    [FUSE-T](https://www.fuse-t.org/)
    ([Github](https://github.com/macos-fuse-t/fuse-t), kext-less) or
    [macFUSE](https://osxfuse.github.io/)
    ([Github](https://github.com/osxfuse/osxfuse), uses a kext), with
    the latter being more mature but requiring third-party kernel
    extensions be enabled
  - [Keka](https://www.keka.io/en/):
    GUI file archiver, macOS; download from website is free, while
    App Store version is paid (basically donationware)
  - [Maestral](https://maestral.app/)
    ([Github](https://github.com/samschott/maestral)):
    Dropbox client, lighter but has less features than the
    [official](https://www.dropbox.com/desktop) client
  - [Magic Wormhole](https://github.com/magic-wormhole/magic-wormhole):
    Command-line tool and library for sending files from one computer
    to another; note that this tool requires
    [mailbox](https://github.com/magic-wormhole/magic-wormhole-mailbox-server)
    and a
    [transit relay](https://github.com/magic-wormhole/magic-wormhole-transit-relay)
    server, which by default uses one hosted by the project
  - [Midnight Commander](https://midnight-commander.org/) or
    [nnn](https://github.com/jarun/nnn) or
    [lf](https://github.com/gokcehan/lf) or
    [broot](https://github.com/Canop/broot):
    Terminal file manager
  - [PeaZip](https://peazip.github.io/):
    File manager and file archiver
  - [rclone](https://rclone.org/)
    ([Github](https://github.com/rclone/rclone)):
    Like `rsync` but for cloud storage; `rclone mount` can be used to
    mount cloud storage to a filesystem mount point (requires FUSE)
  - [rdfind](https://github.com/pauldreik/rdfind) or
    [fdupes](https://github.com/adrianlopezroche/fdupes):
    Command-line tool to find duplicate files
  - [renameutils](https://www.nongnu.org/renameutils/) or
    [f2](https://github.com/ayoisaiah/f2) or
    [KRename](https://userbase.kde.org/KRename) or
    [GPRename](http://gprename.sourceforge.net/) or
    [Szyszka](https://github.com/qarmin/szyszka):
    Rename lots of files easily
  - [Spacedrive](https://spacedrive.com/)
    ([Github](https://github.com/spacedriveapp/spacedrive)):
    Cross-platform cross-device/service file manager
  - [SSHFS](https://github.com/libfuse/sshfs)
    ([fork](https://github.com/deadbeefsociety/sshfs)):
    Mount remote filesystem using SSH (requires FUSE),
    alternative is `rclone mount` (see rclone) or
    some more complex setup using NFSv4 and Wireguard
    ([example](https://sigmdel.ca/michel/ha/wireguard/network_share_over_wireguard_en.html))
  - [Stow](https://www.gnu.org/software/stow/):
    Symlink farm manager, useful for managing dotfiles
  - [Syncthing](https://syncthing.net/):
    Continous file synchronization; macOS
    [frontend](https://github.com/syncthing/syncthing-macos)
    available
- Image text extraction
  - [Apache Tika](http://tika.apache.org/):
    [Go](https://github.com/google/go-tika)
    [Python](https://github.com/chrismattmann/tika-python) bindings
    available
  - [Tesseract](https://github.com/tesseract-ocr/tesseract)
- Markup language document conversion and rendering
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
    Document converter, can also process Markdown extendable with
    filters like [this one](https://github.com/pandoc-ext/diagram)
    facilitating diagramming using code blocks
  - [Quarto](https://quarto.org/)
    ([Github](https://github.com/quarto-dev/quarto-cli)):
    Publishing system built on Pandoc, successor to R Markdown,
    supporting Markdown and Jupyter notebooks with dynamic content
    using Python, R, Julia, and Observable JS; for a user-local
    install, one option is to extract the tarball version somewhere
    and symlink the `quarto` binary from a directory in `$PATH`
  - [Tectonic](https://tectonic-typesetting.github.io/) or
    [TeXLive](https://www.tug.org/texlive/) or
    [TinyTeX](https://github.com/yihui/tinytex):
    Processor for (La)TeX, which is used widely in scientific writing
  - [TeXmacs](https://texmacs.org/)
    ([Github](https://github.com/texmacs/texmacs)):
    WYSIWYG scientific writing software, that is an alternative
    to LaTeX but can import/export LaTeX files; there is an a
    distribution of TeXmacs called [Mogan](https://mogan.app/)
    ([Github](https://github.com/XmacsLabs/mogan)) that aims to be
    more user-friendly
  - [Typst](https://typst.app/)
    ([Github](https://github.com/typst/typst)):
    Markup-based typesetting system, that is an alternative to LaTeX
  - [WeasyPrint](https://github.com/Kozea/WeasyPrint) or
    [percollate](https://github.com/danburzo/percollate):
    Convert HTML to PDF files; percollate processes the page for
    readability before conversion; the file can also be directly
    opened in a browser and printed to PDF
- Newsreader
  - [NetNewsWire](https://github.com/Ranchero-Software/NetNewsWire):
    RSS reader; iOS and macOS
  - [Newsboat](https://newsboat.org/)
    ([Github](https://github.com/newsboat/newsboat)):
    CLI RSS reader
  - [Pan](http://pan.rebelbase.com/):
    Usenet reader
- Networking
  - [LuLu](https://objective-see.org/products/lulu.html)
    ([Github](https://github.com/objective-see/LuLu)):
    Firewall for macOS; while the built-in firewall only filters
    incoming connections, this can also filter outgoing connections
  - [Netiquette](https://objective-see.org/products/netiquette.html)
    ([Github](https://github.com/objective-see/Netiquette)):
    Network monitor, macOS
- Package/runtime manager
  - [asdf](https://asdf-vm.com/):
    Language runtime manager with plugins
    [plugins](https://github.com/asdf-vm/asdf-plugins) for many
    language runtimes and tools (like `bat`, `jq`, `ripgrep`, etc)
  - [Flatpak](https://flatpak.org/):
    Application manager; [Flathub](https://flathub.org/) is the de
    facto repository; for easier permissioning control install
    [Flathub](https://flathub.org/apps/details/com.github.tchx84.Flatseal)
  - [MacPorts](https://www.macports.org/) or
    [Spack](https://spack.io/) or
    [Homebrew](https://brew.sh/):
    macOS package manager for open-source software
  - [Mamba](https://mamba.readthedocs.io/)
    ([Github](https://github.com/mamba-org)):
    Drop-in replacement for [conda](https://conda.io/), with the
    [micromamba](https://github.com/mamba-org/micromamba-releases)
    the preferred variant; using [conda-forge](https://conda-forge.org/)
    as the primary repository is recommended
  - [Rhumba](https://github.com/mamba-org/rhumba):
    R package manager installable via conda or mamba
- PDF reader or transformer
  - [diffpdf](https://tracker.debian.org/pkg/diffpdf)
    ([source](http://www.qtrac.eu/diffpdf-foss.html)):
    Diff two PDF files, Can be used with `git` by setting
    `git config --global difftool.diffpdf.cmd 'diffpdf "$LOCAL" "$REMOTE"'`
    and `git config --global alias.diffpdf "difftool -t diffpdf"` and
    running e.g. `git diffpdf somecommit:somefile.pdf somefile.pdf`;
    Linux
  - [DjVu.js](https://djvu.js.org/)
    ([Github](https://github.com/RussCoder/djvujs)):
    View DjVu documents in the browser; for local viewer usage
    instructions, see [here](https://djvu.js.org/downloads)
  - [MuPDF](https://mupdf.com/):
    PDF, XPS and EPUB reader; AGPL version is available in many
    package managers like APT (Linux), Homebrew or MacPorts (macOS)
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
- Presentation
  - [Marp](https://marp.app/):
    Markdown-based presentations; use via the command-line tool
    [marp-cli](https://github.com/marp-team/marp-cli)
  - [pdfpc](https://github.com/pdfpc/pdfpc):
    Multi-monitor PDF presentations
  - [slides](https://github.com/maaslalani/slides):
    Terminal-based presentations using Markdown files
- Project (codebase)
  - [CodeQuery](https://ruben2020.github.io/codequery/)
    ([Github](https://github.com/ruben2020/codequery)):
    Code-browsing and -understanding tool, building on
    [cscope](http://cscope.sourceforge.net/) and
    [ctags](http://ctags.sourceforge.net/), providing a GUI interface
    to these tools and visualization capabilities; supports C, C++,
    Go, Java, Javascript, Python and Ruby; complementary tools include
    [cflow](https://www.gnu.org/software/cflow/) to analyze C code or
    [Doxygen](https://www.doxygen.nl/) to extract code structure
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
    Ubuntu, `port install editorconfig-core-c` via MacPorts on macOS);
    [editorconfig-checker](https://editorconfig-checker.github.io/)
    can be used to check that files conform to EditorConfig settings
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
  - [Moose](https://modularmoose.org/)
    ([Github](https://github.com/moosetechnology/Moose)):
    Platform for static analysis of software (better described
    [here](http://agilevisualization.com/AgileVisualization/Moose/0306-Moose.html))
    built on [Pharo](https://pharo.org/); for more info, see
    [here](http://themoosebook.org/book/); source code parsers for
    generating Moose models are available for
    [Java](https://github.com/moosetechnology/VerveineJ),
    [Fortran](https://github.com/NicolasAnquetil/VerveineF),
    [C#](https://github.com/feenkcom/roslyn2famix) and
    [C/C++](https://github.com/moosetechnology/VerveineC-Cpp), or
    [create](https://github.com/moosetechnology/PetitParser)
    [one](https://vimeo.com/139004257); alternative is
    [Glamorous Toolkit](https://gtoolkit.com/)
    ([Github](https://github.com/feenkcom/gtoolkit))
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
  - [cpu](https://github.com/u-root/cpu):
    Implementation of Plan 9 [cpu](https://man.cat-v.org/plan_9/1/cpu)
    command in Go, modified to work over SSH; useful for logging into
    remote systems while allowing use of files from the local system,
    (e.g., 1. cpu into an embedded system with limited disk capacity
    and running binaries from the local system, or 2. cpu into an
    x86-64 Linux machine from an Apple Silicon macOS system to develop
    for and run on the Linux system while keeping the code repository
    and code output on the macOS system); currently requires the
    `cpud` daemon to be run as root on remote Linux systems as `mount`
    is a privileged operation in Linux, and if running on a system
    using `systemd` the daemon should be started with `cpud -d -init`
    (see [discussion](https://github.com/u-root/cpu/issues/59));
    [u-root](https://github.com/u-root/u-root), which is a fully
    Go userland, can be useful in conjunction with cpu to work with
    remote machines with minimal disk space
  - [Mosh](https://mosh.org/):
    Robust SSH alternative; `dumb` terminals don't work well with mosh
    because it always outputs escape sequences to set the window title
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
  - [fzf](https://github.com/junegunn/fzf) or
    [zf](https://github.com/natecraddock/zf):
    Command-line fuzzy finder; zf has better filename matching
    ([link](https://nathancraddock.com/blog/2023/a-different-approach-to-fuzzy-finding/))
  - [pdfgrep](https://pdfgrep.org/):
    Command-line tool for searching text in PDF files
  - [ripgrep](https://github.com/BurntSushi/ripgrep):
    `grep` alternative, extend to more file formats with
    [ripgrep-all](https://github.com/phiresky/ripgrep-all)
- Shell enhancement
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
  - [eBPF/bcc](https://github.com/iovisor/bcc) or
    [strace](https://strace.io/):
    Process debugging;
    eBPF/bcc [tutorial](https://ish-ar.io/python-ebpf-tracing/)
  - [fkill](https://github.com/sindresorhus/fkill-cli):
    Command-line tool to interactively kill running user and system
    procs
  - [forkstat](https://github.com/ColinIanKing/forkstat):
    Command-line program to log process forks, execs and exits;
    useful for tracking runaway processes
  - [GrandPerspective](https://grandperspectiv.sourceforge.net/):
    Visualize disk usage using a tree map, macOS
  - [htop](https://github.com/htop-dev/htop) or
    [zenith](https://github.com/bvaisvil/zenith) or
    [bottom](https://github.com/ClementTsang/bottom):
    System resource monitor, alternative to `top`; note that dumb
    terminals or logging ncurses should not be used, in which case do
    `top -b` to run `top` in batch mode (or `top -b -n NUMBER` to
    limit the number of iterations to `NUMBER`)
  - [hyperfine](https://github.com/sharkdp/hyperfine):
    Command-line benchmarking tool that supports multiple and warmup
    runs, export of results to various formats, etc; basically a
    featureful alternative to the standard `time` command
  - [klogg](https://klogg.filimonov.dev/)
    ([Github](https://github.com/variar/klogg)):
    Cross-platform GUI log explorer
  - [lnav](https://lnav.org/)
    ([Github](https://github.com/tstack/lnav)):
    TUI log file viewer supporting multiple file formats and
    compression types, with the ability to consolidate multiple
    log files into a single view
  - [pstree](https://github.com/FredHucht/pstree):
    List processes as a tree
  - [Sloth](https://sveinbjorn.org/sloth):
    macOS application to show open files, dirs, sockets and pipes
  - [Stacer](https://github.com/oguzhaninan/Stacer):
    Linux system optimizer and monitoring GUI tool
  - [tree](http://mama.indstate.edu/users/ice/tree/):
    Command-line tool to list files in subdir tree depth-indented
- Text (general)
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
    list on Unix or Linux systems); install `enchant-2` with APT on
    Debian and Ubuntu; install `enchant-2` with MacPorts on macOS
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
    (note that the `ngrams-en-20150817.zip` file should be unzipped to
    `~/languagetool/ngram-data/en/`, modify for other languages)
  - [FileMerge](https://developer.apple.com/xcode/features/) or
    [Meld](https://meldmerge.org/) or
    [kdiff3](https://apps.kde.org/kdiff3/) or
    [tkdiff](https://sourceforge.net/projects/tkdiff/) or
    [xxdiff](https://github.com/blais/xxdiff) or
    [P4Merge](https://www.perforce.com/products/helix-core-apps/merge-diff-tool-p4merge):
    GUI `diff` alternative; Meld supports Windows and Linux and has a
    [macOS port](https://github.com/yousseb/meld), kdiff3 is
    [cross-platform](https://binary-factory.kde.org/), xxdiff is
    lightweight but does not support Unicode, FileMerge comes with the
    macOS XCode IDE and callable from the command-line using
    `opendiff`, tkdiff is lightweight and straightforward to set up on
    macOS (see _Mac Notes_ > _Graphical diff and merge tool_), P4Merge
    is free but not open-source; these also diff directories
  - [par](http://www.nicemice.net/par/):
    Paragraph reformatter, like a smarter version of `fmt` from GNU
    [coreutils](https://www.gnu.org/software/coreutils/); note that
    macOS built-in `fmt` does not support Unicode, while `par` and
    GNU Coreutils `fmt` (installable as `gfmt` from the MacPorts
    or Homebrew `coreutils` package on macOS) do support Unicode
  - [sttr](https://github.com/abhimanyu003/sttr):
    Command-line tool for string operations
  - [uni](https://github.com/arp242/uni) or
    [chars](https://github.com/antifuchs/chars):
    Command-line tool for querying Unicode characters
- Text (structured)
  - [The One True Awk](https://github.com/onetrueawk/awk):
    Version of AWK described in The Awk Programming Language
    [book](https://awk.dev/); alternative implementations include
    [Gawk](https://www.gnu.org/software/gawk/manual/) and
    [others](https://www.gnu.org/software/gawk/manual/html_node/Other-Versions.html)
  - [csvquote](https://github.com/dbro/csvquote):
    Makes its easier to use CSV files with standard Unix tools like
    awk, sed, cut and join by converting (and reverting) embedded
    commas and newlines to non-printing characters, for example
    `csvquote a.csv | cut -d',' -f3 | sort | uniq -c | csvquote -u`
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
    GUI tabular data viewer for CSV, Parquet, SQLite and DuckDB files;
    alternative is [CSView](https://kothar.net/csview) (different from
    another one that is listed here)
  - [Visidata](https://www.visidata.org/) or
    [CSView](https://github.com/wfxr/csview):
    TUI tabular data viewer; Visidata is more of a multitool, in that
    it allows for editing and supports any source loadable via Pandas
    using the `-f` option
  - [yq](https://github.com/mikefarah/yq):
    Command-line YAML processor
- Text editor or integrated development environment
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
  - [Glamorous Toolkit](https://gtoolkit.com/)
    ([Github](https://github.com/feenkcom/gtoolkit)): Workbench for
    software and knowledge management built on the
    [Pharo](https://pharo.org/) Smalltalk environment
    ([link](https://lepiter.io/feenk/glamorous-toolkit-and-pharo-9q25tavxwfq6z1drwvegd5u9o/));
    some examples of what it can do out of the box include analyzing
    C++ code ([link](https://gtoolkit.com/docs/analyzing-cpp/)),
    running and debugging Javascript and Python code
    ([link](https://lepiter.io/feenk/introducing-lepiter--knowledge-management--e2p6apqsz5npq7m4xte0kkywn/)),
    and so on; for more info, see [blog](https://lepiter.io/feenk/) or
    [paper](https://scg.unibe.ch/archive/papers/Chis15c-PracticalDomainSpecificDebuggers.pdf)
  - [Helix](https://helix-editor.com/):
    TUI editor like Kakoune, but designed to have many editor features
    like [LSP](https://microsoft.github.io/language-server-protocol/)
    support built into the editor
  - [Kakoune](https://kakoune.org/):
    TUI editor like vi, but implements an object-verb command model
    rather than vi's verb-object command-model; designed to call out
    to the shell for most features beyond basic editing
  - [Lapce](https://lapce.dev/)
    ([Github](https://github.com/lapce/lapce)):
    Batteries-loaded fast cross-platform code editor
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
    GUI IDE; VSCodium is a build of VSCode that is free
    of tracking and Microsoft branding, but note that some
    Microsoft plugins like pylance (Python LSP server) does
    not support it; some useful plugins include LLM-based
    assistant [Continue](https://continue.dev/docs/intro)
    ([Github](https://github.com/continuedev/continue)) and the
    different extensions for remote development include over SSH
    ([link](https://code.visualstudio.com/docs/remote/remote-overview))
  - [Zed](https://zed.dev/)
    ([Github](https://github.com/zed-industries/zed)):
    Multithreaded GPU-accelerated collaborative code editor
- User experience and interface (graphical)
  - [Amethyst](https://ianyh.com/amethyst/)
    ([Github](https://github.com/ianyh/Amethyst):
    Tiling window manager for macOS
  - [Barrier](https://github.com/debauchee/barrier):
    Cross-platform software that mimics KVM switch functionality
  - [Hyperkey](https://hyperkey.app/):
    Use CapsLock as a "hyper" key, that is, C-Opt-Cmd-Shift, on macOS
  - [LinearMouse](https://github.com/linearmouse/linearmouse) or
    [Mos](https://github.com/Caldis/Mos) or
    [Scroll Reverser](https://github.com/pilotmoon/Scroll-Reverser):
    Mouse enhancements like reverse scrolling, linear scrolling,
    acceleration, etc, for external mice on macOS
  - [Maccy](https://maccy.app/)
    ([Github](https://github.com/p0deje/Maccy)):
    Clipboard manager; macOS
  - [MiddleClick](https://github.com/artginzburg/MiddleClick-Sonoma):
    Middle-click with three-finger tap; commercial alternatives
    include [Middle](https://middleclick.app/),
    [Multitouch](https://multitouch.app/) and
    [BetterTouchTool](https://folivora.ai/)
  - [Rectangle](https://rectangleapp.com/)
    ([Github](https://github.com/rxhanson/Rectangle)):
    Move and resize windows using keyboard shortcuts and snap areas
    in macOS; as an alternative, three of the more useful keyboard
    shortcuts in Rectangle can be replicated using _Preferences_ >
    _Keyboard_ > _Keyboard Shortcuts_ > _App Shortcuts_ and clicking
    on `+`, keeping `All Applications`, inserting as the Menu Title
    action `Move Window to Left Side of Screen` and setting the
    shortcut key as desired (`Ctrl-Option-Left` is recommended),
    and repeating for other actions `Move Window to Right Side
    of Screen` (recommend `Ctrl-Option-Right` here), as well as
    `Zoom` (recommend using `Ctrl-Option-Return` here), **or**
    hover over the green `+` button on the top-left corner of the
    window and press `Option` which allows mouse selection of the
    actions (does not require keyboard shortcut configuration);
    there's also a commercial ([Pro](https://rectangleapp.com/pro)
    version with more features
  - [shottr](https://shottr.cc/):
    Screenshot app for macOS, although note that macOS has built-in
    screenshot taking via `Cmd-Shift-3` (capture full screen),
    `Cmd-Shift-4` (select a region and capture it),
    `Cmd-Shift-4-Space` (capture window), and via the
    Screenshot app (`Cmd-Shift-5` or use Spotlight search)
  - [skhd](https://github.com/koekeishiya/skhd):
    Hotkey daemon; macOS
  - [stats](https://github.com/exelban/stats):
    Menu bar system monitor; macOS
  - [Textinator](https://github.com/RhetTbull/textinator)
    or [TRex](https://github.com/amebalabs/TRex):
    Detect text in screenshots and copy it to the clipboard; macOS
  - [ueli](https://ueli.app/):
    Launcher like [Alfred](https://www.alfredapp.com/) but open-source
    and available for Windows in addition to macOS
  - [xbar](https://github.com/matryer/xbar) or
    [SwiftBar](https://github.com/swiftbar/SwiftBar):
    Pipe output to the menu bar; macOS
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
    [link](https://old.reddit.com/r/archlinux/comments/d41c1w/screen_vs_tmux/f0dj9rn/);
    for just the session management (detach and re-attach)
    functionality of tmux and screen, there is
    [dtach](https://github.com/crigler/dtach),
    [abduco](https://github.com/martanne/abduco) or
    [diss](https://github.com/yazgoo/diss/)
  - [ttyplot](https://github.com/tenox7/ttyplot):
    Real-time plotting tool in the terminal using stdin as data input
- Virtualization
  - [Blink](https://github.com/jart/blink):
    Tiny x86-64-linux emulator that can run on any POSIX system
  - [UTM](https://github.com/utmapp/UTM):
    iOS and macOS tool for managing [QEMU](https://www.qemu.org/)
    virtual machines; alternatives are [lima](https://lima-vm.io/)
    ([Github](https://github.com/lima-vm/lima)) if there is no need to
    run GUI apps, or Ubuntu-only [Multipass](https://multipass.run/)
    ([Github](https://github.com/canonical/multipass))
  - [virt-manager](https://virt-manager.org/):
    Linux desktop tool for managing QEMU/KVM virtual machines
- VPN
  - [headscale](https://github.com/juanfont/headscale) or
    [innernet](https://github.com/tonarino/innernet) or
    [nebula](https://github.com/slackhq/nebula) or
    [netbird](https://github.com/netbirdio/netbird):
    Overlay networking tool for creating a private network, like
    [Tailscale](https://tailscale.com/); especially, headscale is
    an open-source self-hosted Tailscale control server alternative
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
  - [Hush](https://oblador.github.io/hush/):
    Safari extension to block nag popups and trackers; iOS and macOS
  - [Lagrange](https://gmi.skyjake.fi/lagrange/)
    ([Github](https://github.com/skyjake/lagrange)) or
    [Kristall](https://kristall.random-projects.net/)
    ([Github](https://github.com/MasterQ32/kristall)):
    Gemini GUI client; Kristall also supports Gopher
  - [Monolith](https://github.com/Y2Z/monolith.git):
    Save complete webpages to a single HTML file with embedded CSS,
    images and Javascript
  - [shot-scraper](https://github.com/simonw/shot-scraper):
    Command-line utility for taking partial or full screenshots of
    websites
  - [YT-DLP](https://github.com/yt-dlp/yt-dlp):
    Fork of [youtube-dl](https://youtube-dl.org/), example usage
    is `yt-dlp --list-formats URL` (change `URL` to video's URL)
    to list formats and `yt-dlp 123 URL` to download just format
    `123` from the listed formats (usually one with combined video
    and audio) or `yt-dlp -f 123+456` to download formats `123` and
    `456` followed by merging them (usually one video and one audio)
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
  - [diffoscope](https://diffoscope.org/)
    ([Salsa](https://salsa.debian.org/reproducible-builds/diffoscope)):
    Like `diff` but supports archives and directories in addition to
    files, and can output to formats like HTML, JSON, Markdown and
    reStructuredText in addition to text; as a pure Python package, it
    is installable via `pip` from [PyPI](https://pypi.org/)
  - [elfcat](https://github.com/ruslashev/elfcat):
    ELF visualizer using HTML files generated from ELF binaries
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
  - [FontForge](https://fontforge.org/)
    ([Github](https://github.com/fontforge/fontforge)):
    Open-source cross-platform scriptable font editor
  - [Goxel](https://goxel.xyz/)
    ([Github](https://github.com/guillaumechereau/goxel)):
    3D voxel editor
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
  - [Jiggler](http://www.sticksoftware.com/software/Jiggler.html)
    ([Github](https://github.com/bhaller/Jiggler)):
    Keep the system awake, useful when running lengthy tasks, macOS
  - [Joplin](https://joplinapp.org/):
    Note-taking application with sync support; cross-platform
  - [kgt](https://github.com/katef/kgt/):
    Convert between BNF syntaxes and visualize using railroad diagrams
  - [LCDF Typetools](https://www.lcdf.org/type/)
    ([Github](https://github.com/kohler/lcdf-typetools)):
    Font manipulation utilities, `otfinfo` is useful for getting
    available features and code points in TTF and OTF fonts; if using
    MacPorts, `port install lcdf-typetools -texlive` installs the
    utilities without pulling in TexLive as a dependency
  - [libfsm](https://github.com/katef/libfsm):
    NFA, DFA (finite automata), regex and lexical analysis tools;
    [re](https://github.com/katef/libfsm/blob/main/doc/tutorial/re.md)
    can compile regex to state machine and output as a `.dot` diagram
  - [Mathics](https://mathics.org/)
    ([Github](https://github.com/Mathics3)):
    Open-source alternative to
    [Mathematica](https://www.wolfram.com/mathematica/)
  - [Nextcloud](https://nextcloud.com/)
    ([Github](https://github.com/nextcloud)):
    Self-host collaboration platform offering similar functionality to
    Google Suite; Nextcloud Files can be used as a a WebDAV service
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
    see [link](https://gist.github.com/abtrout/d64fb11ad6f9f49fa325);
    for completion support when installing from source, see `INSTALL`
    in the pass source or directly copy the completion files to the
    appropriate directories (e.g., for Zsh, copy the file
    `src/completion/pass.zsh_completion` to a file with name prefixed
    by `_` in an `$fpath` dir, say `$HOME/.zsh_completions/_pass`)
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
  - [Vale](https://vale.sh/)
    ([Github](https://github.com/errata-ai/vale)):
    Linter for prose
  - [Velja](https://sindresorhus.com/velja):
    Open different links in different browsers or apps, macOS
  - [Zotero](https://www.zotero.org/):
    Reference management software to collect, organize, cite and
    share research material

## Commercial software

- Audio and video creation and editing
  - [DaVinci Resolve](https://www.blackmagicdesign.com/products/davinciresolve):
    Freeware non-linear video editor, has paid
    [version](https://www.blackmagicdesign.com/products/davinciresolve/studio)
- Containerization
  - [Orbstack](https://orbstack.dev/):
    macOS Docker runtime and Linux virtual machine management,
    more performant and lightweight than Docker Desktop and Colima
    as of July 2023; free for personal use
- Database client
  - [DbVisualizer](https://www.dbvis.com/):
    Cross-database SQL client with data visualization capabilities;
    free version available with limited functionality
- Diagramming
  - [iThoughts](https://www.toketaware.com/):
    Mind-mapping tool
  - [Monodraw](https://monodraw.helftone.com/):
    ASCII art editor; macOS (App Store and direct download)
- Ebook authoring:
  - [Vellum](https://vellum.pub/) or
    [Atticus](https://www.atticus.io/):
    Format ebooks and print books for publication; Vellum is
    macOS-only and expensive but more polished, while Atticus is
    less polished but cross-platform and cheaper
- File management
  - [DevonThink Pro](https://www.devontechnologies.com/apps/devonthink):
    Document management and search solution; macOS
  - [Mountain Duck](https://mountainduck.io/):
    Like `rclone mount` but with a nicer user interface and its vaults
    are interoperable with Cryptomator vaults
- Markup language document conversion and rendering
  - [AntennaHouse](https://www.antennahouse.com/) or
    [Prince](https://www.princexml.com/):
    Convert HTML to PDF files; Prince is free for non-commercial use
    (note that the free version watermarks the resulting PDF file)
- Project (codebase)
  - [Sublime Merge](https://www.sublimemerge.com/):
    GUI Git client for macOS
- Remote login and desktop
  - [Jump Desktop](https://jumpdesktop.com/):
    Remote desktop software, connect to host systems that have
    [Jump Desktop Connect](https://jumpdesktop.com/connect/)
    installed; macOS and Windows
  - [Royal TSX](https://royalapps.com/ts/):
    Cross-platform RDP and VNC client; has free version
- Text (general)
  - [Araxis Merge](https://www.araxis.com/merge/) or
    [Beyond Compare](https://www.scootersoftware.com/) or
    [Deltawalker](https://www.deltawalker.com/) or
    [Kaleidoscope](https://kaleidoscope.app/):
    Compare files and folders; Deltawalker and Kaleidoscope
    also support diffing images
- Text (structured)
  - [Modern CSV](https://www.moderncsv.com/):
    Cross-platform CSV editor/viewer
- Text editor or integrated development environment
  - [DataGrip](https://www.jetbrains.com/datagrip/):
    Database admin-oriented IDE
  - [DataSpell](https://www.jetbrains.com/dataspell/):
    Data science-oriented IDE
  - [PyCharm](https://www.jetbrains.com/pycharm/):
    Python programming-oriented IDE; has open-source community edition
  - [RStudio](https://rstudio.com/):
    IDE for R; has open-source edition
  - [Sublime Text](https://www.sublimetext.com/):
    Lightweight text editor
- User experience and interface (graphical)
  - [Synergy](https://symless.com/synergy)
    ([Github](https://github.com/symless/synergy-core/)):
    Cross-platform software mimicking KVM switch functionality to share
    a keyboard and a mouse across systems; source code available for
    [self-compile](https://symless.com/synergy/news/how-to-use-synergy-for-free),
    but is also available compiled by a third-party as
    [binaries](https://github.com/DEAKSoftware/Synergy-Binaries)
- Word processing
  - [Scrivener](https://www.literatureandlatte.com/scrivener/overview):
    Word processor for authoring books and screenplays
- Virtualization
  - Orbstack (see Containerization)

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

### Anaconda, Miniconda, Miniforge, Mambaforge, Micromamba

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

[Micromamba](https://github.com/mamba-org/micromamba-releases)
is a statically-linked single executable conda package manager.
It does not come pre-configured for any specific repo.

It is usually best to install Miniconda, Miniforge, Mambaforge or
Micromamba because of the smaller disk footprint since they don't
come pre-installed with a wide array of packages.

It is recommended that any code development, and even tools where
multiple versions need to exist on the system, be installed into
specific environments (whether using Conda or some other tool).

### Workflow examples

An example workflow is shown here, with some useful commands. See the
[documentation](https://docs.conda.io/en/latest/) for more details on
how to use the package manager.

Using `conda` here but `mamba` and `micromamba` support the same
params except where noted.

```sh
# List environments
conda env list
# Create a new environment using a specific channel-only
conda env create -n envname -c bioforge --override-channels
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

### Micromamba install example

Manual install, assumes `$HOME/.local/bin` is in `$PATH`, ZSH is
the user shell, and `$HOME/micromamba/` used for micromamba envs:

```sh
# Download binary and place into a directory on the path
curl -Ls https://micro.mamba.pm/api/micromamba/osx-arm64/latest | tar -xvj $HOME/.local/bin/micromamba
# Set up micromamba in shell config
micromamba shell init -s zsh -p $HOME/micromamba
# Use conda-forge repo and don't auto-activate base env
micromamba config append channels conda-forge
micromamba config append channels nodefaults
micromamba config set channel_priority strict
micromamba config set auto_activate_base false
```

(Or for a wizard-based install, run
`"${SHELL}" <(curl -L micro.mamba.pm/install.sh)`
in the console.)

Set up a tools environment and install some simple tooling:

```sh
micromamba create tools
micromamba activate tools
micromamba install ripgrep pandoc
```

Notes:

- R packages begin with an `r-` prefix but only popular packages
  are available and these should not be mixed with the packages
  installed using R's `package.install` mechanism. It is better
  to use [rhumba](https://github.com/mamba-org/rhumba) to manage
  packages from within R if running R from a conda installation.
- Conda has an `emacs` package, but it is generally
  better to compile, go with a native package-manager
  distributed Eamcs, or on macOS one of the standalone Emacs
  applications ([Emacs for Mac OSX](https://emacsformacosx.com/)
  [emacs-mac](https://github.com/railwaycat/homebrew-emacsmacport)
  port) as they are better integrated with the system.
- On macOS, conda can provide a basic CLI dev environment without
  XCode or its command-line tools installed. However, it is
  recommended to install at least the command-line tools.

## Python virtual environments

Conda (see above) can work as a virtual environment manager.

However, for developing Python code it's probably
better to use a dedicated Python version manager
like [pyenv](https://github.com/pyenv/pyenv) or
[asdf](https://github.com/asdf-vm/asdf) to control Python
version, and Python-native virtual environment tooling like
[venv](https://docs.python.org/3/library/venv.html) to create an
isolated Python environment for the installing packages specific
to the code being developed.

### Using specific Python versions

Use [pyenv](https://github.com/pyenv/pyenv) to install and switch
between different versions of Python.

Install pyenv (replace `$HOME/.bashrc` with `$HOME/.zshrc` if
using Zsh as in the case of the macOS default shell):

```sh
curl https://pyenv.run | bash
cat >>$HOME/.bashrc <<EOF
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
EOF
```

Examples of commonly used pyenv commands:

- `pyenv update` updates pyenv and its installed plugins
- `pyenv versions` shows available versions
- `pyenv install -l` shows installable versions
- `pyenv install <version>` installs the specified Python version,
  e.g., `pyenv install 3.11.8`
- `pyenv shell <version>` uses the specified Python version for
  the current shell session
- `pyenv local <version>` uses the specific Python version while
  in the current directory when running `pyenv exec`
- `pyenv global <version>` sets the global default Python version
- `pyenv exec <command> [args...]` runs the given command with
  the given arguments with the local (or global if there isn't one
  configured) Python version activated

An alternative is to use [asdf](https://github.com/asdf-vm/asdf):

```sh
# Installation (replace version as needed), restart shell after
git clone https://github.com/asdf-vm/asdf.git "$HOME/.asdf" --branch v0.14.0
asdf plugin-add python
# Common commands
asdf update                   # Update asdf to latest stable version
asdf current                  # Display current lang runtime versions
asdf list all python          # List available Python versions
asdf list python              # List installed versions
asdf install python 3.11.8    # Install Python version 3.11.8
asdf shell python 3.11.8      # Use Python 3.11.8 in current shell
asdf shell python --unset     # Unset asdf-set Python version in shell
asdf local python 3.11.8      # Use Python 3.11.8 in current dir
asdf global python 3.11.8     # Use Python 3.11.8 by default
asdf uninstall python 3.11.8  # Uninstall Python 3.11.8
```

### Global Python virtual environments

Use [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv)
to manage global virtual environments for Python (the following
creates a virtual environment named `myenv` using Python version
`3.11.8`, change as needed):

```sh
pyenv virtualenvs              # list virtualenvs
pyenv virtualenv 3.11.8 myenv  # create virtualenv
pyenv activate myenv           # activate virtualenv
pyenv deactivate               # deactivate current virtualenv
pyenv virtualenv-delete myenv  # delete virtualenv
```

These global virtual environments can be used to install Python
tooling, and a wrapper script can be created in a directory
on the `$PATH`. Example:

```sh
pyenv virtualenv 3.11.8 myenv
pyenv activate myenv
pip install black
pyenv deactivate
cat >$HOME/.local/bin/black <<EOF
#!/bin/zsh
pyenv activate myenv
black $@
EOF
chmod +x $HOME/.local/bin/black
```

### Project-specific Python virtual environments

Set up a virtual environment for a project using a specific Python
version installed via pyenv (version `3.11.8` is used here, change
as needed), at a `.venv` subdirectory in the project folder:

```sh
cd /path/to/project/dir
pyenv local 3.11.8               # or `asdf local python 3.11.8`
pyenv exec python -m venv .venv  # or `python -m venv .venv`
```

Activate the virtual environment:

```sh
cd /path/to/project/dir
source .venv/bin/activate
```

Deactivate an activated virtual environment:

```sh
deactivate
```

It is possible to automate activation and deactivation of the virtual
environment using [direnv](https://github.com/direnv/direnv), see
[here](https://github.com/direnv/direnv/wiki/Python).

## Linux notes

Some notes also apply to BSD systems.

### APT usage tips

Usage tips for [APT](https://en.wikipedia.org/wiki/APT_(software))
which is the default package manager for
[Debian](https://www.debian.org/) and [Ubuntu](https://ubuntu.com/).

- Reverse dependencies of `PACKAGE`:

  ```sh
  apt-cache rdepends \
      --no-{suggests,conflicts,breaks,replaces,enhances} \
      --installed --recurse \
      PACKAGE
  ```

- Install `PACKAGE` without its recommended packages:

  ```sh
  apt-get install --no-install-recommends PACKAGE
  ```

- Simulate installing or removing `PACKAGE`:

  ```sh
  apt-get install --dry-run PACKAGE
  apt-get remove --dry-run PACKAGE
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
  (some websites are only fully compatible with this; note that if
  `Google LLC` and `GoogleUpdater` are disabled from starting at
  login and are not allowed Full Disk Access, the browser needs to
  be updated manually by downloading the new version and installing
  over the current version)
- [Firefox](https://www.mozilla.org/en-US/firefox/new/)
  (for compatibility with more powerful extensions, see
  [link](https://blog.mozilla.org/addons/2022/05/18/manifest-v3-in-firefox-recap-next-steps/));
  [browsh](https://www.brow.sh/)
  ([Github](https://github.com/browsh-org/browsh)) can be used to
  offload webpage rendering to headless Firefox on a remote server and
  accessing it in text via SSH or Mosh, which can be useful for
  browsing the internet on a low-power low-bandwidth client device
- [Mullvad Browser](https://github.com/mullvad/mullvadvpn-app)
  (privacy-focused web browser, based on Firefox)

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

- `coreutils` (GNU
  [coreutils](https://en.wikipedia.org/wiki/List_of_GNU_Core_Utilities_commands);
  MacPorts-installed GNU coreutils commands are prefixed with `g`, for
  example GNU `realpath` is installed as `grealpath`)
- `diffutils` (GNU
  [Diffutils](https://www.gnu.org/software/diffutils/manual/html_node/index.html);
  MacPorts installed GNU Diffutils commands are prefixed for `g`, for
  example GNU `diff` is installed as `gdiff`)
- `gawk` (GNU
  [AWK](https://www.gnu.org/software/gawk/manual/gawk.html))
- `git` (it is recommended to override the default system-wide macOS
  Git credential helper setting which is to store Git credentials
  in the macOS keychain, see below)
- `gnupg2`
- `gsed` (GNU [sed](https://www.gnu.org/software/sed/))
- `htop`
- `mosh`
- `pstree`
- `stow`
- `tree`

```sh
port -N install coreutils gawk git gnupg2 gsed mosh pstree stow tree
```

If installing `gnupg2` above, it is recommended to change the
`pinentry` symlink to `pinentry-tty` instead of the default
`pinentry-ncurses`. For more info, see the _Using TTY pinentry_
subsection of the _GnuPG_ section.

It is also recommended to override the default system-wide
Git credential helper setting. It is set to `osxkeychain` which
stores credentials using the system keychain. That can be undesired
behavior, especially when using another Git credential manager and
to avoid Git credentials being cached in unexpected places. See
_Overriding the default macOS Git credential helper_ in the _Git_
section.

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
port clean --all installed      # clean up temp build files
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
  which requires a lot of disk space be installed. Non-open source.
- `P4Merge`: Non-open source.
- `tkdiff`: typically installed via Homebrew (requires root privileges
  to set up) or manually installed (MacPorts version is pretty old),
  requires [Tk](https://www.tcl.tk/).
- `xxdiff`: typically installed via Homebrew (requires root privileges
  to set up) or MacPorts, and requires Qt which has many build
  dependencies if not installing pre-built binaries.

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

### iPad and iPhone integration with macOS

- [Continuity Camera](https://support.apple.com/en-us/HT213244):
  Use iPhone as a webcam for Mac
- [Continuity Camera](https://support.apple.com/en-us/HT209037):
  Use iPhone or iPad to scan documents or take a picture on a Mac
- [Continuity Markup and Sketch](https://support.apple.com/en-us/HT204975):
  Use iPad or iPhone to sketch in or mark up Mac documents
- [Sidecar](https://support.apple.com/en-us/HT210380):
  Use iPad as a second display for a Mac
- [Universal Control](https://support.apple.com/en-us/HT212757):
  Use a single keyboard and mouse between Mac and iPad
- [More info](https://www.apple.com/macos/continuity/)

### Working around "[SOFTWARE NAME] can't be opened because Apple cannot check it for malicious software"

Apps downloaded from the internet normally have a quarantine extended
attribute attached to them so opening them needs to first pass through
Gatekeeper's first run checks. For unverified and unnotarized
applications, a message like "[SOFTWARE NAME] can't be opened because
Apple cannot check it for malicious software" will pop up and the app
will not be opened.

Usually, it is sufficient to do one of the following:

- In Finder, Control-click the application icon and choose Open from
  the context menu.

- Go to System Settings > Privacy & Security, locate a section about
  trying to open the app and grant an exception for the app by
  clicking the Open Anyway button.

If the above approaches do not work, one can downloading the
application (usually a DMG file containing the application bundle) by
using `curl` or `wget` in Terminal to avoid the system automatically
setting the xattr on the application. The downloaded file will not
have its xattr set, so it (or its extracted application bundle) should
run without the Gatekeeper first run check.

Note that it is recommended to verify checksums or the GPG signature
of any application downloaded, especially if this workaround is
needed.

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
  test -t 0 && export GPG_TTY=$(tty)
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

### Overriding the default macOS Git credential helper

Both XCode command-line tools `git` and MacPorts `git` default to
using `osxkeychain` as a credential helper which will automatically
save Git passwords to the macOS keychain which is typically not the
desired behavior. (Run `git config --system --list` to see the
`git` default system settings of the installation.)

It is recommended to reset the credential helper in the global
config if managing Git credentials separately, or if storing Git
credentials in the system keychain is to be avoided.

For doing this, Git allows a higher priority config file to override
(i.e., clear) lower priority config settings by using an empty string
as a setting's value and adding new setting values after as desired.
The priority order of the different config scopes is _worktree_ >
_local_ > _global_ > _system_.  (For more info on config scopes,
see [here](https://git-scm.com/docs/git-config#FILES).)

**Example 1.** The following global settings override the system
credential helper settings and adds `netrc` as a credential
helper. (The helper needs to be set up first, see next section).

```sh
git config --global credential.helper ''
git config --global --add credential.helper 'netrc -f ~/.netrc.gpg -v'
```

**Example 2.** The following local settings override the
system and global credential helper settings and adds
[1Password](https://1password.com/) as a Git credential
[helper](https://github.com/develerik/git-credential-1password).
(The helper needs to be set up first.)

```sh
git config --local credential.helper ''
git config --local credential.helper '!git-credential-1password'
```

For more info, see
[here](https://stackoverflow.com/questions/13198143/how-do-i-disable-gits-credential-helper-for-a-single-repository)
and
[here](https://git-scm.com/docs/gitcredentials#Documentation/gitcredentials.txt-helper).

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
   git config --local --add credential.helper "netrc -f ~/.netrc.gpg -v"
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

### Track remote branch as local branch

When a remote repository is cloned, only the main (usually `main` or
`master`) branch is tracked locally. To develop on an existing remote
branch, it needs to be tracked locally, which can be done by using the
`--track` option for `git checkout`.

List remote branches:

```sh
git branch -r
```

Track remote branch locally (`REMOTENAME` must match one that was
output by `git branch -r`):

```sh
git checkout --track <REMOTENAME>
```

List all branches (branches not tracked locally are in red):

```sh
git branch -avv
```

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

In addition to code search tools, other code structure analysis tools
like CodeQuery, GNU cflow and Doxygen can be help understand code.

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
# A quoted heredoc limit string autoescapes special characters
go install github.com/direnv/direnv@latest
cat >>$HOME/.zshrc <<'EOF'
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

## Setting up Box sync on Linux

Windows and Linux has official support for mounting Box on the
filesystem ([Box Drive](https://www.box.com/resources/downloads)),
Linux does not. [rclone](https://rclone.org/) can be used instead.

### Install rclone binary

Install `rclone` either using a package manager or by
[downloading](https://rclone.org/downloads/) a static binary into a
directory in `$PATH`.

Also make sure FUSE is installed.

### Configure new rclone remote config for Box:

1. Run "rclone config".
1. Select "n" (new remote).
1. Enter a name for the new remote, e.g. "Box".
1. Enter the number associated with Box (number may change with rclone
   versions, it is "6" as of v1.52).
1. Leave blank for client id.
1. Leave blank for client secret.
1. Leave blank for box config file.
1. For box subtype, enter the number associated to act on behalf of
   "user" (or "enterprise" if it is an enterprise account).
1. Enter "n" (No) when asked whether to edit advanced config.
1. Enter "y" (Yes) when asked whether to use auto config.
1. A Box app web page to give authorization to rclone to access the
   Box account will open (may have to use the alternate URL with host
   127.0.0.1 if the initial redirection does not work). Log in to
   authorize rclone.
1. Select "y" (Yes) to indicate the config is ok.

### Usage

- List directories in the top level

  ```sh
  rclone lsd Box:
  ```

- List all files

  ```sh
  rclone ls Box:
  ```

- Mount Box to a given directory:

  Make sure mount directory exists. The example here uses `~/Box`.

  ```sh
  mkdir -p ~/Box
  ```

  Use `rclone mount` to create a FUSE mount. It is recommended to
  mount Box with caching enabled. This increases compatibility with
  most other apps since they typically assume the ability to do
  simultaneously read/writes to a file, butcomes at the cost of higher
  disk usage and files only syncing backremotely (to Box) when closed.

  ```sh
  rclone mount Box: ~/Box --vfs-cache-mode full
  ```

  See [here](https://rclone.org/commands/rclone_mount/) for more info.

- Unmount mounted Box directory:

  ```sh
  fusermount -u ~/Box
  ```

### Automount on system startup using systemd

- Systemd config file:

  Save the following to `~/.config/systemd/user/rclone-box.service`
  (change `USERNAME` and paths in `ExecStart` and/or `ExecStartPost`
  as needed). An alternative configuration where the directory
  structure, filenames and attributes are pre-cached and persisted is
  also included but commented out (this option is only safe when there
  are not multiple users uploading to the remote storage at the same
  time).

  ```desktop
  # ~/.config/systemd/user/rclone-box.service
  [Unit]
  Description=Box (rclone)
  After=

  [Service]
  Type=notify
  # Make sure rclone and mount point exists
  ExecStartPre=/usr/bin/test -x /home/USERNAME/.local/bin/rclone
  ExecStartPre=/usr/bin/test -d /home/USERNAME/Box
  ExecStartPre=/usr/bin/test -w /home/USERNAME/Box
  # Mount on start
  # The --rc flag starts rclone's remote control service.
  # This is pretty useful for the ability to manually refresh
  # the directory structure and file attributes before directory
  # cache expiry using
  # $ rclone rc vfs/refresh --fast-list recursive=true
  # ---
  ###
  # Option 1 (standard mount)
  ###
  ExecStart=/home/USERNAME/.local/bin/rclone mount Box: /home/USERNAME/Box --rc --vfs-cache-mode full
  ###
  # Option 2 (persistent cache and pre-caching on startup)
  # Increase --attr-timeout, --vfs-cache-max-age and --dir-cache-time
  # for more persistent cache. After mounting, pre-caching of the
  # directory structure, filenames and attributes is triggered using
  # a vfs/refresh command to the remote control API.
  # Note that this setup is only safe when there aren't multiple users
  # uploading the same file at the same time
  ###
  # ExecStart=/home/USERNAME/.local/bin/rclone mount Box: /home/USERNAME/Box --rc --vfs-cache-mode full --vfs-cache-max-age 720h --dir-cache-time 720h --attr-timeout 1m --poll-interval 30s
  # ExecStartPost=/home/USERNAME/.local/bin/rclone rc vfs/refresh recursive=true --rc-addr 127.0.0.1:5572 _async=true
  # ---
  # Unmount on stop
  ExecStop=/bin/fusermount -u /home/USERNAME/Box
  # Always restart if the process exists
  Restart=always
  RestartSec=10

  [Install]
  WantedBy=default.target
  ```

- Enabling autostart of service:

  ```sh
  systemctl --user enable rclone-box.service
  ```

- Disabling autostart of service:

  ```sh
  systemctl --user disable rclone-box.service
  ```

- Starting service manually:

  ```sh
  systemctl --user start rclone-box.service
  ```

- Stopping service manually:

  ```sh
  systemctl --user stop rclone-box.service
  ```

## Using dar and par2

dar and par2 are useful for data archival on nix platforms.

### dar

dar (disk archive) is an archiving tool that supports encryption. Also
supports recovery records if par2 is installed.

- Create archive:

  ```sh
  dar -c backup_file_without_extension -g file1 -g file2 ... -g fileN
  ```

- Extract from archive:

  ```sh
  dar -x backup_file_without_extension
  ```

- Use the `-K` or `--key` to encrypt the archive (AES is recommended):

  ```sh
  dar -c backup_file_without_extension -g file1 -K aes:some_pw_str
  ```

Additional info [here](http://dar.linux.free.fr/doc/index.html).

### par2

Create recovery records for files with par2.

- Create record:

  ```sh
  par2 create some_files.par2 file1 file2 file3
  ```

- Verify files:

  ```sh
  par2 verify some_files.par2
  ```

- Repair files:

  ```sh
  par2 repair some_files.par2
  ```

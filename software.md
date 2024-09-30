# Software notes

## Open-source software

(Note: ᴸ → Linux, ᴹ → Mac, ᵂ → Windows, ᴬ → Android, ᴵ → iOS,
ᴿ → Language runtime-based (e.g., Shell, Python, Java, Javascript)
with no platform restrictions; Many Linux tools are usable in Windows
via [WSL](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux)
or in Mac since it is POSIX-compliant.)

- Audio and video creation and editing
  - [Ardour](https://ardour.org/)ᴸᴹᵂ
    ([Github](https://github.com/Ardour/ardour)) or
    [Zrythm](https://www.zrythm.org/)ᴸᴹᵂ
    ([Gitlab](https://gitlab.zrythm.org/zrythm/zrythm)):
    Digital audio workstation; Ardour works with
    [ReaPlugs](https://www.reaper.fm/reaplugs/) VST plugins; Ardour
    seems more suited to audio recording and engineering, while Zrythm
    seems more suited to MIDI electronic music production
  - [AtomicParsley](https://github.com/wez/atomicparsley)ᴸᴹᵂ:
    MPEG-4 (e.g., `.mp4`, `.m4a`) metadata editor
  - [Blackhole](https://existential.audio/blackhole/)ᴹ
    ([Github](https://github.com/ExistentialAudio/BlackHole)):
    Route audio between apps (e.g., to record internal audio)
  - [Blender](https://www.blender.org/)ᴸᴹᵂ
    ([Github](https://github.com/blender/blender)):
    3D creation suite covering the full 3D animation pipeline
  - [eyeD3](https://eyed3.readthedocs.io/en/latest/)ᴿ
    ([Github](https://github.com/nicfit/eyeD3)) or
    [kid3](https://kid3.kde.org/)
    ([KDE Projects](https://invent.kde.org/multimedia/kid3/)) or
    [One Tagger](https://onetagger.github.io/)
    ([Github](https://github.com/Marekkon5/onetagger)) or
    [opustags](https://github.com/fmang/opustags) or
    [puddletag](https://docs.puddletag.net/)ᴿ
    ([Github](https://github.com/puddletag/puddletag)):
    Command-line (eyeD3 is mp3-only, opustags is Ogg Opus only) or GUI
    (kid3, One Tagger, puddletag) audio tag editor
  - [Handbrake](https://handbrake.fr/)ᴸᴹᵂ
    ([Github](https://github.com/HandBrake/HandBrake)):
    Video encoder
  - [Kdenlive](https://kdenlive.org/en/)ᴸᴹᵂ
    ([KDE Invent](https://invent.kde.org/multimedia/kdenlive)):
    Video editor; alternative is [ShotCut](https://shotcut.org/)ᴸᴹᵂ
    ([Github](https://github.com/mltframework/shotcut))
  - [LMMS](https://lmms.io/)ᴸᴹᵂ
    ([Github](https://github.com/LMMS/lmms)):
    Music sequencer
  - [LosslessCut](https://github.com/mifi/lossless-cut)ᴸᴹᵂ:
    Tool for cutting/trimming audio/video files without re-encoding
  - [MediaInfo](https://mediaarea.net/en/MediaInfo)ᴸᴹᵂᴬᴵ
    ([Github](https://github.com/MediaArea/MediaInfo)):
    Tool for showing technical and tag data of audio and video files
  - [Natron](https://natrongithub.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/NatronGitHub/Natron)):
    Cross-platform video compositing software, like commercial
    [Nuke](https://www.foundry.com/products/nuke-family/nuke) by The
    Foundry and Adobe After Effects, or freeware DaVinci Resolve
    [Fusion](https://www.blackmagicdesign.com/products/davinciresolve/fusion)
  - [sfxr-qt](https://github.com/agateau/sfxr-qt)ᴸ:
    SFX creator for games; Qt port of
    [SFXR](http://www.drpetter.se/project_sfxr.html)ᴸᵂ;
    [JavaScript](https://github.com/chr15m/jsfxr) port available
    [online](https://sfxr.me/); also built into LMMS
  - [SunVox](https://warmplace.ru/soft/sunvox/)ᴸᴹᵂᴬᴵ:
    Cross-platform modular synth and music sequencer, also see this
    [guide](https://sunvox-guide.readthedocs.io/en/latest/index.html)
    ([Github](https://github.com/metrasynth/sunvox-guide))
  - [Synfig](https://www.synfig.org/)ᴸᴹᵂ
    ([Github](https://github.com/synfig/synfig/)):
    2D vector animation
  - [Tooll 3](https://tooll.io/)ᵂ
    ([Github](https://github.com/tooll3/t3)):
    Tool for creating real-time motion graphics
  - [Ultimate Vocal Remover GUI](https://github.com/Anjok07/ultimatevocalremovergui)ᴸᴹᵂ:
    Tool for using open-source deep learning models (e.g.,
    [Demucs](https://github.com/facebookresearch/demucs) and
    [MDX-NET](https://github.com/kuielab/mdx-net)) to isolate
    vocals from audio tracks; a donation-supported web interface
    [MVSEP](https://mvsep.com/) is also available
  - [unflac](https://sr.ht/~ft/unflac/) or
    [Flacon](https://flacon.github.io/)
  - [xACT](http://xact.scottcbrown.org/)ᴹ or
    [XLD](https://tmkk.undo.jp/xld/index_e.html)ᴹ
    ([Sourceforge](https://sourceforge.net/projects/xld/)) or
    [fre:ac](https://www.freac.org/)ᴸᴹᵂ:
    Audio format converter; xACT and XLD are GUI, fre:ac is CLI-only
- Audio and video playback and streaming
  - [Ampache](https://ampache.org/)
    ([Github](https://github.com/ampache/ampache)) or
    [beets](https://beets.io/)
    ([Github](https://github.com/beetbox/beets)) or
    [Gonic](https://github.com/sentriz/gonic) or
    [LMS](https://github.com/epoupon/lms) or
    [Navidrone](https://www.navidrome.org/)
    ([Github](https://github.com/navidrome/navidrome/)):
    Streaming music server; all support the
    [Subsonic](https://www.subsonic.org/) API (beets via
    [Beetstream](https://github.com/BinaryBrain/Beetstream), built-in
    for others); Nextcloud (see Other) also has an optional Music app
    that supports the Ampache and Subsonic API;
    [many](https://github.com/ampache/ampache/wiki/client-api)
    [clients](https://github.com/owncloud/music/wiki/Subsonic)
    [available](https://name.subsonic.org/pages/apps.jsp), like
    [substreamer](https://substreamerapp.com/) (Android, iOS),
    [Amperfy](https://github.com/BLeeEZ/amperfy) (iOS),
    [play:Sub](https://michaelsapps.dk/playsubapp/) (iOS, paid),
    [Ultrasonic](https://gitlab.com/ultrasonic/ultrasonic) (Android),
    [tempo](https://github.com/CappielloAntonio/tempo) (Android),
    [Symfonium](https://symfonium.app/) (Android, paid),
    [Submariner](https://github.com/SubmarinerApp/Submariner) (macOS),
    [Supersonic](https://github.com/dweymouth/supersonic) (desktops),
    etc; for other self-hosted music servers and clients, see
    [here](https://github.com/basings/selfhosted-music-overview)
  - [Jellyfin](https://jellyfin.org/)ᴸᴹᵂ
    ([Github](https://github.com/jellyfin/jellyfin)):
    Media server alternative to [Plex](https://www.plex.tv/) and
    [Emby](https://emby.media/), with clients that span across
    [many](https://jellyfin.org/downloads)
    [platforms](https://github.com/awesome-jellyfin/awesome-jellyfin/)
  - [NDI Tools](https://www.ndi.tv/tools/)ᴹᵂ:
    Software for low-latency broadcasting over LAN by taking any video
    source, like a webcam, video capture card or desktop, and making
    that an NDI source accessible by other computers on the network
  - [OBS Studio](https://obsproject.com/)ᴸᴹᵂ
    ([Github](https://github.com/obsproject/obs-studio)):
    Software for broadcasting video streams with
    [very](https://obsproject.com/forum/resources/downstream-keyer.1254/)
    [many](https://obsproject.com/forum/resources/closed-captioning-via-google-speech-recognition.833/)
    [different](https://sammisolutions.itch.io/sammi)
    [plugins](https://github.com/Xaymar/obs-StreamFX/wiki)
  - [Quod Libet](https://quodlibet.readthedocs.io/)ᴸᴹᵂ
    ([Github](https://github.com/quodlibet/quodlibet)):
    Audio player and library manager
  - [VLC](https://www.videolan.org/vlc/)ᴸᴹᵂᴬᴵ
    ([Git](https://code.videolan.org/videolan/vlc)):
    Media player
- Backup
  - [BorgBackup](https://www.borgbackup.org/)ᴸᴹ
    ([Github](https://github.com/borgbackup/borg)):
    File backup tool; frontends available like
    [Vorta](https://vorta.borgbase.com/)ᴸᴹ, along with other
    [tooling](https://github.com/borgbackup/community);
    an alternative is [bup](https://bup.github.io/)ᴸᴹ
    ([Github](https://github.com/bup/bup))
  - [dar](http://dar.linux.free.fr/)ᴸᴹᵂ
    ([Github](https://github.com/Edrusb/DAR),
    [SourceForge](https://sourceforge.net/projects/dar/)):
    File archiving, supports recovery records using
    [par2](https://github.com/Parchive/par2cmdline/)ᴸᴹᵂ
  - [Timeshift](https://github.com/linuxmint/timeshift)ᴸ:
    Like Windows System Restore but for Linux
- Calendar and task management
  - [Remind](https://dianne.skoll.ca/projects/remind/)ᴸ
    ([Salsa](https://salsa.debian.org/dskoll/remind),
    [Git](https://git.skoll.ca/Skollsoft-Public/Remind)):
    Scriptable calendar and alarm program, with
    [CalDAV](https://github.com/jspricke/remind-caldav) (Google) and
    [iCalendar](https://github.com/jspricke/python-remind) integration
    along with CLI ([link1](https://sr.ht/~mlaparie/remint/),
    [link2](https://gitlab.com/wyrd-calendar/wyrd)) and GUI
    (`tkremind` that comes with Remind) front-ends
  - [Taskwarrior](https://taskwarrior.org/)ᴸ
    ([Github](https://github.com/GothenburgBitFactory/taskwarrior))
    and [Timewarrior](https://timewarrior.net/)ᴸ
    ([Github](https://github.com/GothenburgBitFactory/timewarrior)):
    Todo list manager (Taskwarrior) and time-tracker (Timewarrior);
    [syncall](https://github.com/bergercookie/syncall) can be used to
    sync Taskwarrior tasks with various services like Asana, Google
    Calendar, Google Keep and so on
- Compilers and linkers
  - [Mold](https://github.com/rui314/mold)ᴸ:
    Drop-in replacement for Linux linkers on various architectures;
    for macOS support, there's the previously--commercial now--open
    [sold](https://github.com/bluewhalesystems/sold)ᴸᴹ but which
    probably won't be actively maintained going forward (Github
    [issue](https://github.com/bluewhalesystems/sold/issues/43))
- Containerization
  - [Colima](https://github.com/abiosoft/colima)ᴸᴹ or
    [Rancher Desktop](https://rancherdesktop.io/)ᴸᴹᵂ
    ([Github](https://github.com/rancher-sandbox/rancher-desktop/)):
    Minimal-setup container runtimes on macOS with support for Docker,
    containerd, or Kubernetes; uses [lima](https://lima-vm.io/), which
    is like WSL2 for macOS, under the hood; alternative to Docker
    Desktop; note that the Docker CLI client is needed (see
    [here](https://docs.docker.com/engine/install/binaries/) for
    binaries which can be extracted a `$PATH` dir like `~/.local/bin`)
    to use Colima's and Rancher's Docker runtime; also see
    [link](https://www.cncf.io/blog/2023/02/02/docker-on-macos-is-slow-and-how-to-fix-it/)
  - [dive](https://github.com/wagoodman/dive)ᴸᴹᵂ:
    Tool for exploring Docker image layers
  - [lazydocker](https://github.com/jesseduffield/lazydocker)ᴸᴹᵂ or
    [oxker](https://github.com/mrjackwills/oxker)ᴸᵂ:
    Terminal UI to monitor and control
    [Docker](https://docs.docker.com/) containers
- Data analytics
  - [JASP](https://jasp-stats.org/)ᴸᴹᵂ
    ([Github](https://github.com/jasp-stats/jasp-desktop)):
    GUI statistical analysis tool
  - [Netron](https://netron.app/)ᴸᴹᵂ
    ([Github](https://github.com/lutzroeder/netron)):
    Neural network, deep learning and machine learning model viewer
  - [Paraview](https://www.paraview.org/)ᴸᴹᵂ
    ([Github](https://github.com/Kitware/ParaView)):
    Cross-platform tool for scientific visualization
  - [PSPP](https://www.gnu.org/software/pspp/)ᴸᴹᵂ:
    Open-source SPSS alternative
  - [Veusz](https://veusz.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/veusz/veusz)):
    Scientific plotting and graphing
- Data pipeline
  - [Apache Airflow](https://airflow.apache.org/)ᴿ
    ([Github](https://github.com/apache/airflow)):
    Scheduling workflows
  - [Apache HOP](https://hop.apache.org/)ᴿ
    ([Github](https://github.com/apache/hop)):
    Fork of [Kettle/PDI](https://github.com/pentaho/pentaho-kettle)
  - [Snakemake](https://snakemake.github.io/)ᴿ
    ([Github](https://github.com/snakemake/snakemake)) or
    [Nextflow](https://www.nextflow.io/)ᴿ
    ([Github](https://github.com/nextflow-io/nextflow)):
    Run batch workflows
- Database
  - [BaseX](https://basex.org/)ᴿ:
    XML database and XQuery processor; useful for analyzing or
    processing many or huge XML files
    ([example](https://yobriefca.se/blog/2014/05/17/a-brief-overview-of-basex/))
  - [ClickHouse](https://clickhouse.com/)ᴸᴹ
    ([Github](https://github.com/ClickHouse/ClickHouse)):
    Time-series DB, can also run queries on local or
    remote files without installing a database server using
    [clickhouse-local](https://clickhouse.com/docs/en/operations/utilities/clickhouse-local)
  - [GlareDB](https://glaredb.com/)ᴸᴹᵂ
    ([Github](https://github.com/glaredb/glaredb)):
    Embeddable analytical database that can query multiple sources
    (local files, S3, Snowflake, PostgreSQL, etc), like
    [DuckDB](https://duckdb.org/)ᴸᴹᵂ
    ([Github](https://github.com/duckdb/duckdb)) or
    [chDB](https://doc.chdb.io/)ᴸᴹᵂ
    ([Github](https://github.com/chdb-io/chdb)); also can
    be run on the command-line like OctoSQL; a non-embeddable
    alternative is [ROAPI](https://github.com/roapi/roapi)ᴸᴹᵂ
  - [Litestream](https://litestream.io/)ᴸᴹ
    ([Github](https://github.com/benbjohnson/litestream)) and
    [LiteFS](https://github.com/superfly/litefs)ᴸ:
    Not a DB, but SQLite tools for streaming backup of a SQLite DB to
    cloud storage (Litestream) and replicating SQLite databases across
    a cluster using a FUSE-based filesystem (LiteFS)
  - [MongoDB](https://www.mongodb.com/)ᴸᴹᵂ
    ([Github](https://github.com/mongodb/mongo)):
    NoSQL database
  - [PostgreSQL](https://www.postgresql.org/)ᴸᴹᵂ
    ([Git](https://git.postgresql.org/gitweb/)):
    Relational DB; use the
    [TimescaleDB](https://github.com/timescale/timescaledb)ᴸᴹᵂ
    plugin to better support time-series, or the
    [Citus](https://github.com/citusdata/citus)ᴸ plugin
    for schema-based sharding
  - [rqlite](https://rqlite.io/)ᴸ
    ([Github](https://github.com/rqlite/rqlite)):
    Distributed DB using SQLite as its storage engine
  - [TigerBeetle](https://tigerbeetle.com/)ᴸᴹᵂ
    ([Github](https://github.com/tigerbeetledb/tigerbeetle)):
    Distributed high-throughput fault-tolerant database targeting
    financial accounting use cases; long-term plans are to extract the
    core into a library applicable to any state machine business logic
    (see [here](https://news.ycombinator.com/item?id=32788795) and
    [here](https://news.ycombinator.com/item?id=32787324))
- Database clients
  - [Altair](https://altairgraphql.dev/)ᴸᴹᵂ
    ([Github](https://github.com/altair-graphql/altair)):
    GraphQL client
  - [Another Redis Desktop Manager](https://github.com/qishibo/AnotherRedisDesktopManager)ᴸᴹᵂ:
    Redis GUI client
  - [DBeaver](https://dbeaver.io/)ᴸᴹᵂ
    ([Github](https://github.com/dbeaver/dbeaver)):
    Client for any database that has a jDBC driver; there is a paid
    Pro version available [here](https://dbeaver.com/)
  - [DbGate](https://dbgate.org/)ᴸᴹᵂ
    ([Github](https://github.com/dbgate/dbgate)):
    Client for MySQL, PostgreSQL, SQL Server, MongoDB, Redis, SQLite,
    Amazon Redshift, CockroachDB, MariaDB databases
  - [Jailer](https://github.com/Wisser/Jailer)ᴸᵂᴿ:
    Subset databases and browse relational data
  - [Mathesar](https://mathesar.org/)ᴿ
    ([Github](https://github.com/centerofci/mathesar)):
    PostgreSQL web client with a spreadsheet-like web interface
  - [OctoSQL](https://github.com/cube2222/octosql)ᴸᴹᵂ:
    Make queries from the command-line spanning MySQL, PostgreSQL
    databases along with CSV and JSON; if only local files need be
    supported, there are
    [clickhouse-local](https://clickhouse.com/docs/en/operations/utilities/clickhouse-local)ᴸᴹᵂ
    or [q](https://harelba.github.io/q/)ᴸᴹᵂᴿ
    ([Github](https://github.com/harelba/q)) or
    [columnq](https://github.com/roapi/roapi/tree/main/columnq-cli)ᴸᴹᵂ,
    or use [sqlite](https://www.sqlite.org/)ᴸᴹᵂ
    ([howto](https://til.simonwillison.net/sqlite/one-line-csv-operations))
  - [pgcli](https://www.pgcli.com/)ᴿ
    ([Github](https://github.com/dbcli/pgcli)):
    PostgreSQL command-line client; the developer org has CLIs for
    other databases ([link](https://www.dbcli.com/)) like
    LiteCLI(https://litecli.com/)ᴿ
    ([Github](https://github.com/dbcli/litecli)) for SQLite or
    MyCLI(https://www.mycli.net/)ᴿ
    ([Github](https://github.com/dbcli/mycli)) for MySQL or
    IRedis(https://iredis.xbin.io/)ᴿ
    ([Github](https://github.com/laixintao/iredis)) for Redis
  - [SchemaCrawler](https://www.schemacrawler.com/)ᴿ
    ([Github](https://github.com/schemacrawler/SchemaCrawler)):
    Database schema discovery and comprehension tool; supports schema
    [search](https://www.schemacrawler.com/schemacrawler-grep.html),
    [diagramming](https://www.schemacrawler.com/diagramming.html) and
    [scripting](https://www.schemacrawler.com/scripting.html) on any
    database with a JDBC driver
  - [Sequel Ace](https://github.com/Sequel-Ace/Sequel-Ace)ᴹ:
    MySQL, MariaDB client; macOS
- Desktop publishing
  - [Scribus](https://www.scribus.net/)ᴸᴹᵂ
    ([Github mirror](https://github.com/scribusproject/scribus)):
    Alternative to commercial Adobe InDesign; good for book layouts
- Diagramming and image editing
  - [ASCIIFlow](https://asciiflow.com/)ᴿ
    ([Github](https://github.com/lewish/asciiflow)):
    ASCII drawing application, web-based but can be run locally with
    [Bazel](https://bazel.build/) (install the Bazel launcher
    [Bazelisk](https://github.com/bazelbuild/bazelisk) to pick the
    right Bazel version; for live reloading, install and use
    [ibazel](https://github.com/bazelbuild/bazel-watcher));
    commercial Monodraw is more full-featured; alternatives are
    [MonoSketch](https://monosketch.io/)ᴿ
    ([Github](https://github.com/tuanchauict/MonoSketch)) or
    [Asciio](https://nkh.github.io/P5-App-Asciio/)
    ([Github](https://github.com/nkh/P5-App-Asciio))
  - [ascii_tree](https://github.com/yzhong52/ascii_tree)ᴸ:
    Renders Markdown headings as ASCII trees
  - [Caire](https://github.com/esimov/caire)ᴸᴹᵂ:
    Smart image resizing
  - [Diagon](https://github.com/ArthurSonzogni/Diagon)ᴸᴹ:
    Command-line tool for transforming Markdown-style expressions into
    ASCII art, [webapp](https://arthursonzogni.com/Diagon/) and
    [snap](https://snapcraft.io/diagon) available
  - [Diagrams](https://diagrams.mingrammer.com/)ᴿ
    ([Github](https://github.com/mingrammer/diagrams)):
    Create cloud system architecture diagrams using Python code
  - [goat](https://github.com/blampe/goat)ᴿ:
    Command-line utility to convert ASCII diagrams to PNG or SVG;
    an alternative is the webapp
    [svgbob](http://ivanceras.github.io/svgbob-editor/)ᴿ
    ([Github](https://github.com/ivanceras/svgbob))
  - [drawio-desktop](https://github.com/jgraph/drawio-desktop)ᴸᴹᵂ or
    [yEd](https://www.yworks.com/products/yed)ᴸᴹᵂ:
    Vector diagramming; drawio-desktop is the Electron build of
    [draw.io](https://www.drawio.com/); yEd is free but not
    open-source; an alternative is
    [Excalidraw](https://excalidraw.com/)ᴿ
    ([Github](https://github.com/excalidraw/excalidraw))
  - [Freeplane](https://docs.freeplane.org/)ᴸᴹᵂᴿ
    ([Github](https://github.com/freeplane/freeplane)):
    Java GUI mind mapping application
  - [GIMP](https://www.gimp.org/)ᴸᴹᵂ
    ([GNOME Gitlab](https://gitlab.gnome.org/GNOME/gimp)) or
    [Krita](https://krita.org/en/)ᴸᴹᵂ
    ([KDE Invent](https://invent.kde.org/graphics/krita)):
    Raster graphics editor
  - [Inkscape](https://inkscape.org/)ᴸᴹᵂ
    ([Gitlab](https://gitlab.com/inkscape/inkscape)):
    Vector graphics editor
  - [libvips](https://www.libvips.org/)ᴸᴹᵂ
    ([Github](https://github.com/libvips/libvips)):
    Image processing library with a command-line tool,
    like [ImageMagick](https://imagemagick.org/)ᴸᴹᵂ;
    has a speadsheet-like GUI front-end
    [nip2](https://github.com/libvips/nip2))ᴸᴹᵂ;
    [article](https://tonisagrista.com/blog/2023/libvips/)
  - [Markmap](https://markmap.js.org/)
    ([Github](https://github.com/markmap/markmap)):
    Visualize Markdown as a mind-map, available on the
    [CLI](https://markmap.js.org/docs/packages--markmap-cli)ᴿ,
    [Webapp](https://markmap.js.org/repl) or VSCode
    [plugin](https://marketplace.visualstudio.com/items?itemName=gera2ld.markmap-vscode)
  - [Mermaid](https://mermaid.js.org/)
    ([Github](https://github.com/mermaid-js/mermaid),
    [CLI](https://github.com/mermaid-js/mermaid-cli)ᴿ) or
    [Pikchr](https://pikchr.org/)ᴸᴹᵂ
    ([Fossil](https://pikchr.org/home/dir?ci=trunk)) or
    [PlantUML](https://plantuml.com/)ᴿ
    ([Github](https://github.com/plantuml/plantuml)) or
    [LikeC4](https://likec4.dev/)
    ([Github](https://github.com/likec4/likec4)) or
    [D2](https://d2lang.com/)ᴸᴹᵂ
    ([Github](https://github.com/terrastruct/d2),
    [Webapp](https://play.d2lang.com/)):
    Markup language for diagramming, standalone or supported by
    Markdown processers like Pandoc via plugins, also see
    [here](https://xosh.org/text-to-diagram/)
  - [Moebius](https://blocktronics.github.io/moebius/)ᴸᴹᵂᴿ
    ([Github](https://github.com/blocktronics/moebius)) or
    [Pablodraw](https://picoe.ca/products/pablodraw/)ᴸᴹᵂ
    ([Github](https://github.com/cwensley/pablodraw)):
    Cross-platform ANSI and ASCII art editor
  - [Gaphor](https://gaphor.org/)ᴸᴹᵂᴿ
    ([Github](https://github.com/gaphor/gaphor)) or
    [UMLet](https://www.umlet.com/)ᴿ
    ([Github](https://github.com/umlet/umlet),
    [Webapp](https://www.umletino.com/), VSCode
    [plugin](https://marketplace.visualstudio.com/items?itemName=TheUMLetTeam.umlet)):
    GUI tool for drawing UML diagrams
  - [Treesheets](https://strlen.com/treesheets/)ᴸᴹᵂ
    ([Github](https://github.com/aardappel/treesheets)):
    "Hierarchical spreadsheet" that can be used for mind-maps
- Digital design
  - [Penpot](https://penpot.app/)ᴸ
    ([Github](https://github.com/penpot/penpot)):
    Open source design and prototyping platform like
    [Figma](https://www.figma.com/)
- Ebook
  - [Alexandria](https://github.com/btpf/Alexandria)ᴸᴹᵂ or
    [Foliate](https://johnfactotum.github.io/foliate/)ᴸ
    ([Github](https://github.com/johnfactotum/foliate)) or
    [Thorium Reader](https://thorium.edrlab.org/)ᴸᴹᵂ
    ([Github](https://github.com/edrlab/thorium-reader)):
    EPUB3-compliant GUI ebook reader; Alexandria (cross-platform) and
    Foliate (Linux-only) are more performant; Thorium (cross-platform)
    is the reference reader for the format and so the most compliant
  - [Calibre](https://calibre-ebook.com/)ᴸᴹᵂ
    ([Github](https://github.com/kovidgoyal/calibre)):
    Ebook manager supporting an array of formats, with many plugins
  - [epr](https://github.com/wustho/epr)ᴿ or
    [epy](https://github.com/wustho/epy)ᴿ:
    TUI ebook reader; epr supports only EPUB; epy is a fork of epy
    with support for more formats like MOBI and AZW3, and adds
    features like bookmarks, integration with external dictionaries,
    and inline formatting
  - [epub2txt2](https://github.com/kevinboone/epub2txt2)ᴸ:
    Extract text from EPUB
  - [MuPDF](https://mupdf.com/)ᴸᴹᵂ
    ([Git](https://git.ghostscript.com/?p=mupdf.git;a=summary),
    [Github](https://github.com/ArtifexSoftware/mupdf),
    [Documentation](https://mupdf.readthedocs.io/en/latest/)):
    See _PDF reader or transformer_
  - [Sigil](https://sigil-ebook.com/)ᴸᴹᵂ
    ([Github](https://github.com/Sigil-Ebook/Sigil)):
    Cross-platform ebook editor supporting EPUB
- Email
  - [aerc](https://git.sr.ht/~rjarry/aerc)ᴸ:
    TUI email client with [notmuch](https://notmuchmail.org/)ᴸ support.
    See the appropriate section below for how to set up Notmuch,
    [Lieer](https://github.com/gauteh/lieer)ᴿ and aerc for Gmail usage.
  - [Apple Mail](https://support.apple.com/mail)ᴹ or
    [Evolution](https://gitlab.gnome.org/GNOME/evolution/-/wikis/home)ᴸ or
    [Thunderbird](https://www.thunderbird.net/)ᴸᴹᵂ:
    Thunderbird is a cross-platform graphical email client and has a
    soft fork [Betterbird](https://www.betterbird.eu/)ᴸᴹᵂ
    ([Github](https://github.com/Betterbird/)) that applies patches
    atop the ESR version; Microsoft Exchange and Office 365 is
    supported in Apple Mail (macOS), in Evolution (Linux) via the EWS
    [module](https://gitlab.gnome.org/GNOME/evolution/-/wikis/Building#3rd-party-modules)
    (`evolution-ews` Debian pkg), and in Thunderbird with paid plugins
    ([OWA](https://addons.thunderbird.net/en-US/thunderbird/addon/owl-for-exchange/),
    [EWS](https://addons.thunderbird.net/en-US/thunderbird/addon/exquilla-exchange-web-services/)
- File management
  - [7-Zip](https://www.7-zip.org/)ᴸᴹᵂ
    ([SourceForge](https://sourceforge.net/projects/sevenzip/)):
    Command-line file archiver that supports many formats
  - [Cryptomator](https://cryptomator.org/)ᴸᴹᵂ
    ([Github](https://github.com/cryptomator)):
    Provides client-side encryption of local or cloud data (relevant
    sync software needs to be installed on the machine), surfacing it
    as a virtual drive; cross-platform, including iOS and Android;
    compatible with first-party cloud storage provider software and
    its vaults are interoperable with Mountain Duck vaults; not
    compatible with `rclone mount`
  - [Cyberduck](https://cyberduck.io/)ᴹᵂ
    ([Github](https://github.com/iterate-ch/cyberduck)):
    Cross-platform remote FTP, SFTP, WebDAV, cloud storage GUI tool;
    has a CLI version [`duck`](https://duck.sh/)ᴸᴹᵂ
  - [Double Commander](https://doublecmd.sourceforge.io/)ᴸᴹᵂ
    ([Github](https://github.com/doublecmd/doublecmd)):
    GUI Midnight Commander clone
  - [FUSE](https://en.wikipedia.org/wiki/Filesystem_in_Userspace)ᴸ:
    Lets users mount filesystems without superuser privileges; usually
    installed as a dependency by Linux package managers, for example,
    `apt install rclone` on Debian will also pull in `fuse` or `fuse3`
    as a suggested package; on macOS, install
    [FUSE-T](https://www.fuse-t.org/)ᴹ
    ([Github](https://github.com/macos-fuse-t/fuse-t), kext-less) or
    [macFUSE](https://osxfuse.github.io/)ᴹ
    ([Github](https://github.com/osxfuse/osxfuse), uses a kext), with
    the latter being more mature but requiring third-party kernel
    extensions be enabled
  - [Immich](https://immich.app/)ᴸ:
    Self-hosted photo library (like Google Photos), server can be run
    [dockerized](https://immich.app/docs/install/docker-compose), with
    web (access at `http://<machine-ip-address>:2283`) and
    [mobile](https://immich.app/docs/features/mobile-app/)ᴬᴵ clients
  - [Keka](https://www.keka.io/en/)ᴹᴵ
    ([Github](https://github.com/aonez/Keka)) or
    [PeaZip](https://peazip.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/peazip/PeaZip/)):
    GUI file archiver; Keka is donationware (paid App Store version)
  - [lftp](https://lftp.yar.ru/)ᴸ
    ([Github](https://github.com/lavv17/lftp)):
    Command-line file transfer program supporting multiple protocols;
    if only FTP is needed, there is [ncftp](https://www.ncftp.com/)ᴸᴹᵂ
  - [Maestral](https://maestral.app/)ᴸᴹ
    ([Github](https://github.com/samschott/maestral)):
    Dropbox clientfor Linux and macOS, lighter but has less features
    than the [official](https://www.dropbox.com/desktop) client
  - [Magic Wormhole](https://github.com/magic-wormhole/magic-wormhole)ᴿ:
    Command-line tool and library for sending files from one computer
    to another; note that this tool requires
    [mailbox](https://github.com/magic-wormhole/magic-wormhole-mailbox-server)ᴿ
    and a
    [transit relay](https://github.com/magic-wormhole/magic-wormhole-transit-relay)ᴿ
    server, which by default uses one hosted by the project
  - [Midnight Commander](https://midnight-commander.org/)ᴸᴹ
    ([Github](https://github.com/MidnightCommander/mc)) or
    [nnn](https://github.com/jarun/nnn)ᴸᴹ or
    [lf](https://github.com/gokcehan/lf)ᴸᴹᵂ or
    [broot](https://github.com/Canop/broot)ᴸᴹᵂ:
    Terminal file manager
  - [rclone](https://rclone.org/)ᴸᴹᵂ
    ([Github](https://github.com/rclone/rclone)):
    Like `rsync` but for cloud storage; `rclone mount` can be used to
    mount cloud storage to a filesystem mount point (requires FUSE);
    an SSH-only mount alternative which also requires FUSE is
    [SSHFS](https://github.com/libfuse/sshfs)ᴸ; another more complex
    setup for romote filesystem mounting is using NFSv4 and Wireguard
    ([example](https://sigmdel.ca/michel/ha/wireguard/network_share_over_wireguard_en.html))
  - [rdfind](https://github.com/pauldreik/rdfind)ᴸᴹ or
    [fdupes](https://github.com/adrianlopezroche/fdupes)ᴸᴹ:
    Command-line tool to find duplicate files
  - [renameutils](https://www.nongnu.org/renameutils/)ᴸ or
    [f2](https://github.com/ayoisaiah/f2)ᴸᴹᵂᴬ or
    [KRename](https://userbase.kde.org/KRename)ᴸ or
    [GPRename](http://gprename.sourceforge.net/)ᴸ or
    [Szyszka](https://github.com/qarmin/szyszka)ᴸᴹᵂ:
    Rename lots of files easily
  - [Spacedrive](https://spacedrive.com/)ᴸᴹᵂ
    ([Github](https://github.com/spacedriveapp/spacedrive)):
    Cross-platform cross-device/service file manager
  - [Stow](https://www.gnu.org/software/stow/)ᴸ:
    Symlink farm manager, useful for managing dotfiles
  - [Syncthing](https://syncthing.net/)ᴸᴹᵂ
    ([Github](https://github.com/syncthing/syncthing)):
    Continous file synchronization; has a macOS
    [front-end](https://github.com/syncthing/syncthing-macos)ᴹ,
    and there is an implementation (by the developers) for
    [Android](https://github.com/syncthing/syncthing-android)ᴬ
    ([Google Play](https://play.google.com/store/apps/details?id=com.nutomic.syncthingandroid))
    and another for iOS (paid, not by the developers) called
    [Möbius](https://apps.apple.com/us/app/m%C3%B6bius-sync-pro/id1671184333)ᴵ
- Image text extraction
  - [Apache Tika](http://tika.apache.org/)ᴿ
    ([Github](https://github.com/apache/tika/)):
    [Go](https://github.com/google/go-tika) and
    [Python](https://github.com/chrismattmann/tika-python) bindings
    available; also extracts text from many other file types
  - [Tesseract](https://tesseract-ocr.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/tesseract-ocr/tesseract))
- Markup language document conversion and rendering
  - [Asciidoctor](https://asciidoctor.org/)ᴿ
    ([Github](https://github.com/asciidoctor/asciidoctor)):
    Asciidoc processor, has an
    [extension](https://asciidoctor.org/docs/asciidoctor-pdf/)ᴿ
    for PDF generation; Javascript
    [port](https://github.com/asciidoctor/asciidoctor.js) and
    [CLI](https://github.com/asciidoctor/asciidoctor-cli.js) available
  - [Docutils](https://www.docutils.org/)ᴿ
    ([Github](https://github.com/docutils/docutils)):
    Convert ReStructuredText to other formats
  - [groff](https://www.gnu.org/software/groff/)ᴸ:
    Typesetting system that can output PS, PDF, HTML or DVI, developed
    to replace Unix `troff` and `nroff`; compared to (La)Tex, it is
    lighter-weight but less popular; alternatives are
    [Heirloom Doctools](https://n-t-roff.github.io/heirloom/doctools.html)ᴸ
    ([Github](https://github.com/n-t-roff/heirloom-doctools), and
    manpage-only formatter [mandoc](https://mandoc.bsd.lv/)ᴸ
  - [hred](https://github.com/danburzo/hred)ᴿ:
    Command-line HTML (and XML) to JSON converter
  - [Pandoc](https://pandoc.org/)ᴸᴹᵂ
    ([Github](https://github.com/jgm/pandoc)):
    Document converter, can also process Markdown extendable with
    filters like [this one](https://github.com/pandoc-ext/diagram)
    facilitating diagramming using code blocks
  - [Quarto](https://quarto.org/)ᴸᴹᵂ
    ([Github](https://github.com/quarto-dev/quarto-cli)):
    Publishing system built on Pandoc, successor to R Markdown,
    supporting Markdown and Jupyter notebooks with dynamic content
    using Python, R, Julia, and Observable JS; for a user-local
    install, one option is to extract the tarball version somewhere
    and symlink the `quarto` binary from a directory in `$PATH`
  - [Sphinx](https://www.sphinx-doc.org/)ᴿ
    ([Github](https://github.com/sphinx-doc/sphinx)):
    Generate project docs from source code and ReStructuredText (or
    [Markdown](https://myst-parser.readthedocs.io/en/latest/)),
    supports multiple programming langs (domains), built on Docutils
  - [Tectonic](https://tectonic-typesetting.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/tectonic-typesetting/tectonic)) or
    [TeXLive](https://www.tug.org/texlive/)ᴸᴹᵂ or
    [TinyTeX](https://github.com/rstudio/tinytex)
    ([binaries](https://github.com/rstudio/tinytex-releases)ᴸᴹᵂ):
    Processor for (La)TeX, which is used widely in scientific writing
  - [TeXmacs](https://texmacs.org/)ᴸᴹᵂ
    ([Github](https://github.com/texmacs/texmacs)):
    WYSIWYG scientific writing software, that is an alternative
    to LaTeX but can import/export LaTeX files; there is an a
    distribution of TeXmacs called [Mogan](https://mogan.app/)
    ([Github](https://github.com/XmacsLabs/mogan)) that aims to be
    more user-friendly
  - [Typst](https://typst.app/)ᴸᴹᵂ
    ([Github](https://github.com/typst/typst)):
    Markup-based typesetting system, that is an alternative to LaTeX
  - [WeasyPrint](https://github.com/Kozea/WeasyPrint)ᴿ or
    [percollate](https://github.com/danburzo/percollate)ᴿ:
    Convert HTML to PDF files; percollate processes the page for
    readability before conversion; the file can also be directly
    opened in a browser and printed to PDF
- Newsreader
  - [NetNewsWire](https://netnewswire.com/)ᴹ
    ([Github](https://github.com/Ranchero-Software/NetNewsWire)):
    RSS reader; iOS and macOS
  - [Newsboat](https://newsboat.org/)ᴸ
    ([Github](https://github.com/newsboat/newsboat)):
    CLI RSS reader
  - [Pan](http://pan.rebelbase.com/)ᴸᵂ
    ([GNOME Gitlab](https://gitlab.gnome.org/GNOME/pan/)):
    Usenet reader
- Networking
  - [LuLu](https://objective-see.org/products/lulu.html)ᴹ
    ([Github](https://github.com/objective-see/LuLu)):
    Firewall; while the built-in firewall for macOS only filters
    incoming connections, this can also filter outgoing connections
  - [Netiquette](https://objective-see.org/products/netiquette.html)ᴹ
    ([Github](https://github.com/objective-see/Netiquette)):
    Network monitor
- Package/runtime manager
  - [asdf](https://asdf-vm.com/)ᴿ
    ([Github](https://github.com/asdf-vm/asdf)):
    Language runtime manager with plugins
    [plugins](https://github.com/asdf-vm/asdf-plugins) for many
    language runtimes and tools (like `bat`, `jq`, `ripgrep`, etc)
  - [Flatpak](https://flatpak.org/)ᴸ:
    Linux application manager; [Flathub](https://flathub.org/) is the
    de facto repository; for easier permissioning control install
    [Flathub](https://flathub.org/apps/details/com.github.tchx84.Flatseal)ᴸ
  - [MacPorts](https://www.macports.org/)ᴹ
    ([Github org](https://github.com/macports)) or
    [Spack](https://spack.io/)ᴸᴹ
    ([Github](https://github.com/spack/spack)) or
    [Homebrew](https://brew.sh/)ᴸᴹ
    ([Github](https://github.com/Homebrew/brew)):
    macOS package manager for open-source software; recommendation is
    to use Spack for a user-local install, MacPorts for a multi-user
    machine, and Homebrew for a single-user machine
  - [Mamba](https://mamba.readthedocs.io/)ᴸᴹᵂ
    ([Github](https://github.com/mamba-org),
    [Binaries](https://github.com/conda-forge/miniforge)):
    Drop-in replacement for [conda](https://conda.io/), with
    [micromamba](https://github.com/mamba-org/micromamba-releases)ᴸᴹᵂ
    the preferred variant; using [conda-forge](https://conda-forge.org/)
    as the primary repository is recommended
  - [Rhumba](https://github.com/mamba-org/rhumba):
    R package manager installable via conda or mamba
- PDF reader or transformer
  - [diffpdf](https://tracker.debian.org/pkg/diffpdf)ᴸ
    ([source](http://www.qtrac.eu/diffpdf-foss.html)):
    Diff two PDF files, Can be used with `git` by setting
    `git config --global difftool.diffpdf.cmd 'diffpdf "$LOCAL" "$REMOTE"'`
    and `git config --global alias.diffpdf "difftool -t diffpdf"` and
    running e.g. `git diffpdf somecommit:somefile.pdf somefile.pdf`;
    Linux
  - [DjVu.js](https://djvu.js.org/)ᴿ
    ([Github](https://github.com/RussCoder/djvujs)):
    View DjVu documents in the browser; for local viewer usage
    instructions, see [here](https://djvu.js.org/downloads)
  - [MuPDF](https://mupdf.com/)ᴸᴹᵂ
    ([Git](https://git.ghostscript.com/?p=mupdf.git;a=summary),
    [Github](https://github.com/ArtifexSoftware/mupdf),
    [Documentation](https://mupdf.readthedocs.io/en/latest/)):
    PDF, XPS and EPUB reader; AGPL version is available in many
    package managers like APT (Linux), Homebrew or MacPorts (macOS)
  - [QPDF](https://qpdf.sourceforge.io/)ᴸᵂ
    ([Github](https://github.com/qpdf/qpdf)) or
    [pdfcpu](https://pdfcpu.io/)ᴸᴹᵂ
    ([Github](https://github.com/pdfcpu/pdfcpu)) or
    [MuTool](https://mupdf.com/docs/mutool.html)ᴸᴹᵂ
    (part of [MuPDF](https://mupdf.com/), may be packaged separately
    like in Debian where it is in the `mupdf-tools` package):
    PDF transformations and processing
  - [sioyek](https://sioyek.info/)ᴸᴹᵂ
    ([Github](https://github.com/ahrm/sioyek)) or
    [Zathura](https://pwmt.org/projects/zathura/)ᴸ
    ([Github](https://github.com/pwmt/zathura)):
    PDF reader for research and technical PDFs
  - [Skim](https://skim-app.sourceforge.io/)ᴹ:
    PDF reader and annotator; the built-in Apple macOS
    [Preview](https://support.apple.com/guide/preview/welcome/mac) is
    usually good enough
  - [Xournal++](https://xournalpp.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/xournalpp/xournalpp)):
    Handwriting notebook with PDF annotation support
- Presentation
  - [Marp](https://marp.app/)
    ([Github](https://github.com/marp-team/marp),
    [CLI app](https://github.com/marp-team/marp-cli)ᴿ) or
    [Slidev](https://sli.dev/)ᴿ
    ([Github](https://github.com/slidevjs/slidev)):
    Markdown-based presentations on the browser;
    for presentations on the terminal, there are also
    [slides](https://maaslalani.com/slides/)
    ([Github](https://github.com/maaslalani/slides)) and
    [presenterm](https://mfontanini.github.io/presenterm/)
    ([Github](https://github.com/mfontanini/presenterm));
    there is also a simpler text-but-not-Markdown-based presenter
    [sent](https://tools.suckless.org/sent/)ᴸ
  - [pdfpc](https://pdfpc.github.io/)ᴸ
    ([Github](https://github.com/pdfpc/pdfpc)):
    Multi-monitor PDF presentations
- Project (codebase)
  - [CodeQuery](https://ruben2020.github.io/codequery/)ᴸᵂ
    ([Github](https://github.com/ruben2020/codequery)):
    Code-browsing and -understanding tool, building on
    [cscope](http://cscope.sourceforge.net/)ᴸ and
    [ctags](http://ctags.sourceforge.net/)ᴸ, providing a GUI interface
    to these tools and visualization capabilities; supports C, C++,
    Go, Java, Javascript, Python and Ruby; complementary tools include
    [cflow](https://www.gnu.org/software/cflow/)ᴸ to analyze C code or
    [Doxygen](https://www.doxygen.nl/)ᴸᴹᵂ to extract code structure
  - [Cookiecutter](https://github.com/cookiecutter/cookiecutter)ᴿ
    Project templates
  - [Dependency Management Data](https://dmd.tanna.dev/)ᴸᴹᵂ
    ([Gitlab](https://gitlab.com/tanna.dev/dependency-management-data)):
    Tooling for tracking dependencies across an organization and
    making them queryable, supports importing data from several
    sources ([link](https://dmd.tanna.dev/related/)) including AWS,
    [Renovate](https://docs.renovatebot.com/), OpenSSF
    [scorecard](https://securityscorecards.dev/)
    ([link](https://dmd.tanna.dev/cookbooks/import-scorecards/)),
    [Dependabot](https://docs.github.com/en/code-security/dependabot),
    [OSS Review Toolkit](https://oss-review-toolkit.org/ort/)
    ([link](https://dmd.tanna.dev/cookbooks/getting-started-ort/)),
    etc
  - [DevSkim](https://github.com/microsoft/DevSkim)ᴸᴹᵂ or
    [Semgrep](https://github.com/returntocorp/semgrep)ᴿ:
    Code security linter for multiple languages, with a command-line
    interface; ones for specific languages or tools also exist (e.g.,
    [C++](https://github.com/david-a-wheeler/flawfinder)ᴿ,
    [Go](https://github.com/securego/gosec)ᴸᴹᵂ,
    [Kubernetes](https://github.com/controlplaneio/kubesec)ᴸᴹᵂ,
    [Ruby on Rails](https://github.com/presidentbeef/brakeman/)ᴿ,
    [Terraform](https://github.com/aquasecurity/tfsec)ᴸᴹᵂ); there are
    also [other](https://github.com/Microsoft/ApplicationInspector)ᴸᴹᵂ
    [tools](https://github.com/googleprojectzero/weggli)ᴸᴹᵂ that analyze
    what code does instead of looking for specific code smells; also
    see [these](https://jreyesr.github.io/posts/semgrep-blog-rules/)
    [articles](https://parsiya.net/blog/2022-04-07-code-review-hot-spots-with-semgrep/)
  - [EditorConfig](https://editorconfig.org/)
    ([Github](https://github.com/editorconfig/)):
    Define formatting conventions for project code and
    text files, with multiple implementations and editor
    plugins to apply them (install base command with `apt
    install editorconfig` on Debian or Ubuntu, `port install
    editorconfig-core-c` via MacPorts on macOS, via a Python
    [package](https://github.com/editorconfig/editorconfig-core-py)ᴿ,
    or download a
    [binary](https://github.com/editorconfig/editorconfig-core-go)ᴸᴹᵂ);
    [editorconfig-checker](https://editorconfig-checker.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/editorconfig-checker/editorconfig-checker))
    can be used to check that files conform to EditorConfig settings
  - [Gittyup](https://murmele.github.io/Gittyup/)ᴸᴹᵂ
    ([Github](https://github.com/Murmele/Gittyup)) or
    [Gitup](https://github.com/git-up/GitUp)ᴹ or
    [TortoiseGit](https://tortoisegit.org/)ᵂ:
    [Git](https://git-scm.com/) GUI client
  - [gogs](https://gogs.io/)ᴸᴹᵂ or
    [Soft Serve](https://github.com/charmbracelet/soft-serve)ᴸᴹᵂ:
    Self-hosted Git server
  - [kondo](https://github.com/tbillington/kondo)ᴸᴹᵂ:
    Clean unneeded files like build artifacts from project directories
  - [onefetch](https://github.com/o2sh/onefetch)ᴸᴹᵂ:
    Git repository information and statistics
  - [OSS Review Toolkit](https://oss-review-toolkit.org/ort/)ᴿ
    ([Github](https://github.com/oss-review-toolkit/ort)):
    Suite of tools for analyzing project dependencies and checking
    software license compliance
  - [ruplacer](https://github.com/your-tools/ruplacer)ᴸᴹᵂ:
    Find and replace recursively, like a combo of `find` and `sed -i`
  - [scc](https://github.com/boyter/scc)ᴸᴹᵂ or
    [tokei](https://github.com/XAMPPRocky/tokei)ᴸᴹᵂ:
    Count lines of code by language
  - [scorecard](https://scorecard.dev/)ᴸᴹᵂ
    ([Github](https://github.com/ossf/scorecard)):
    Assess operational risk (code, build, dependencies, testing,
    maintenance, etc) of open-source software; can be run on public
    repositories owned by others; results for many open source
    projects are also published weekly as a BigQuery dataset (see
    [here](https://github.com/ossf/scorecard?tab=readme-ov-file#public-data))
  - [task](https://taskfile.dev/)ᴸᴹᵂ
    ([Github](https://github.com/go-task/task)):
    Easier-to-use [Make](https://www.gnu.org/software/make/)
    alternative
- Remote login and desktop
  - [cpu](https://github.com/u-root/cpu)ᴸ:
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
  - [Mosh](https://mosh.org/)ᴸᴹ
    ([Github](https://github.com/mobile-shell/mosh)):
    Robust SSH alternative; `dumb` terminals don't work well with mosh
    because it always outputs escape sequences to set the window title
  - [Remmina](https://remmina.org/)ᴸ
    ([Gitlab](https://gitlab.com/Remmina/Remmina)):
    Remote desktop client for POSIX systems; on systems that can
    run [Chrome](https://www.google.com/chrome/), there is also
    [Chrome Remote Desktop](https://remotedesktop.google.com/)
    ([extension](https://chromewebstore.google.com/detail/chrome-remote-desktop/inomeogfingihgjfjlpeplalcfajhgai))
- Search
  - [Code Search](https://github.com/google/codesearch)ᴸᴹᵂ or
    [hound](https://github.com/hound-search/hound)ᴸᴹᵂ or
    [zoekt](https://github.com/sourcegraph/zoekt)ᴸᴹᵂ:
    Text search engine for source code; codesearch and hound are based
    on [this paper](https://swtch.com/~rsc/regexp/regexp4.html); zoekt
    is based on a simplified version of a technique described
    [here](https://link.springer.com/article/10.1007/s11390-016-1618-6)
    ([design](https://github.com/sourcegraph/zoekt/blob/main/doc/design.md),
    [HN comment](https://news.ycombinator.com/item?id=38370711)).
    Code Search and zoekt have CLI interfaces, while hound and zoekt
    (using zoekt-webserver) have web interfaces; hound and zoekt are
    more featureful, e.g., works with various Git hosts; see the
    **Working with large codebases** section below for a good use
    case; Code Search is archived now (although it still compiles and
    works), but there are maintained forks with added features
    ([one](https://github.com/junkblocker/codesearch),
    [two](https://github.com/hakonhall/codesearch),
    [three](https://github.com/tylerwilliams/codesearch))
  - [Meilisearch](https://github.com/meilisearch/meilisearch)ᴸᴹᵂ or
    [ElasticSearch](https://github.com/elastic/elasticsearch)ᴸᴹᵂ or
    [Toshi](https://github.com/toshi-search/Toshi)ᴸᴹᵂ:
    Search engine server; ElasticSearch and Toshi are designed for
    general-purpose search, while Meilisearch focuses on simpler
    searches (algorithm is inverse-index to Levenshtein automaton
    for handling typos to bucket sort for ranking documents)
  - [fd](https://github.com/sharkdp/fd)ᴸᴹᵂ:
    Simpler command-line alternative to `find`
  - [fzf](https://github.com/junegunn/fzf)ᴸᴹᵂ or
    [zf](https://github.com/natecraddock/zf)ᴸᴹᵂ:
    Command-line fuzzy finder; zf has better filename matching
    ([link](https://nathancraddock.com/blog/2023/a-different-approach-to-fuzzy-finding/))
  - [pdfgrep](https://pdfgrep.org/)ᴸ
    ([Gitlab](https://gitlab.com/pdfgrep/pdfgrep)):
    Command-line tool for searching text in PDF files
  - [ripgrep](https://github.com/BurntSushi/ripgrep)ᴸᴹᵂ:
    `grep` alternative, extend to more file formats with
    [ripgrep-all](https://github.com/phiresky/ripgrep-all)
- Shell enhancement
  - [tldr](https://github.com/tldr-pages/tldr) or
    [navi](https://github.com/denisidoro/navi)ᴸᴹᵂ:
    Interactive command references on the command-line; tldr has
    alternative client implementations
    [tealdeer](https://github.com/dbrgn/tealdeer)ᴸᴹᵂ or
    [outfieldr](https://gitlab.com/ve-nt/outfieldr)ᴸᴹᵂ that run much
    faster than the usual Node.js and Python clients
  - [z](https://github.com/rupa/z)ᴿ or
    [zoxide](https://github.com/ajeetdsouza/zoxide)ᴸᴹᵂ:
    Frecent directories; `z` is a plugin for Bash and Zsh, whereas
    `zoxide` is more of a replacement for the `cd` command
- System administration
  - [angle-grinder](https://github.com/rcoh/angle-grinder)ᴸᴹ
    (log analyzer)
  - [atop](https://github.com/Atoptool/atop)ᴸ:
    System resource monitor, runs as a daemon that logs process
    activity to disk
  - [bandwhich](https://github.com/imsnif/bandwhich)ᴸᴹᵂᴬ:
    Network utilization by process, connection, remote IP, hostname,
    and so on
  - [BleachBit](https://www.bleachbit.org/)ᴸᵂ
    ([Github](https://github.com/bleachbit/bleachbit)) or
    [czkawka](https://github.com/qarmin/czkawka)ᴸᴹᵂ:
    Clean up unnecessary and temporary files from the system
  - [dua](https://github.com/Byron/dua-cli)ᴸᴹᵂ or
    [dust](https://github.com/bootandy/dust)ᴸᴹᵂ:
    Alternative to `du` for checking disk usage. dua also makes
    deletion of unwanted data easy
  - [duf](https://github.com/muesli/duf)ᴸᴹᵂ or
    [dysk](https://github.com/Canop/dysk)ᴸ:
    Command-line alternative to `df` with nicer user interface
  - [eBPF/bcc](https://github.com/iovisor/bcc)ᴸ or
    [strace](https://strace.io/)ᴸ
    ([Github](https://github.com/strace/strace),
    [Gitlab](https://gitlab.com/strace/strace)):
    Process debugging;
    eBPF/bcc [tutorial](https://ish-ar.io/python-ebpf-tracing/)
  - [fkill](https://github.com/sindresorhus/fkill-cli)ᴿ:
    Command-line tool to interactively kill running user and system
    procs
  - [forkstat](https://github.com/ColinIanKing/forkstat)ᴸ:
    Command-line program to log process forks, execs and exits;
    useful for tracking runaway processes
  - [GrandPerspective](https://grandperspectiv.sourceforge.net/)ᴹ
    ([SourceForge](https://sourceforge.net/projects/grandperspectiv/)):
    Visualize disk usage using a tree map, macOS
  - [htop](https://github.com/htop-dev/htop)ᴸᴹ or
    [zenith](https://github.com/bvaisvil/zenith)ᴸᴹ or
    [bottom](https://github.com/ClementTsang/bottom)ᴸᴹᵂ:
    System resource monitor, alternative to `top`; note that dumb
    terminals or logging ncurses should not be used, in which case do
    `top -b` to run `top` in batch mode (or `top -b -n NUMBER` to
    limit the number of iterations to `NUMBER`)
  - [hyperfine](https://github.com/sharkdp/hyperfine)ᴸᴹᵂ:
    Command-line benchmarking tool that supports multiple and warmup
    runs, export of results to various formats, etc; basically a
    featureful alternative to the standard `time` command
  - [inxi](https://smxi.org/)ᴸ
    ([Codeberg](https://codeberg.org/smxi/inxi)) or
    [hardinfo2](https://www.hardinfo2.org/)ᴸ
    ([Github](https://github.com/hardinfo2/hardinfo2)):
    Tool to show system information; inxi is a CLI tool, hardinfo is a
    GUI tool, both are Linux-only; for macOS, use the built-in
    `system_profiler` (CLI) or "System Information.app" (GUI); a
    nice-looking CLI alternative but with less functionality is
    [fastfetch](https://github.com/fastfetch-cli/fastfetch)ᴸᴹᵂ
  - [klogg](https://klogg.filimonov.dev/)ᴸᴹᵂ
    ([Github](https://github.com/variar/klogg)):
    Cross-platform GUI log explorer
  - [lnav](https://lnav.org/)ᴸᴹ
    ([Github](https://github.com/tstack/lnav)):
    TUI log file viewer supporting multiple file formats and
    compression types, with the ability to consolidate multiple
    log files into a single view
  - [osquery](https://github.com/osquery/osquery)ᴸᴹᵂ:
    Expose operating system as a relational database
    ([HN](https://news.ycombinator.com/item?id=39501281))
  - [pstree](https://github.com/FredHucht/pstree)ᴸᴹ:
    List processes as a tree
  - [Sloth](https://sveinbjorn.org/sloth)ᴹ
    ([Github](https://github.com/sveinbjornt/Sloth)):
    macOS application to show open files, dirs, sockets and pipes
  - [tree](https://oldmanprogrammer.net/source.php?dir=projects/tree)ᴸᴹ
    ([Github](https://github.com/Old-Man-Programmer/tree),
    [Gitlab](https://gitlab.com/OldManProgrammer/unix-tree)):
    Command-line tool to list files in subdir tree depth-indented
- Text (general)
  - [Aspell](http://aspell.net/)ᴸᴹ
    ([Savannah](https://savannah.gnu.org/projects/aspell/),
    [Github](https://github.com/GNUAspell/aspell)) or
    [Nuspell](https://nuspell.github.io/)ᴸᴹᵂ
    ([Github](https://github.com/nuspell/nuspell)) or
    [Hunspell](https://hunspell.github.io/)ᴸᴹ
    ([Github](https://github.com/hunspell/hunspell)) or
    [Enchant](https://abiword.github.io/enchant/)ᴸᴹ
    ([Github](https://github.com/AbiWord/enchant)):
    Command-line spell checkers and libraries; Enchant is a wrapper
    for abstracting different spell checking libraries, including
    the others mentioned here, into a single interface; dictionaries
    for Hunspell can be found [here](http://wordlist.aspell.net/dicts/)
  - [bat](https://github.com/sharkdp/bat):
    Command-line `cat` clone with syntax highlighting and Git integration
  - [cspell](https://cspell.org/)ᴿ
    ([Github](https://github.com/streetsidesoftware/cspell)):
    Code-aware spell checker
  - [daff](https://paulfitz.github.io/daff/)ᴿ
    ([Github](https://github.com/paulfitz/daff)):
    Like `diff` but for tables
  - [delta](https://dandavison.github.io/delta/)ᴸᴹᵂ
    ([Github](https://github.com/dandavison/delta)):
    `diff` alternative
  - [FIGlet](http://www.figlet.org/)ᴸᴹᵂ
    ([Github](https://github.com/cmatsuoka/figlet)):
    Convert text to large ASCII word art, additional fonts are
    available [here](https://github.com/cmatsuoka/figlet-fonts)
  - [hexyl](https://github.com/sharkdp/hexyl)ᴸᴹᵂ:
    Command-line hex viewer
  - [ImHex](https://imhex.werwolv.net/)ᴸᴹᵂ
    ([Github](https://github.com/WerWolv/ImHex)) or
    [HexFiend](https://hexfiend.com/)ᴹ
    ([Github](https://github.com/HexFiend/HexFiend)) or
    [bvi](https://bvi.sourceforge.net/)ᴸ
    ([Github](https://github.com/buergmann/bvi)) or
    [hyx](https://yx7.cc/code/):
    Hex editor; alternatively, use `:%!xxd` and `:%!xxd -r` in Vim to
    go from binary to hex and back again
    ([link](https://vim.fandom.com/wiki/Improved_hex_editing)), or
    `M-x hexl-find-file` or `M-x hexl-mode` in Emacs
    ([link](https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Binary-Files.html))
  - [LanguageTool](https://languagetool.org/)ᴿ
    ([Github](https://github.com/languagetool-org/languagetool)):
    Style and grammar checker; standalone Java version downloadable
    [here](https://languagetool.org/download/), snapshots available
    [here](https://internal1.languagetool.org/snapshots/) (the link
    [link](https://languagetool.org/download/LanguageTool-stable.zip)
    has the latest stable version), and is augmentable
    ([instructions](https://github.com/nschang/languagetool-101)) with
    [n-gram](https://dev.languagetool.org/finding-errors-using-n-gram-data.html)
    [data](https://languagetool.org/download/ngram-data/) for better error finding,
    [fasttext](https://fasttext.cc/) for better language detection and
    [word2vec](https://languagetool.org/download/word2vec/) for confusion pair disambiguation
  - [FileMerge](https://developer.apple.com/xcode/features/)ᴹ or
    [Meld](https://meldmerge.org/)ᴸ or
    [kdiff3](https://invent.kde.org/sdk/kdiff3)ᴸᴹᵂ or
    [tkdiff](https://sourceforge.net/projects/tkdiff/)ᴸ or
    [xxdiff](https://github.com/blais/xxdiff)ᴸ or
    [P4Merge](https://www.perforce.com/products/helix-core-apps/merge-diff-tool-p4merge)ᴸᴹᵂ:
    GUI `diff` alternative; Meld supports Windows and Linux and has a
    [macOS port](https://github.com/yousseb/meld)ᴹ, kdiff3 is
    [cross-platform](https://download.kde.org/stable/kdiff3), xxdiff
    is lightweight but does not support Unicode, FileMerge comes with
    the macOS XCode IDE and callable from the command-line using
    `opendiff`, tkdiff is lightweight and straightforward to set up on
    macOS (see _Mac Notes_ > _Graphical diff and merge tool_), P4Merge
    is free but not open-source; these also diff directories
  - [par](http://www.nicemice.net/par/)ᴸ
    ([BitBucket](https://bitbucket.org/amc-nicemice/par/)):
    Paragraph reformatter, like a smarter version of `fmt` from GNU
    [coreutils](https://www.gnu.org/software/coreutils/); note that
    macOS built-in `fmt` does not support Unicode, while `par` and
    GNU Coreutils `fmt` (installable as `gfmt` from the MacPorts
    or Homebrew `coreutils` package on macOS) do support Unicode
  - [sttr](https://github.com/abhimanyu003/sttr)ᴸᴹᵂ:
    Command-line tool for string operations
  - [uni](https://github.com/arp242/uni)ᴸᴹᵂ or
    [unicode](https://github.com/robpike/unicode)ᴸᴹᵂ or
    [chars](https://github.com/antifuchs/chars)ᴸᴹᵂ:
    Command-line tool for querying Unicode characters
- Text (structured)
  - [The One True Awk](https://github.com/onetrueawk/awk)ᴸ:
    Version of AWK described in The Awk Programming Language
    [book](https://awk.dev/); alternative implementations include
    [Gawk](https://www.gnu.org/software/gawk/manual/) and
    [others](https://www.gnu.org/software/gawk/manual/html_node/Other-Versions.html)
  - [csvquote](https://github.com/dbro/csvquote)ᴸ:
    Makes its easier to use CSV files with standard Unix tools like
    awk, sed, cut and join by converting (and reverting) embedded
    commas and newlines to non-printing characters, for example
    `csvquote a.csv | cut -d',' -f3 | sort | uniq -c | csvquote -u`
  - [fastgron](https://github.com/adamritter/fastgron)ᴸᴹᵂ:
    Flattens JSON into discrete assignments that work better with
    `grep` and `sed`; [gron.awk](https://github.com/xonixx/gron.awk)
    is similar but implemented in pure Awk
  - [fq](https://github.com/wader/fq)ᴸᴹᵂ:
    Like `jq` but for binary formats
  - [ghostwriter](https://ghostwriter.kde.org/)ᴸᵂ
    ([Github](https://github.com/KDE/ghostwriter),
    [Invent](https://invent.kde.org/office/ghostwriter)):
    Graphical Markdown editor; Windows and Linux (also on Flathub)
  - [htmlq](https://github.com/mgdm/htmlq)ᴸᴹᵂ or
    [cascadia](https://github.com/suntong/cascadia)ᴸᴹᵂ:
    Like grep for HTML but using CSS selectors
  - [jq](https://jqlang.github.io/jq/)ᴸᴹᵂ
    ([Github](https://github.com/jqlang/jq)):
    Command-line JSON processor
  - [jless](https://jless.io/)ᴸᴹ
    ([Github](https://github.com/PaulJuliusMartinez/jless)):
    Ncurses command-line JSON viewer
  - [jo](https://github.com/jpmens/jo)ᴸᵂ:
    Command-line utility for creating JSON objects
  - [Miller](https://github.com/johnkerl/miller)ᴸᴹᵂ or
    [qsv](https://github.com/jqnatividad/qsv)ᴸᴹᵂ
    (fork of [xsv](https://github.com/BurntSushi/xsv)ᴸᴹᵂ):
    Command-line tool for working with CSV files; Miller also supports
    TSV and tabular JSON; Miller is more flexible, qsv easier to use
  - [qq](https://github.com/JFryy/qq)ᴸᴹ:
    Config format transcoder with jq-like query syntax, supporting
    JSON, YAML, TOML, XML, INI, HCL, TF, etc
  - [QXmlEdit](http://qxmledit.org/)ᴸᴹᵂ
    ([Github](https://github.com/lbellonda/qxmledit)):
    XML editor
  - [Tad](https://www.tadviewer.com/)ᴸᴹᵂ
    ([Github](https://github.com/antonycourtney/tad)):
    GUI tabular data viewer for CSV, Parquet, SQLite and DuckDB files;
    alternative is [CSView](https://kothar.net/csview) (different from
    another one that is listed here)
  - [Visidata](https://www.visidata.org/)ᴿ
    ([Github](https://github.com/saulpw/visidata)) or
    [CSView](https://github.com/wfxr/csview)ᴸᴹᵂ:
    TUI tabular data viewer; Visidata is more of a multitool, in that
    it allows for editing and supports any source loadable via Pandas
    using the `-f` option
  - [yq](https://github.com/mikefarah/yq)ᴸᴹᵂ:
    Command-line YAML processor
- Text editor or integrated development environment
  - [Acme](https://en.wikipedia.org/wiki/Acme_(text_editor)):
    Mouse-driven GUI text editor from
    [Plan 9](https://en.wikipedia.org/wiki/Plan_9_from_Bell_Labs),
    ported to Linux and macOS
    ([link](https://9fans.github.io/plan9port/),
    [Github](https://github.com/9fans/plan9port)ᴸᴹ)
  - [Emacs](https://www.gnu.org/software/emacs/)ᴸᵂ:
    Extensible TUI and GUI text editor, macOS builds available from
    [here](https://emacsformacosx.com/)ᴹ or
    [here](https://github.com/railwaycat/homebrew-emacsmacport/releases)ᴹ
    or install via some package manager like
    MacPorts (e.g., `port install emacs-mac-app +nativecomp` which
    compiles a native GUI Emacs application, the native-comp variant,
    at `/Applications/MacPorts/EmacsMac.app` or at
    `/Users/Username/Applications/MacPorts/EmacsMac.app`)
  - [Vim](https://www.vim.org/)ᴸᴹᵂ or
    [Neovim](https://neovim.io/)ᴸᴹᵂ:
    TUI text editor based on [vi](https://en.wikipedia.org/wiki/Vi);
    Vim can be found pre-installed on many systems; Neovim is a
    refactor of Vim; other more lightweight clones of vi exist, like
    [OpenVI](https://github.com/johnsonjh/OpenVi)ᴸ,
    [neatvi](https://github.com/aligrudi/neatvi)ᴸ,
    [nextvi](https://github.com/kyx0r/nextvi)ᴸ, or
    [nvi2](https://github.com/lichray/nvi2)ᴸ, (see
    [here](https://mattwidmann.net/notes/the-nvi-text-editor/),
    [here](https://mattwidmann.net/notes/configuring-the-defaults-of-nvi/)
    and [here](https://mattwidmann.net/notes/modern-nvi-mappings/) for
    more info; easily compilable only on BSDs and macOS), but these
    lack a good number of features added in vim and Neovim
  - [Vis](https://github.com/martanne/vis)ᴸ
    ([Sourcehut](https://git.sr.ht/~martanne/vis)):
    TUI text editor combining vi modal editing with
    [Sam](https://en.wikipedia.org/wiki/Sam_(text_editor))'s
    structural regular expressions and command language
  - [Visual Studio Code](https://code.visualstudio.com/)ᴸᴹᵂ or
    [VSCodium](https://vscodium.com/)ᴸᴹᵂ:
    GUI IDE; VSCodium is a build of VSCode that is free
    of tracking and Microsoft branding, but note that some
    Microsoft plugins like pylance (Python LSP server) does
    not support it; some useful plugins include LLM-based
    assistant [Continue](https://continue.dev/docs/intro)
    ([Github](https://github.com/continuedev/continue)) and the
    different extensions for remote development include over SSH
    ([link](https://code.visualstudio.com/docs/remote/remote-overview))
  - [Zed](https://zed.dev/)ᴸᴹ
    ([Github](https://github.com/zed-industries/zed)):
    Multithreaded GPU-accelerated collaborative code editor
- User experience and interface (graphical)
  - [Amethyst](https://ianyh.com/amethyst/)ᴹ
    ([Github](https://github.com/ianyh/Amethyst)) or
    [yabai](https://github.com/koekeishiya/yabai)ᴹ:
    Tiling window manager for macOS
  - [Hyperkey](https://hyperkey.app/)ᴹ:
    Use CapsLock as a "hyper" key, that is, C-Opt-Cmd-Shift, on macOS
  - [Input Leap](https://github.com/input-leap/input-leap)ᴸᴹᵂ:
    Cross-platform software that mimics KVM switch functionality; Macs
    and iPads with the same account can just use the built-in
    [Universal Control](https://support.apple.com/en-us/102459)
  - [LinearMouse](https://github.com/linearmouse/linearmouse)ᴹ or
    [Mos](https://github.com/Caldis/Mos)ᴹ or
    [Scroll Reverser](https://github.com/pilotmoon/Scroll-Reverser)ᴹ:
    Mouse enhancements like reverse scrolling, linear scrolling,
    acceleration, etc, for external mice on macOS
  - [Maccy](https://maccy.app/)ᴹ
    ([Github](https://github.com/p0deje/Maccy)):
    Clipboard manager for macOS
  - [MiddleClick](https://github.com/artginzburg/MiddleClick-Sonoma)ᴹ:
    Middle-click with three-finger tap; commercial alternatives
    include [Middle](https://middleclick.app/)ᴹ,
    [Multitouch](https://multitouch.app/)ᴹ and
    [BetterTouchTool](https://folivora.ai/)ᴹ
  - [Rectangle](https://rectangleapp.com/)ᴹ
    ([Github](https://github.com/rxhanson/Rectangle)):
    Move and resize windows using keyboard shortcuts and snap areas
    in macOS; as an alternative, three of the more useful keyboard
    shortcuts in Rectangle can be replicated using _Preferences_ >
    _Keyboard_ > _Keyboard Shortcuts_ > _App Shortcuts_ and clicking
    on `+`, keeping `All Applications`, inserting as the Menu Title
    action `Move Window to Left Side of Screen` and setting the
    shortcut key as desired (`Ctrl-Option-Left` is recommended),
    and repeating for `Move Window to Right Side of Screen`
    (recommend `Ctrl-Option-Right` here), as well as `Zoom`
    (recommend using `Ctrl-Option-Return` here), **or** hover over the
    green `+` button on the top-left corner of the window and press
    `Option` which allows mouse selection of the actions (does not
    require keyboard shortcut configuration); there's also a
    commercial [Pro](https://rectangleapp.com/pro) version
  - [skhd](https://github.com/koekeishiya/skhd)ᴹ or
    [sxhkd](https://github.com/baskerville/sxhkd)ᴸ (X11) or
    [swhkd](https://github.com/waycrate/swhkd)ᴸ (Wayland):
    Hotkey daemon
  - [stats](https://github.com/exelban/stats)ᴹ:
    Menu bar system monitor for macOS
  - [Textinator](https://github.com/RhetTbull/textinator)ᴹ
    or [TRex](https://github.com/amebalabs/TRex)ᴹ:
    Detect text in screenshots and copy it to the clipboard in macOS
  - [ueli](https://ueli.app/)ᴸᴹᵂ
    ([Github](https://github.com/oliverschwendener/ueli)):
    Launcher like [Alfred](https://www.alfredapp.com/) but open-source
    and cross-platform
  - [Velja](https://sindresorhus.com/velja)ᴹ
    ([App Store](https://apps.apple.com/app/id1607635845)) or
    [finicky](https://github.com/johnste/finicky)ᴹ or
    [Browserosaurus](https://browserosaurus.com/)ᴹ
    ([Github](https://github.com/will-stone/browserosaurus)):
    Open different links in different browsers or apps in macOS
  - [xbar](https://github.com/matryer/xbar)ᴹ or
    [SwiftBar](https://github.com/swiftbar/SwiftBar)ᴹ:
    Pipe output to the menu bar in macOS
  - [Unshaky](https://github.com/aahung/Unshaky)ᴹ:
    Double keypress workaround for Apple Mac butterfly keyboards
- User experience and interface (text)
  - [direnv](https://direnv.net/)ᴸᴹᵂ
    ([Github](https://github.com/direnv/direnv)):
    Load and unload env vars based on location; for `direnv`, config
    files go into `$XDG_CONFIG_HOME/direnv/` and allowed directory
    environment files are recorded in `$XDG_DATA_HOME/direnv/allow/`;
    alternatives: [autoenv](https://github.com/hyperupcall/autoenv)ᴿ,
    [shadowenv](https://github.com/Shopify/shadowenv)ᴸᴹ
  - [hollywood](https://github.com/dustinkirkland/hollywood)ᴸ:
    Hollywood technobabble in a Byobu session; repository also has a
    Wall Street version called wallstreet
    ([announcement](https://blog.dustinkirkland.com/2014/12/hollywood-technodrama.html))
  - [parallel](https://www.gnu.org/software/parallel/)ᴸ:
    Command-line tool for executing jobs in parallel
  - [pv](http://www.ivarch.com/programs/pv.shtml)ᴸ
    ([Codeberg](https://codeberg.org/a-j-wood/pv)):
    Like `cat` (reads from stdin or file and forwards it to stdout)
    while printing progress to stderr; useful for monitoring progress
    when piping large amounts of data from one program to another
  - [rlwrap](https://github.com/hanslub42/rlwrap)ᴸ:
    `readline` wrapper to enable completion and history for any
    command-line tool taking keyboard input
  - [screen](https://www.gnu.org/software/screen/)ᴸ or
    [tmux](https://github.com/tmux/tmux)ᴸ or
    [Zellij](https://zellij.dev/)ᴸᴹ
    ([Github](https://github.com/zellij-org/zellij)):
    Terminal multiplexer, useful for managing and persisting remote
    sessions over Mosh or SSH; generally, tmux is recommended over
    screen unless there is a need to access a serial console (using
    `screen /dev/somedevice` where `somedevice` is the device point
    surfaced after connecting the serial port; for more info, see
    [link](https://old.reddit.com/r/archlinux/comments/d41c1w/screen_vs_tmux/f0dj9rn/);
    for just the session management (detach and re-attach)
    functionality of tmux and screen, there is
    [dtach](https://github.com/crigler/dtach)ᴸ,
    [abduco](https://github.com/martanne/abduco)ᴸ or
    [diss](https://github.com/yazgoo/diss/)ᴸ or
    [shpool](https://github.com/shell-pool/shpool)ᴸ
  - [ttyplot](https://github.com/tenox7/ttyplot)ᴸ:
    Real-time plotting tool in the terminal using stdin as data input
- Virtualization
  - [Blink](https://github.com/jart/blink)ᴸᴹ:
    Tiny x86-64-linux emulator that can run on any POSIX system
  - [UTM](https://github.com/utmapp/UTM)ᴹᴵ:
    iOS and macOS tool for managing [QEMU](https://www.qemu.org/)ᴸ
    virtual machines; alternatives are [lima](https://lima-vm.io/)ᴸᴹ
    ([Github](https://github.com/lima-vm/lima)) if there is no need to
    run GUI apps, or Ubuntu-only [Multipass](https://multipass.run/)ᴸ
    ([Github](https://github.com/canonical/multipass))
  - [virt-manager](https://virt-manager.org/)ᴸ:
    Linux desktop tool for managing QEMU/KVM virtual machines
- VPN
  - [OpenVPN](https://openvpn.net/client/)ᴸᴹᵂᴬᴵ or
    [TunnelBlick](https://tunnelblick.net/)ᴹ
    ([Github](https://github.com/Tunnelblick/Tunnelblick)):
    OpenVPN client, note that Linux clients be should installed via
    the system package manager (see
    [here](https://community.openvpn.net/openvpn/wiki/OpenVPN3Linux)
    and [here](https://gitlab.com/openvpn/openvpn3-linux));
    TunnelBlick is macOS-only and built on the older but still
    maintained OpenVPN 2 libraries
  - [WireGuard](https://www.wireguard.com/)ᴸᴹᵂᴬᴵ:
    Simple and high-performance VPN; can be involved to set up, use
    tools like [wg-easy](https://github.com/wg-easy/wg-easy) for
    easier installation and administration; WireGuard orchestration
    tools include [headscale](https://github.com/juanfont/headscale)ᴸᴹ
    (open-source self-hosted [Tailscale](https://tailscale.com/)ᴸᴹᵂᴬᴵ
    control server alternative designed to be used with Tailscale
    clients), [innernet](https://github.com/tonarino/innernet)ᴸᴹ,
    [nebula](https://github.com/slackhq/nebula)ᴸᴹᵂ, or
    [netbird](https://github.com/netbirdio/netbird)ᴸᴹᵂ;
    non-WireGuard-based alternatives include
    [ZeroTier](https://www.zerotier.com/)ᴸᴹᵂᴬᴵ (self-hosting
    [supported](https://docs.zerotier.com/selfhost))
- Web browsing
  (besides the standard [Chrome](https://www.google.com/chrome/),
  [Firefox](https://www.mozilla.org/en-US/firefox/), and
  [Safari](https://www.apple.com/safari/) browsers)
  - [Amfora](https://github.com/makew0rld/amfora)ᴸᴹᵂ or
    [Bombadillo](https://bombadillo.colorfield.space/)ᴸᴹᵂ
    ([tildegit](https://tildegit.org/sloum/bombadillo)) or
    [gcat](https://github.com/aaronjanse/gcat)ᴿ:
    Gemini TUI clients, available on Debian or compile using
    Go via `go install github.com/makeworld-the-better-one/amfora` or
    `go install tildegit.org/sloum/bombadillo@latest`; Bombadillo
    is also a Gopher client; use gcat in Acme or dumb terminals
  - [Hush](https://oblador.github.io/hush/)ᴹᴵ
    ([Github](https://github.com/oblador/hush)):
    Safari extension to block nag popups and trackers; iOS and macOS
  - [Lagrange](https://gmi.skyjake.fi/lagrange/)ᴸᴹᵂ
    ([Github](https://github.com/skyjake/lagrange)) or
    [Kristall](https://kristall.random-projects.net/)ᴸᴹᵂ
    ([Github](https://github.com/MasterQ32/kristall)):
    Gemini GUI client; Kristall also supports Gopher
  - [Monolith](https://crates.io/crates/monolith)ᴸᴹᵂ
    ([Github](https://github.com/Y2Z/monolith.git)):
    Save complete webpages to a single HTML file with embedded CSS,
    images and Javascript; macOS version needs to be self-compiled
  - [shot-scraper](https://shot-scraper.datasette.io/)ᴿ
    ([Github](https://github.com/simonw/shot-scraper)):
    Command-line utility for taking partial or full screenshots of
    websites
  - [YT-DLP](https://github.com/yt-dlp/yt-dlp)ᴸᴹᵂ:
    Fork of [youtube-dl](https://youtube-dl.org/), example usage
    is `yt-dlp --list-formats URL` (change `URL` to video's URL)
    to list formats and `yt-dlp -f 123 URL` to download just format
    `123` from the listed formats (usually one with combined video
    and audio) or `yt-dlp -f 123+456 URL` to download formats `123`
    and `456` then merge them (usually one video and one audio)
- Web development
  - [doge](https://github.com/Dj-Codeman/doge)ᴸᴹᵂ:
    Alternative to `dig`, fork of [dog](https://github.com/ogham/dog)
    which is no longer maintained
  - [Hurl](https://hurl.dev/)ᴸᴹᵂ
    ([Github](https://github.com/Orange-OpenSource/hurl)):
    Command-line tool for running HTTP requests defined in a text file
  - [frp](https://github.com/fatedier/frp)ᴸᴹᵂ:
    Reverse proxy, like [ngrok](https://ngrok.com/)
  - [httpie](https://httpie.io/)ᴿ
    ([Github](https://github.com/httpie/cli)) or
    [xh](https://github.com/ducaale/xh)ᴸᴹᵂ:
    Command-line API client; much lighter weight than
    [Postman](https://www.postman.com/)
  - [Mockoon](https://github.com/mockoon/mockoon)ᴸᴹᵂ:
    HTTP mock server
  - [mitmproxy](https://mitmproxy.org/))ᴸᴹᵂ
    ([Github](https://github.com/mitmproxy/mitmproxy)):
    Interactive HTTPS proxy
  - [Prism](https://stoplight.io/open-source/prism)ᴿ
    ([Github](https://github.com/stoplightio/prism)):
    HTTP mock server with behavior that can be specified from OpenAPI
    v2 (Swagger), OpenAPI v3 or Postman Collection files
  - [Skyvern](https://www.skyvern.com/)ᴸᴹᵂ
    ([Github](https://github.com/Skyvern-AI/Skyvern/)):
    Automated browser workflows using computer vision and LLMs;
    [article](https://blog.skyvern.com/we-open-sourced-and-ended-up-1-on-hackernews/)
- Word processing
  - [Manuskript](https://www.theologeek.ch/manuskript/)ᴸᴹᵂ
    ([Github](https://github.com/olivierkes/manuskript)):
    Word processor for writers like Scrivener
  - [Twine](https://twinery.org/)ᴸᴹᵂᴿ
    ([Github](https://github.com/klembot/twinejs)):
    Tool for authoring interactive, non-linear stories
- Other
  - [age](https://github.com/FiloSottile/age)ᴸᴹᵂ or
    [ccrypt](https://ccrypt.sourceforge.net/)ᴸ:
    Command-line encryption/decryption tool; ccrypt is simpler (prompt
    for password when encrypting, use same password to decrypt); age
    supports using either identity files or generates a token to be
    used by a recipient; or just use GnuPG, either without public key
    cryptography by running `gpg -o FILE.gpg -c FILE` to encrypt a
    file with a symmetric cipher using a passphrase where the
    encrypted file can later be decrypted with `gpg -d FILE.gpg` and
    the same passphrase, or with public key cryptography by encrypting
    a file using a recipient's public key and `gpg -r EMAIL -o
    FILE.gpg -e FILE` where the encrypted file can later be decrypted
    with the recipient's private key and `gpg -d <file>.gpg`; better
    to use something like Cryptomator if not encrypting to share
  - [Aiko](https://sindresorhus.com/aiko)ᴹᴵ
    ([App Store](https://apps.apple.com/app/id1672085276)):
    iOS and macOS app for local audio transcription using OpenAI's
    [Whisper](https://openai.com/research/whisper) model
  - [Al Dente](https://github.com/davidwernhart/AlDente)ᴹ:
    macOS tool to limit battery charging (i.e., the claim is keeping
    charge percentage at or below 80% can help prolong battery life),
    however this may not be necessary if "Optimized Battery Charging"
    is sufficient for usage needs; there is also a paid Pro version
    with more features available [here](https://apphousekitchen.com/)
  - [Amazing AI](https://sindresorhus.com/amazing-ai)ᴹᴵ
    ([App Store](https://apps.apple.com/app/id1660147028)):
    iOS and macOS (Apple Silicon M1/M2/... chip required) app for
    local image generation from text using the Stable Diffusion model
  - [Anki](https://apps.ankiweb.net/)ᴸᴹᵂᴬᴵ
    ([Github](https://github.com/ankitects/anki)):
    Flashcards software; cross-platform, and also available on
    [iOS](https://apps.apple.com/us/app/ankimobile-flashcards/id373493387),
    [Android](https://play.google.com/store/apps/details?id=com.ichi2.anki),
    and as a [webapp](https://ankiweb.net/)
  - [ClamAV](https://www.clamav.net/)ᴸᴹᵂ
    ([Github](https://github.com/Cisco-Talos/clamav)):
    Open-source antivirus engine, with frontends available like
    [ClamTk](https://gitlab.com/dave_m/clamtk)ᴸ
  - [diffoscope](https://diffoscope.org/)ᴸᴿ
    ([Salsa](https://salsa.debian.org/reproducible-builds/diffoscope)):
    Like `diff` but supports archives and directories in addition to
    files, and can output to formats like HTML, JSON, Markdown and
    reStructuredText in addition to text; as a pure Python package, it
    is installable via `pip` from [PyPI](https://pypi.org/)
  - [elfcat](https://github.com/ruslashev/elfcat)ᴸ:
    ELF visualizer using HTML files generated from ELF binaries
  - [entr](https://github.com/eradman/entr)ᴸᴹ or
    [watchdog](https://github.com/gorakhargosh/watchdog)ᴿ or
    [watchexec](https://github.com/watchexec/watchexec)ᴸᴹᵂ or
    [watchfiles](https://github.com/samuelcolvin/watchfiles) or
    [Watch](https://pkg.go.dev/9fans.net/go/acme/Watch)ᴿ or
    [Watchman](https://github.com/facebook/watchman)ᴸᴹᵂ:
    Run command when files change; entr, watchexec and watchfiles are
    somewhat easier to use; Watch is Acme editor-specific; Watchman
    has a client-server architecture and is more of a per-user system
    service; watchdog is a library but has an optional tool watchmedo
    (install `watchdog[watchmedo]` via pip) similar to watchexec
  - [FontForge](https://fontforge.org/)ᴸᴹᵂ
    ([Github](https://github.com/fontforge/fontforge)):
    Open-source cross-platform scriptable font editor
  - [Goxel](https://goxel.xyz/)ᴸᴹᵂ
    ([Github](https://github.com/guillaumechereau/goxel)):
    3D voxel editor
  - [grex](https://github.com/pemistahl/grex)ᴸᴹᵂ:
    Generate regex from test cases
  - [Hammerspoon](https://www.hammerspoon.org/)ᴹ
    ([Github](https://github.com/Hammerspoon/hammerspoon)):
    Use [Lua](https://www.lua.org/) for macOS scripts that can call
    system APIs, for example middle-click-move mouse to scroll
    ([link](https://superuser.com/questions/303424/can-i-enable-scrolling-with-middle-button-drag-in-os-x))
  - [ivy](https://github.com/robpike/ivy)ᴸᴹᵂ:
    APL-like calculator ([docs](https://pkg.go.dev/robpike.io/ivy)),
    install with `go install robpike.io/ivy@latest` (requires Go)
  - [hyperfine](https://github.com/sharkdp/hyperfine)ᴸᴹᵂ:
    Benchmark shell commands
  - [Jiggler](http://www.sticksoftware.com/software/Jiggler.html)ᴹ
    ([Github](https://github.com/bhaller/Jiggler)):
    Keep the system awake, useful when running lengthy tasks, macOS
  - [Joplin](https://joplinapp.org/)ᴸᴹᵂᴬᴵ
    ([Github](https://github.com/laurent22/joplin/)):
    Note-taking application with sync support; cross-platform
  - [kgt](https://github.com/katef/kgt/)ᴸᴹ:
    Convert between BNF syntaxes and visualize using railroad diagrams
  - [LCDF Typetools](https://www.lcdf.org/type/)ᴸ
    ([Github](https://github.com/kohler/lcdf-typetools)):
    Font manipulation utilities, `otfinfo` is useful for getting
    available features and code points in TTF and OTF fonts; if using
    MacPorts, `port install lcdf-typetools -texlive` installs the
    utilities without pulling in TexLive as a dependency
  - [libfsm](https://github.com/katef/libfsm)ᴸᴹ:
    NFA, DFA (finite automata), regex and lexical analysis tools;
    [re](https://github.com/katef/libfsm/blob/main/doc/tutorial/re.md)
    can compile regex to state machine and output as a `.dot` diagram
  - [Mathics](https://mathics.org/)ᴿ
    ([Github org](https://github.com/Mathics3)):
    Open-source alternative to
    [Mathematica](https://www.wolfram.com/mathematica/)
  - [Nextcloud](https://nextcloud.com/)ᴸ
    ([Github org](https://github.com/nextcloud)):
    Self-host collaboration platform offering similar functionality to
    Google Suite; Nextcloud Files can be used as a a WebDAV service;
    has desktopᴸᴹᵂ and mobileᴬᴵ clients
    ([link](https://nextcloud.com/install/)); supports apps like
    [Music](https://apps.nextcloud.com/apps/music)
    ([Github](https://github.com/owncloud/music)) which adds a music
    player and Ampache/Subsonic music server (iOS clients include
    [Amperfy](https://github.com/BLeeEZ/amperfy) and
    [play:Sub](https://michaelsapps.dk/playsubapp/))
  - [Ollama](https://ollama.com/)ᴸᴹᵂ
    ([Github](https://github.com/ollama/ollama)):
    Serve LLMs locally; on the Mac, the app bundle requires admin
    privileges but the standalone binary does not
  - [OmegaT](https://omegat.org/)ᴸᴹᵂᴿ
    ([Github](https://github.com/omegat-org/omegat)):
    Translation memory tool
  - [pass](https://www.passwordstore.org/)ᴿ
    ([Git](https://git.zx2c4.com/password-store/)) or
    [gopass](https://www.gopass.pw/)ᴸᴹᵂ
    ([Github](https://github.com/gopasspw/gopass)):
    Command-line password manager; **pass-specific instructions**:
    on macOS pass is available in MacPorts or Homebrew, or install
    directly from source where if using MacPorts along with
    XCode command-line tools Git, install dependencies `tree`,
    `util-linux` and `qrencode` with MacPorts, and install `pass`
    from source specifying the `PREFIX` environment variable
    as appropriate (e.g., `PREFIX=$HOME/.local make install`),
    then modifying the installed `lib/password-store/platform.sh`
    file (e.g., `$HOME/.local/lib/password-store/platform.sh`)
    so that the line `GETOPT=...` points directly
    to the MacPorts-installed `getopt` (e.g.,
    `GETOPT="/Users/$(whoami)/macports/bin/getopt"`) (for more, see
    [link](https://gist.github.com/abtrout/d64fb11ad6f9f49fa325), and
    for completion support when installing from source see `INSTALL`
    in the pass source or directly copy the completion files
    to the appropriate directories (e.g., for Zsh, copy the file
    `src/completion/pass.zsh_completion` to a file with name prefixed
    by `_` in an `$fpath` dir, say `$HOME/.zsh_completions/_pass`);
    **gopass-specific instructions**: gopass is mostly
    compatible with pass, its binaries can be downloaded
    [here](https://github.com/gopasspw/gopass/releases),
    and Zsh completion can be enabled by copying or symlinking
    `zsh.completion` from the extracted gopass binary archive to a
    `_`-prefixed file in an `$fpath` directory using a command like
    `ln -s /path/to/gopass/zsh.completion ~/.zsh_completions/_gopass`
    and rebuilding the cache via `rm ~/.zcompdump && compinit`,
    and optionally add `alias pass=gopass` to `~/.zshrc` to use
    gopass as a drop-in replacement for pass; **supporting apps**:
    [Pass (iOS)](https://github.com/mssun/passforios)ᴵ
    ([Appstore](https://apps.apple.com/us/app/pass-password-store/id1205820573)),
    [Password Store (Android)](https://github.com/android-password-store/android-password-store)ᴬ
    ([Play Store](https://play.google.com/store/apps/details?id=dev.msfjarvis.aps))
  - [pastel](https://github.com/sharkdp/pastel)ᴸᴹᵂ or
    [rgb-tui](https://github.com/ArthurSonzogni/rgb-tui)ᴸᴹᵂ:
    Terminal color picker
  - [Platypus](https://sveinbjorn.org/platypus)ᴹ
    ([Github](https://github.com/sveinbjornt/Platypus)):
    macOS tool for wrapping command-line programs into a
    macOS application bundle
  - [sc-im](https://github.com/andmarti1424/sc-im)ᴸ
    Terminal spreadsheet program
  - [Stellarium](https://stellarium.org/)ᴸᴹᵂ
    ([Github](https://github.com/Stellarium/stellarium)):
    Open-source desktop planetarium
  - [Sweet Home 3D](https://www.sweethome3d.com/)ᴸᴹᵂᴬᴵ
    ([SourceForge](https://sourceforge.net/projects/sweethome3d/)):
    Open-source interior design application, also available as a
    [webapp](https://www.sweethome3d.com/SweetHome3DOnlineManager.jsp)
  - [Task Spooler](https://viric.name/soft/ts/)ᴸ or
    [nq](https://github.com/leahneukirchen/nq)ᴸ or
    [Pueue](https://github.com/Nukesor/pueue)ᴸᴹᵂ:
    Lightweight command-line utility for queueing jobs; Task Spooler
    and nq only support running jobs sequentially, while Pueue can
    also run jobs in parallel
  - [uutils](https://uutils.github.io/)
    ([Github org](https://github.com/uutils)):
    Re-implementation of standard command-line tools in Rust
    ([coreutils](https://github.com/uutils/coreutils)ᴸᴹᵂ,
    [findutils](https://github.com/uutils/findutils)ᴸᴹᵂ,
    [diffutils](https://github.com/uutils/diffutils)ᴸᴹᵂ)
  - [Vale](https://vale.sh/)ᴸᴹᵂ
    ([Github](https://github.com/errata-ai/vale)):
    Linter for prose
  - [Zotero](https://www.zotero.org/)ᴸᴹᵂᴵ
    ([Github](https://github.com/zotero/zotero)):
    Reference management software to collect, organize, cite and
    share research material

## Closed-source and Commercial software

- Audio and video creation and editing
  - [DaVinci Resolve](https://www.blackmagicdesign.com/products/davinciresolve)ᴸᴹᵂ:
    Freeware non-linear video editor, has paid
    [version](https://www.blackmagicdesign.com/products/davinciresolve/studio)ᴸᴹᵂ
  - [EZ CD Audio Converter](https://www.poikosoft.com/music-converter)ᵂ:
    Freeware audio format converter; alternatively, foobar2000
    (specifically the Windows version) comes with a
    [converter](https://wiki.hydrogenaud.io/index.php?title=Foobar2000:Converter)
    ([supported formats](https://www.foobar2000.org/encoderpack))
  - [Mp3tag](https://www.mp3tag.de/)ᵂ and
    [Mp3tag for Mac](https://mp3tag.app/)ᴹ:
    Edit metadata for many different audio file formats;
    Mp3tag for Windows is freeware and Mp3tag for Mac is paid
- Audio and video playback and streaming
  - [foobar2000](https://www.foobar2000.org/)ᴹᵂᴬᴵ:
    Freeware audio player
- Containerization
  - [Orbstack](https://orbstack.dev/)ᴹ:
    macOS Docker runtime and Linux virtual machine management,
    more performant and lightweight than Docker Desktop and Colima
    as of July 2023; free for personal use
- Database client
  - [DbVisualizer](https://www.dbvis.com/)ᴸᴹᵂ:
    Cross-database SQL client with data visualization capabilities;
    free version available with limited functionality
  - [qStudio](https://www.timestored.com/qstudio/)ᴸᴹᵂᴿ:
    Java-based cross-database SQL client, notable for supporting kdb+;
    register on product website for a free license for use with all
    databases except kdb+, which requires a commercial license
- Diagramming
  - [Monodraw](https://monodraw.helftone.com/)ᴹ:
    ASCII art editor; macOS (App Store and direct download)
- Digital Design
  - [Lunacy](https://icons8.com/lunacy)ᴸᴹᵂ
    ([Snap Store](https://snapcraft.io/lunacy),
    [App Store](https://apps.apple.com/us/app/lunacy-graphic-design-editor/id1582493835),
    [Microsoft Store](https://apps.microsoft.com/detail/9pnlmkkpcljj)):
    Freeware design and prototyping app like
    [Sketch](https://www.sketch.com/)ᴹ
- Ebook
  - [Vellum](https://vellum.pub/)ᴹ or
    [Atticus](https://www.atticus.io/):
    Format ebooks and print books for publication; Vellum is
    macOS-only and expensive but more polished, while Atticus is
    less polished but cross-platform and cheaper
- File management
  - [DevonThink Pro](https://www.devontechnologies.com/apps/devonthink)ᴹ:
    Document management and search solution; macOS
  - [Mountain Duck](https://mountainduck.io/)ᴹᵂ:
    Like `rclone mount` but with a nicer user interface and its vaults
    are interoperable with Cryptomator vaults
- Markup language document conversion and rendering
  - [AntennaHouse](https://www.antennahouse.com/)ᴸᴹᵂ or
    [Prince](https://www.princexml.com/)ᴸᴹᵂ:
    Convert HTML to PDF files; Prince is free for non-commercial use
    (note that the free version watermarks the resulting PDF file)
- Project (codebase)
  - [Sublime Merge](https://www.sublimemerge.com/)ᴸᴹᵂ:
    GUI Git client
- Remote login and desktop
  - [Jump Desktop](https://jumpdesktop.com/)ᴹᵂ:
    Remote desktop software, connect to host systems that have
    [Jump Desktop Connect](https://jumpdesktop.com/connect/)
    installed
  - [Royal TSX](https://royalapps.com/ts/)ᴹᵂ:
    Cross-platform RDP and VNC client; has free version
- Text (general)
  - [Araxis Merge](https://www.araxis.com/merge/)ᴹᵂ or
    [Beyond Compare](https://www.scootersoftware.com/)ᴸᴹᵂ or
    [Deltawalker](https://www.deltawalker.com/)ᴸᴹᵂ or
    [Kaleidoscope](https://kaleidoscope.app/)ᴹ:
    Compare files and folders; Deltawalker and Kaleidoscope
    also support diffing images
- Text (structured)
  - [Modern CSV](https://www.moderncsv.com/)ᴸᴹᵂ:
    Cross-platform CSV editor/viewer
- Text editor or integrated development environment
  - [DataGrip](https://www.jetbrains.com/datagrip/)ᴸᴹᵂ:
    Database admin-oriented IDE
  - [DataSpell](https://www.jetbrains.com/dataspell/)ᴸᴹᵂ:
    Data science-oriented IDE
  - [PyCharm](https://www.jetbrains.com/pycharm/)ᴸᴹᵂ:
    Python programming-oriented IDE; has open-source community edition
  - [RStudio](https://rstudio.com/)ᴸᴹᵂ:
    IDE for R; has open-source edition
  - [Sublime Text](https://www.sublimetext.com/)ᴸᴹᵂ:
    Lightweight text editor
- User experience and interface (graphical)
  - [Choosy](https://choosy.app/)ᴹ or
    [OpenIn](https://loshadki.app/openin4/)ᴹ:
    Open different links in different browsers or apps in macOS; free
    alternatives include Velja, finicky and Browserosaurus (see above)
  - [shottr](https://shottr.cc/)ᴹ:
    Screenshot app for macOS, free for non-commercial use; note that
    macOS has built-in screenshot taking via `Cmd-Shift-3` (capture
    full screen), `Cmd-Shift-4` (select a region and capture it),
    `Cmd-Shift-4-Space` (capture window), and via the Screenshot app
    (`Cmd-Shift-5` or use Spotlight search)
  - [Synergy](https://symless.com/synergy)ᴸᴹᵂ
    ([Github](https://github.com/symless/synergy-core/)):
    Cross-platform software mimicking KVM switch functionality to share
    a keyboard and a mouse across systems; source code available for
    [self-compile](https://symless.com/synergy/news/how-to-use-synergy-for-free),
    but is also available compiled by a third-party as
    [binaries](https://github.com/DEAKSoftware/Synergy-Binaries)ᴸᴹᵂ
- Word processing
  - [Scrivener](https://www.literatureandlatte.com/scrivener/overview)ᴹᵂᴵ:
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
cd ~/.local
# Download binary and place into a directory on the path
curl -Ls https://micro.mamba.pm/api/micromamba/osx-arm64/latest | tar -xj bin/micromamba
# Set up micromamba in shell config
micromamba shell init -s zsh -p "$HOME/micromamba"
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

### Script for installing global tools using conda

This repository contains a script
[`setup-conda-tools.sh`](stow/utils/dot-local/bin/setup-conda-tools.sh)
that helps to install tools inside a conda environment,
and make it available globally by creating a wrapper script
in a directory in `$PATH` (default: `$HOME/.local/bin`).

**Usage example**:

Create a `conda-tools.yml` file with the following contents.

```yaml
name: conda-tools
channels:
  - conda-forge
dependencies:
  - editorconfig
  - libqrencode # qrencode
  - mosh
  - pandoc # pandoc pandoc-lua pandoc-server
  - pstree
  - ripgrep # rg
  - tree
```

Running

```console
$ setup-conda-tools.sh install conda-tools.yml
```

will create a `conda-tools` conda environment with editorconfig,
libqrencode, mosh, pandoc, pstree, ripgrep and tree installed, and
create executable wrappers for the `editorconfig`, `qrencode`, `mosh`,
`pandoc`, `pandoc-lua`, `pandoc-server`, `pstree`, `rg` and `tree`
commands from that environment in the `$HOME/.local/bin/` dir.

**Notes**:

- In the YAML file, the commands to wrap are indicated in a trailing
  comment for its corresponding package, separated by whitespace. If
  there is a trailing comment but no commands listed, no wrappers are
  created for that package. If there is no trailing comment for a
  package, then the default behavior is to assume a command exists
  with the same name as the command and to wrap that.

- Some packages install multiple commands, so it may be useful to
  refer to a package listing to figure out what commands are
  installed. For example, the file listing for a Debian package
  `graphviz` for Debian release `bullseye` can be found at
  [`https://packages.debian.org/bullseye/all/graphviz/filelist`](https://packages.debian.org/bullseye/all/graphviz/filelist)
  and installed commands would be files with the `/usr/bin/` prefix.

## Python virtual environments

Conda (see above) can work as a virtual environment manager.

However, another option is to use a dedicated Python version
manager like [pyenv](https://github.com/pyenv/pyenv) or
[asdf](https://github.com/asdf-vm/asdf) to control Python
version, and Python-native virtual environment tooling like
[venv](https://docs.python.org/3/library/venv.html) to create an
isolated Python environment for the installing packages specific
to the code being developed.

Conda can be more convenient it comes with pre-built packages
both within and outside the Python ecosystem, but there may be
dependency resolution issues, build versions may not match up across
architectures, and mixing `pip install`-ed and `conda install`-ed
packages can create subtle packaging issues that are hard to resolve.

In contrast, pyenv and asdf (in conjunction with venv) are simpler
(they only build a base version of Python), but don't come with the
convenience of pre-built packages (especially those outside
the Python ecosystem that may be involved to build).

### Using specific Python versions

Conda already provides a way to do this, just create a new
environment with some specific python version (or no specific
version for the latest). For example, `conda create -n myenv
python=3.11.8` creates a new environment called `myenv` with Python
3.11.8 installed, `conda activate myenv` activates that environment,
and `conda deactivate` after that deactivates it.

Otherwise, use [pyenv](https://github.com/pyenv/pyenv) to install
and switch between different versions of Python.

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

If using Conda, see the previous subsection.

Otherwise, one can use pyenv via the plugin
[pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv)
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
version installed via pyenv (or asdf; version `3.11.8` is used here,
change as needed), at a `.venv` subdirectory in the project folder:

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

This can also be done using Conda, in a similar way as asdf (use
`conda activate myenv` instead of `asdf local python 3.11.8`).

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

### Installing [Spack](https://spack.io/)

Make sure the XCode command-line developer tools are installed.

To install Spack, create a `default` environment, and install some
software (`gnupg` and `hunspell` shown here) into it:

```console
$ cd ~
$ git clone -c feature.manyFiles=true https://github.com/spack/spack.git
$ . ~/spack/share/spack/setup-env.sh  # add this to ~/.zshrc or ~/.bashrc
$ spack install gawk gnupg hunspell
$ spacktivate  # same as "spack env create default" then "spack env activate default"
$ spack add gawk gnupg hunspell
$ spack install
$ spack gc
```

(Note: to use specific releases of Spack rather than the default
rolling `develop` branch, check out the appropriate branch or tag.
For example, to use the latest version in the `v0.21.*` series run
`git checkout releases/v0.21` after cloning the repository, or to use
`v0.21.2` run `git checkout tag/v0.21.2` after cloning the
repository.)

To load Spack and activate the `default` environment automatically on
Zsh shell startup:

```sh
cat >>~/.zshrc <<EOF
# Spack
. ~/spack/share/spack/setup-env.sh
spacktivate  # short command for spack env activate
EOF
```

If desired, before `spack install ...` above, add a build
[cache](https://spack.readthedocs.io/en/latest/binary_caches.html)
(the [develop](https://cache.spack.io/tag/develop/) version of the
[official](https://cache.spack.io/) build cache is used below; as of
2024-March-16, for macOS it only supports Ventura and not Sonoma):

```console
$ spack mirror add develop https://binaries.spack.io/develop
$ spack buildcache keys --install --trust
```

Useful Spack commands (for a more complete listing, see
[here](https://spack.readthedocs.io/en/latest/basic_usage.html)):

- `spack help` for usage help
- `spack list [PACKAGE]` lists packages Spack can install,
  optionally filtered by a PACKAGE name if specified
- `spack info PACKAGE` shows info the package PACKAGE
- `spack install PACKAGE...` installs the given packages
  - Use the `-d` option for more verbose output
  - Use the `--add` option to incrementally install the package and
    concretize in the active environment
  - Install a specific version by using `@`,
    e.g., `spack install mpich@3.0.4`
  - Use a specific compiler by using `%` (also allows adding compiler
    flags), e.g., `spack install mpich %gcc@13.2.0 cflags="-O3 -g"`
    installs `mpich` using GCC version 13.2.0 with `CFLAGS="-O3 -g"`
- `spack uninstall PACKAGE...` uninstalls the given packages
  - To also uninstall dependents, use the `--dependents` option
- `spack find [PACKAGE]` shows installed packages, optionally filtered
  by a PACKAGE name if specified
- `spack gc` uninstalls unneeded packages
- `spack mark -e PACKAGE` and `spack mark -i PACKAGE` marks a package
  as explicitly installed and implicitly installed respectively
- `spack load PACKAGE` and `spack unload PACKAGE` loads and unloads a
  given installed PACKAGE onto `$PATH` for the current shell session
- `spack extensions PACKAGE` finds extensions for a given PACKAGE,
  e.g., `spack extensions python` finds extensions for `python`
- `spack env create ENVIRONMENT` creates a given ENVIRONMENT
- `spack env activate [ENVIRONMENT]` or `spacktivate [ENVIRONMENT]`
  activates the given ENVIRONMENT (`default` if none specified)
- `spack env deactivate` of `despacktivate` deactivates the current
  environment
- `spack add PACKAGE` in an environment adds PACKAGE to the env specs
- `spack remove PACKAGE` in an environment removes PACKAGE from the
  env specs
- `spack concretize --fresh --force` in an environment tells Spack to
  use the latest version of every package in the env and forcibly
  overwrites its previous concretization
- `spack install` in an environment will build and install packages
  for its concretized spec

Common workflows (adapted from
[here](https://spack.readthedocs.io/en/latest/replace_conda_homebrew.html)):

- To set up an environment (some sampling of packages chosen below,
  change as appropriate):

  ```console
  $ spack env create myenv
  $ spack -e myenv add bash@5 python py-numpy py-scipy py-matplotlib
  ```

  To ensure only a single version of any package exists in the
  environment, it is recommended to run `spack -e myenv config edit`
  and add the following:

  ```yaml
  spack:
    # ...
    concretizer:
      unify: true
  ```

  Concretize the environment (concretization picks the optimal
  versions of each package within the specs constraints according to
  policies set for the particular Spack installation), and build and
  install the packages:

  ```console
  $ spack -e myenv concretize
  $ spack -e myenv install
  ```

- To update packages in an environment, typically after a `git pull`
  (or fetch and merge) of the Spack repository:

  ```console
  $ spack env activate myenv
  $ spack concretize --fresh --force
  $ spack install
  ```

- To clean up old packages in an environment after an update:

  ```console
  $ spack env activate myenv
  $ spack mark -i --all
  $ spack concretize --fresh --force
  $ spack install
  $ spack gc
  ```

- To remove an environment:

  ```console
  $ spack env deactivate
  $ spack env remove myenv
  ```

- To upgrade Spack (below assumes it's cloned to `~/spack`):

  Upgrade current branch, which assumes the current branch is
  `develop` (rolling) or `releases/<some_version>` (release series):

  ```console
  $ cd ~/spack
  $ git pull  # same as `git fetch && git merge origin/<branch-name>`
  ```

  Upgrade to a new release series (e.g., `releases/v0.21`):

  ```console
  $ cd ~/spack
  $ git fetch origin
  $ git branch --track releases/v0.21 origin/releases/v0.21  # optional
  $ git switch releases/v0.21
  ```

  Upgrade to a new tag (e.g., `v0.21.2`):

  ```console
  $ cd ~/spack
  $ git fetch
  $ git checkout -b v0.21.2 tag/v0.21.2  # creates a new branch v0.21.2
  ```

References:

- [Spack Docs](https://spack.readthedocs.io/en/latest/)
- [Tutorial: Spack 101](https://spack-tutorial.readthedocs.io/en/latest/)
- [Spack Packages](https://packages.spack.io/)
- [Spack Build Cache](https://cache.spack.io/)

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
- `git-lfs`
- `gnupg2`
- `gsed` (GNU [sed](https://www.gnu.org/software/sed/))
- `htop`
- `mosh`
- `pstree`
- `stow`
- `tree`

```sh
port -N install coreutils gawk git git-lfs gnupg2 gsed mosh pstree stow tree
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

If the above approaches do not work, one can:

- Delete the quarantine attribute on the app bundle or DMG file, via
  `xattr -d com.apple.quarantine /path/to/file` in the terminal; or
- Download the application, usually a DMG file with the app bundle, by
  using `curl` or `wget` in Terminal to avoid the system automatically
  setting extended attributes on the app (that is, the downloaded file
  will not have its xattr set), so it or its extracted application
  bundle should run with no Gatekeeper first run check

Note that it is recommended to verify checksums or the GPG signature
of any application downloaded, especially if this workaround is
needed.

### Extracting contents of macOS package (`.pkg`) installer files

Sometimes, it can be useful to pull out files from a `.pkg` macOS
installer. One use case is to extract out an application bundle from
an installer directly because the installer requires admin privileges
but the user does not have or does not want to give such privileges.
Another use case is to modify specific files in the installer as
desired, like the install path or some other configuration setting.

Extract `.pkg` installer files to a new (non-existing) target dir:

```console
$ pkgutil --expand-full /path/to/file.pkg /path/to/new/dir
```

Compress a folder with appropriate structure (e.g., a `.pkg` expanded
followed by modifying some files) into a `.pkg` file:

```console
$ pkgutil --flatten /path/to/folder /path/to/new/file.pkg
```

References:
- [Github gist](https://gist.github.com/ugultopu/1adf8e08acb87be649d69419cf7aca3c)
- [Stack Overflow](https://stackoverflow.com/questions/41166805/how-to-extract-contents-from-payload-file-in-a-apple-macos-update-package)

## GnuPG

### Creating a GPG key

Create new key and signing key (a encrypt subkey is automatically
generated). As of 2024-March-15, it is recommended to use the
default key type `ECC (sign and encrypt)` with elliptic curve type
`Curve 25519` (also the default).

```console
$ gpg --expert --full-gen-key
Your selection? 9     # Kind of key: ECC (sign and encrypt) *default*
Your selection? 1     # Which elliptic curve: Curve 25519 *default*
Key is valid for? 0   # key does not expire *default*
$ gpg --edit-key [NEW_KEY_FINGERPRINT]
gpg> addkey
Your selection? 10    # Kind of subkey: Signing only, ECC
Your selection? 1     # Which elliptic curve: Default (Curve 25519) is ok
Key is valid for? 0   # Does not expire, or set an expiration date if desired
gpg> save
```

Make sure to backup the keys somewhere safe (see below section
"Migrating keys", or just archive the GnuPG directory into a
tarball although in that case one may want to avoid backing
up the `openpgp-revocs.d` subdirectory), encrypting if needed
([ccrypt](https://ccrypt.sourceforge.net/) is used below):

```console
$ killall gpg-agent
$ cd path/to/folder-with-exported-keys/..                   # or `cd ~`
$ tar cf name-of-backup-file.tar folder-with-exported-keys  # or `tar cf name-of-backup-file.tar .gnupg`
$ ccrypt -e name-of-backup-file.tar                         # encrypt file
$ shasum -a256 name-of-backup-file.tar.cpt > name-of-backup-file.tar.cpt.sha256
```

(Optional) Run `gpg --edit-key [KEY_FINGERPRINT_WITHOUT_SPACES]` to
change subkey expirations to 1 year. These would have to be extended
and resynced each year. This is recommended if distributing subkeys
to many machines.

**Note**: It is recommended to keep the primary key on as few
machines as possible, and distribute (see "Migrating keys" or
"Exporting ASCII-armored keys as QR codes" below) only subkeys
to other machines (either signing or encrypt subkey, or both,
as appropriate).

### Using TTY pinentry

If using a console pinentry for entering the GnuPG password in the
terminal, it can be better to use TTY pinentry rather than ncurses
pinentry. Configure this as follows:

- The default `pinentry` may be symlinked to `pinentry-ncurses`
  which has issues being run from other ncurses programs like Vim.
  Change the symlink `pinentry` so it points to `pinentry-tty`. For example,
  suppose if using `pinentry` from MacPorts installed to `$HOME/macports`:

  ```sh
  cd $HOME/macports/bin
  rm pinentry && ln -s pinentry-tty pinentry
  ```

  Or just set the pinentry program used in `$HOME/.gnupg/gpg-agent.conf`:

  ```conf
  pinentry-program /path/to/pinentry-tty
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

Below, replace `[EMAIL...]` or `[KEY_FINGERPRINT_WITHOUT_SPACES]` as
appropriate. Fingerprints (mainly needed if there are multiple keys
for the same email) can be listed running `gpg -k` in the terminal.

Export keys and ownertrust (from the old machine to migrate from):

```console
$ gpg --export --armor [EMAIL_OR_KEY_FINGERPRINT_WITHOUT_SPACES] > [EMAIL_OR_OTHER_ID].pub.asc
$ gpg --export-secret-keys --armor [EMAIL_OR_KEY_FINGERPRINT_WITHOUT_SPACES] > [EMAIL_OR_OTHER_ID].priv.asc
$ gpg --export-secret-subkeys --armor [EMAIL_OR_KEY_FINGERPRINT_WITHOUT_SPACES] > [EMAIL_OR_OTHER_ID].sub_priv.asc
$ gpg --export-ownertrust > ownertrust.txt
```

Import keys and ownertrust (on new machine to migrate to):

If you need to restore your keys and trust level (e.g., after reinstalling the system or on a new computer), use the following commands:

```console
$ gpg --import [EMAIL_OR_OTHER_ID].pub.asc
$ gpg --import [EMAIL_OR_OTHER_ID].priv.asc
$ gpg --import [EMAIL_OR_OTHER_ID].sub_priv.asc
$ gpg --import-ownertrust ownertrust.txt
$ gpg --edit-key [EMAIL_OR_KEY_FINGERPRINT_WITHOUT_SPACES]
gpg> trust
Your decision? 5
```

**Alternative**: Copy over the GnuPG data directory (this preserves
all keys and the the trust database) which is `C:/Documents
and Settings/Username/Application Data/GnuPG` on Windows systems
(replace `Username` as appropriate) or `$HOME/.gnupg` on Linux/macOS
systems (Linux/macOS instructions shown), but don't include the
`openpgp-revocs.d` subdirectory and kill `gpg-agent` first to close
any open socket files.

References:

- [How to backup GPG](https://serverfault.com/a/1040984)
- [Moving/Copying your PGP Keys](https://www.phildev.net/pgp/gpg_moving_keys.html)

### Exporting ASCII-armored keys as QR codes

(Requires `qrencode` be installed on the system.)

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
gpg --export --armor <EMAIL_OR_SUBKEY_FINGERPRINT_WITHOUT_SPACES> > pub.key
split -C 2500 pub.key pubkey-
for file in pubkey-??; do <"$file" qrencode -s 3 -d 150 -o "$file".qr; done
# export secret subkey to subkey-??.qr QR code image files
gpg --export-secret-subkeys --armor <SUBKEY_FINGERPRINT_WITHOUT_SPACES> > sub.key
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

## [Gopass](https://www.gopass.pw/)

This section covers setup and other
non-routine tasks for gopass. Also see
[here](https://github.com/gopasspw/gopass/blob/master/docs/features.md).

### Setup

After [installing]() gopass, iniitialize a password store and add
completion to the shell (if `--path ...` is not provided, the store
path defaults to `~/.local/share/gopass/stores/root`):

```console
$ gopass init --path ~/.password-store [GPG_KEY_EMAIL_OR_KEY_ID_OR_FINGERPRINT_WITHOUT_SPACES]
$ echo "source <(gopass completion zsh)" >> ~/.zshrc
```

Configure git:

```console
$ gopass git remote add origin [repo-url]
$ gopass git config set user.name [name of user]
$ gopass git config set user.email [email of user]
$ gopass git config commit.gpgsign true
$ gopass git config user.signingKey <SIGN SUBKEY FINGERPRINT>\!
```

### Pass/gopass entry format

Entries can be edited using `gopass edit [some-entry]`:

- The first line of the encrypted file for each entry contains the password.
- Other lines should be preceded by an identifier for what that line contains.
- Use `Username: [username]` to indicate a user name.
- Others can be ad hoc and descriptive, examples:
  - `Secret question: [secret qn] [answer]`
  - `Last renewed: [last renewal date]`

### Extensions and mobile apps

- [gopassbridge](https://github.com/gopasspw/gopassbridge):
  Browser extension
- [Pass - Password Store](https://github.com/mssun/passforios)
  ([Appstore](https://apps.apple.com/us/app/pass-password-store/id1205820573)):
  iOS app, only supports 1 repository
- [Password Store](https://github.com/android-password-store/android-password-store)
  ([Play Store](https://play.google.com/store/apps/details?id=dev.msfjarvis.aps)):
  Android app, only supports 1 repository

### Rotate GPG key for the password store

Create a new GPG key:

```console
$ gpg --expert --full-gen-key
$ gpg -k           # Note the new key's fingerprint
$ gpg --edit-key [NEW_KEY_FINGERPRINT]
gpg> addkey
Your selection? 10    # Kind of subkey: Signing only, ECC
Your selection? 1     # Which elliptic curve: Default (Curve 25519) is ok
Key is valid for? 0   # Does not expire, or set an expiration date if desired
gpg> save
$ gpg -k --with-subkey-fingerprints
/path/to/pubring.kbx
-----------------------------
pub   rsa4096/XXXXXXXXXXXXXXXX 2020-01-01 [SC]
      Key fingerprint = YYYY YYYY YYYY YYYY YYYY  YYYY YYYY YYYY YYYY YYYY
uid                 [ultimate] Some Name <user@domain.com>
sub   rsa4096/UUUUUUUUUUUUUUUU 2020-01-01 [E]
      Key fingerprint = VVVV VVVV VVVV VVVV VVVV  VVVV VVVV VVVV VVVV VVVV
sub   rsa4096/PPPPPPPPPPPPPPPP 2020-01-01 [S]
      Key fingerprint = QQQQ QQQQ QQQQ QQQQ QQQQ  QQQQ QQQQ QQQQ QQQQ QQQQ

pub   ed25519/A000000000000000 2023-12-31 [SC]
      Key fingerprint = AAAA AAAA AAAA AAAA AAAA  AAAA AAAA AAAA AAAA AAAA
uid                 [ultimate] Some Name <user@domain.com>
sub   cv25519/B111111111111111 2023-12-31 [E]
      Key fingerprint = BBBB BBBB BBBB BBBB BBBB  BBBB BBBB BBBB BBBB BBBB
sub   ed25519/C222222222222222 2023-12-31 [S]
      Key fingerprint = CCCC CCCC CCCC CCCC CCCC  CCCC CCCC CCCC CCCC CCCC
```

Note *sub* key (the one with `[E]`) fingerprint for the old and new
keys (`VVVV VVVV ...` and `BBBB BBBB ...` respectively in the example
output above), and the subkey id for the new key's signing subkey
(the one with `[S]`, `C222...` in the example output above).

If the old key was the default one, update it by adding the following
line to `$HOME/.gnupg/gpg.conf` or modifying the appropriate line
if one already exists, using the new primary key's fingerprint
without spaces (in the above example, this would be `AAAAAAAA...`):

```conf
default-key [NEW_KEY_FINGERPRINT_WITHOUT_SPACES]
```

Add new encrypt subkey and remove the old encrypt subkey to and
from the configured gopass recipients:

```console
$ cd /path/to/gopass/password/store/
$ gopass recipients add [NEW_SUBKEY_FINGERPRINT_WITHOUT_SPACES]
$ gopass recipients remove [OLD_SUBKEY_FINGERPRINT_WITHOUT_SPACES]
```

Update the signing subkey (the one with `[S]` for the new key, e.g.,
`C222...` in the above example):

```console
$ gopass git configure git config user.signingkey [NEW_SIGNING_SUBKEY_ID]\!
```

Export the signing subkey public key to the remote Git forge. E.g.,
for Gitlab go to user preferences to add a new GPG key by
copying over the text block output by run `gpg --armor --export
[NEW_SIGNING_SUBKEY_ID]` (instructions may differ by forge).

Immediately continuing from above, it is recommended to remove
all Git history so passwords encrypted with the old key cannot be
accessed by looking at an old commit (following assumes the
main branch name is `master` and the remote repository settings
allows force push to that branch):

```console
$ git checkout --orphan latest_branch
$ git add -A
$ git commit -am 'Initial commit after changing GPG key'
$ git branch -m master
$ git push -f origin master
```

**Note**: If there are files on the system (e.g., a
`$HOME/.netrc.gpg` file) that are encrypted with the old GPG
key, re-encrypt them (use `--recipient [NEW_KEY_ID]` instead of
`--default-recipient-self` if the new key won't be the default one):

```console
$ gpg --output somefile --decrypt somefile.gpg
$ rm -f somefile.gpg
$ gpg --output somefile.gpg --encrypt --default-recipient-self [NEW_KEY_ID]
```

Adapted from
[here](https://daryl.wakatara.com/migrate-to-ecc-encryption-keys/)
and [here](https://stackoverflow.com/a/26000395).

Other references:
- [Git Tools - Signing Your Work](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work)

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

(`git switch` can also be used, run `man git-switch` for its manpage.)

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
(basically `git rm --cache` to remove the old filename from the index
and `git add` the file with the new name):

```sh
mv a.txt b.txt         # oops, meant to "git mv" but did "mv" instead
git rm --cached a.txt  # fix pt 1: remove old file from index
git add b.txt          # fix pt 2: add new file
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

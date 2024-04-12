# Font notes

## Installing fonts

### macOS

Use the [Font Book](https://support.apple.com/guide/font-book/welcome/mac) app.

### Linux (including Crostini on Chrome OS)

```sh
mkdir -p $HOME/.fonts
cp SOMEFONT.ttf ANOTHERFONT.ttc $HOME/.fonts/
fc-cache -fv
```

### Bitmap fonts

Bitmap fonts, if distributed in TTF or OTF or OTB form, should be used
without antialiasing and at their intended sizes (or multiples of it).

### Variable fonts (VF)

Variable fonts are font files that contain a range
of variants (e.g., by weight, width, slant, etc) in a
single file ([more](https://web.dev/variable-fonts/)
[info](https://en.wikipedia.org/wiki/Variable_font)).

### Freezing font features

For software that don't support toggling of OpenType features, one way
to use those features is to "freeze" those features into a new font,
making them the default. Some tooling to help do that:

- [pyftfeatfreeze](https://twardoch.github.io/fonttools-opentype-feature-freezer/)
  ([Github](https://github.com/twardoch/fonttools-opentype-feature-freezer))
  (there's a GUI app OTFeatureFreezer, but the Python command-line
  tool basically works the same; also see
  [here](https://fsd.it/2024/02/16/now-you-can-customize-your-pragmata-pro-font/))
- [FontFreeze](https://mutsuntsai.github.io/fontfreeze/)
  ([Github](https://github.com/MuTsunTsai/fontfreeze)) webapp

## Font managers

- [Font Book](https://support.apple.com/guide/font-book/welcome/mac)
  (macOS)
- [FontBase](https://fontba.se/)
- [Typeface](https://typefaceapp.com/) (_commercial_)

## Font tools

- [Bits'N'Picas](https://github.com/kreativekorp/bitsnpicas):
  Create and convert bitmap and emoji font
- [bdf2x](https://github.com/Francesco149/bdf2x):
  Upscale BDF bitmap fonts
- [Calligraphr](https://www.calligraphr.com/en/):
  Turn handwriting into fonts (_commercial_)
- [FontDrop!](https://fontdrop.info/):
  Online webapp for inspecting font files
- [FontForge](https://fontforge.org/)
  ([Github](https://github.com/fontforge/fontforge)):
  Font editor that can create, convert and display fonts
- [fontjoy](https://fontjoy.com/)
  ([Github](https://github.com/Jack000/fontjoy)):
  Font pairings using deep learning
- [fontTools](https://github.com/fonttools/fonttools):
  Font manipulation libraries and utilities
- [FontoGen](https://github.com/SerCeMan/fontogen):
  Generative AI-created fonts
  ([article](https://serce.me/posts/02-10-2023-hey-computer-make-me-a-font))
- [Fontra](https://fontra.xyz/)
  ([Github](https://github.com/googlefonts/fontra)):
  Web-based font editor, also available for the
  [desktop](https://github.com/googlefonts/fontra-pak)
- [Glyphr Studio](https://www.glyphrstudio.com/)
  ([Github](https://github.com/glyphr-studio)):
  Web-based font editor like FontForge, also available for the
  [desktop](https://github.com/glyphr-studio/Glyphr-Studio-Desktop)
- [Glyphs](https://glyphsapp.com/):
  Font editor (_commercial_)
- [Google Font Classifier](https://github.com/Storia-AI/font-classify):
  Finds the Google Font that most closely matches text in an image
- [hexagone](https://github.com/nolenroyalty/hexagone):
  Modify font so it converts hex codes to RGB
  ([article](https://eieio.games/nonsense/hexagone-converting-hex-to-rgb-with-a-font/))
- [LCDF Typetools](https://www.lcdf.org/type/)
  ([Github](https://github.com/kohler/lcdf-typetools)):
  Font manipulation utilities, see `software.md`
- [metaflop](https://www.metaflop.com/)
  ([Github](https://github.com/metaflop)):
  Modulate/customize fonts within given parameters to generate range
  of font families
- [monobit](https://github.com/robhagemans/monobit):
  Modify and convert bitmap fonts
- [numderline](https://thume.ca/numderline/)
  ([Github](https://github.com/trishume/numderline)):
  Modify fonts so alternating groups of three digits are underlined,
  making it easier to distinguish thousand/thousand-th parts
  ([article](https://blog.janestreet.com/commas-in-big-numbers-everywhere/))

## Monospace fonts

- [Agave](https://b.agaric.net/page/agave)
  ([Github](https://github.com/blobject/agave))
- [Ark Pixel Font](https://github.com/TakWolf/ark-pixel-font)
  (bitmap, CJK)
- [BQN386](https://dzaima.github.io/BQN386/)
  ([Github](https://github.com/dzaima/BQN386))
  (designed for use with [BQN](https://mlochbaum.github.io/BQN/),
  derived from [APL385](https://www.apl385.com/fonts/) and
  [APL386](https://abrudz.github.io/APL386/), alternative is
  [Uiua386](https://www.uiua.org/install) designed for use with
  [Uiua](https://www.uiua.org/) downloadable
  [here](https://github.com/uiua-lang/uiua/raw/main/site/Uiua386.ttf))
- [Berkeley Mono](https://berkeleygraphics.com/typefaces/berkeley-mono/)
  (_commercial_)
- [Comic Code](https://tosche.net/fonts/comic-code) (_commercial_,
  open-source alts [one](https://dtinth.github.io/comic-mono-font/),
  [two](https://github.com/jesusmgg/comic-shanns-mono) and
  [three](https://github.com/kaBeech/serious-sans))
- [Commit Mono](https://commitmono.com/)
  ([Github](https://github.com/eigilnikolajsen/commit-mono),
  VF[_ital_,**wght**])
- [Cozette](https://github.com/slavfox/Cozette) (bitmap)
- [Ellograph](https://connary.com/ellograph.html) (_commercial_)
- [Everson Mono](https://www.evertype.com/emono/) (_shareware_)
- [Fairfax](http://www.kreativekorp.com/software/fonts/fairfax/) (bitmap)
  and [Fairfax HD](http://www.kreativekorp.com/software/fonts/fairfaxhd/)
  (vector) ([Github](https://github.com/kreativekorp/open-relay))
- [Fantasque Sans Mono](https://github.com/belluzj/fantasque-sans)
- [Galmuri](https://github.com/quiple/galmuri) (bitmap, CJK)
- [IBM3161](https://github.com/wyatt8740/IBM3161-font)
  ([GitLab](https://gitlab.com/wyatt8740/IBM3161-font), bitmap)
- [IBM fonts](https://github.com/farsil/ibmfonts)
  (bitmap)
- [Iosveka](https://github.com/be5invis/Iosevka)
  ([CJK variant](https://github.com/be5invis/Sarasa-Gothic))
  (on macOS, install individual TTF files rather than the super TTC
  file, see [here](https://github.com/be5invis/Iosevka/issues/1377),
  [derivative](https://github.com/shytikov/pragmasevka) to mimic
  PragmataPro more closely than the SS08 variant)
- [JetBrains Mono](https://github.com/JetBrains/JetBrainsMono)
  (VF[**wght**])
- [Julia Mono](https://juliamono.netlify.app/)
  ([Github](https://github.com/cormullion/juliamono),
  good Unicode coverage)
- [Maple Mono](https://github.com/subframe7536/Maple-font)
- [Monaspace](https://monaspace.githubnext.com/)
  ([Github](https://github.com/githubnext/monaspace))
  (superfamily of five type categories,
  VF[_slnt_,wdth,**weight**])
- [Monocraft](https://github.com/IdreesInc/Monocraft)
  (bitmap, emulates Minecraft typeface, proportional
  [version](https://github.com/IdreesInc/Minecraft-Font),
  vector-y [version](https://github.com/IdreesInc/Miracode))
- [OCR A](https://sourceforge.net/projects/ocr-a-font/)
  (the TTF file does work correctly on macOS so for that system
  install the PFA file instead, see next font for alternative)
- [OCR B](https://tsukurimashou.org/ocr.php.en)
  (also has OCR A with larger glyph set than previous font, alternate
  [download site](https://aur.archlinux.org/packages/ocr-fonts); other
  implementations are [CTAN](https://ctan.org/pkg/ocr-b-outline) and a
  constant stroke width
  [variant](https://web.archive.org/web/20190328165040/https://wehtt.am/ocr-b/);
  there is also a variant standard covering Japanese Katakana,
  [OCR-BK](https://force4u.cocolog-nifty.com/skywalker/2010/07/ocrocr-bkw-9d5e.html))
- [Pixel Code](https://qwerasd205.github.io/PixelCode/examples/specimen.html)
  ([Github](https://github.com/qwerasd205/PixelCode))
- [PragmataPro](https://fsd.it/shop/fonts/pragmatapro/) (_commercial_,
  good Unicode coverage)
- [Proggy](https://github.com/bluescan/proggyfonts) fonts
- [Space Mono](https://www.colophon-foundry.org/custom-projects/space-mono)
  ([Github](https://github.com/googlefonts/spacemono))
- [Spleen](https://github.com/fcambus/spleen) (bitmap)
- [Tamsyn](http://www.fial.com/~scott/tamsyn-font/) (bitmap)
- [Tektite](https://www.chiark.greenend.org.uk/~sgtatham/fonts/)
  (bitmap, also see
  [here](https://unix.stackexchange.com/questions/226593/where-can-i-find-a-modern-version-of-the-rock-or-t-console-typeface))
- [Terminus](https://terminus-font.sourceforge.net/)
  (bitmap, [TTF version](https://files.ax86.net/terminus-ttf/))
- [Unifont](http://unifoundry.com/unifont/index.html)
  (bitmap, extremely good Unicode coverage; there is a fork
  [UnifontEX](https://github.com/stgiga/UnifontEX) that merges in
  Unifont Upper that has glyphs above the basic multilingual plane)
- [Unscii](http://viznut.fi/unscii/) (bitmap,
  [Github](https://github.com/viznut/unscii),
  [funscii](https://github.com/asiekierka/funscii) fork with JP
  support and bugfixes)
- [UW ttyp0](https://people.mpi-inf.mpg.de/~uwe/misc/uw-ttyp0/)
  (bitmap, vector port
  [Greybeard](https://github.com/flowchartsman/greybeard))
- [Zpix](https://github.com/SolidZORO/zpix-pixel-font) (bitmap, CJK,
  _free for personal use_)

## Sans-serif proportional fonts

- [Andika](https://software.sil.org/andika/)
  ([Github](https://github.com/silnrsi/font-andika))
- [Atkinson Hyperlegible](https://brailleinstitute.org/freefont)
  ([extended version](https://github.com/jacobxperez/atkinson-hyperlegible-pro))
- [Avenir](https://en.wikipedia.org/wiki/Avenir_(typeface))
  and its variants (included with macOS)
- Brandon [Grotesque](https://www.hvdfonts.com/fonts/brandon-grotesque)
  and [Text](https://www.hvdfonts.com/fonts/brandon-text)
  (condensed versions also available, _commercial_)
- [Canada1500](https://typodermicfonts.com/canada1500/)
  ([Github copy with OFL license](https://github.com/inferno986return/Canada1500))
- [CERN-www](https://github.com/djrrb/CERN-www-fonts)
  ([WorldWideWeb](https://worldwideweb.cern.ch/) browser font clone)
- [Clarity City](https://github.com/vmware/clarity-city)
  (similar to Gotham but with stricter geometry)
- [Clear Sans](https://github.com/intel/clear-sans)
- [Climate Crisis](https://design.google/library/climate-crisis)
  ([Github](https://github.com/dancoull/ClimateCrisis))
  (VF[YEAR])
- [Comic Neue](https://comicneue.com/)
  ([Github](https://github.com/crozynski/comicneue),
  marker-style font mimicking Comic Sans, has angular variant)
- [Cooper Hewitt](https://www.cooperhewitt.org/open-source-at-cooper-hewitt/cooper-hewitt-the-typeface-by-chester-jenkins/)
  ([Github](https://github.com/cooperhewitt/cooperhewitt-typeface))
- [Dungeon Chunk](https://github.com/PriorityInterrupt/dungeon-chunk)
- [Figtree](https://github.com/erikdkennedy/figtree)
  (VF[**wght**])
- [Finlandica](https://github.com/HelsinkiTypeStudio/Finlandica)
  (VF[**wght**])
- [FiraGO](https://bboxtype.com/typefaces/FiraGO/)
  ([Github](https://github.com/bBoxType/FiraGO))
- [Gabarito](https://github.com/naipefoundry/gabarito)
  (VF[**wght**])
- [Georama](https://github.com/productiontype/Georama)
  (VF[wdth,**wght**])
- [Inter](https://github.com/rsms/inter)
  (Helvetica alternative, VF[_ital_,opsz,**wght**])
- [Jost*](https://github.com/indestructible-type/Jost)
  (Futura alternative, VF[_ital_,**wght**])
- [Lato](https://github.com/googlefonts/LatoGFVersion)
  ([source](https://github.com/latofonts/lato-source))
- [Luohei Variable](https://atelier-anchor.com/luohei-variable)
  ([Github](https://github.com/atelier-anchor/luohei-variable))
  (CJK, VF[xwgt,ywgt])
- [National Park](https://nationalparktypeface.com/)
  ([Github](https://github.com/benhoepner/National-Park))
  (VF[**wght**])
- [Neutraface](https://housefonts.com/hi/neutraface)
  (_commercial_)
- [Northrup](https://github.com/mirnovov/northrup)
- [Open Sans](https://github.com/googlefonts/opensans)
  (VF[wdth,**wght**])
- [Outfit](https://github.com/Outfitio/Outfit-Fonts)
  (VF[**wght**])
- [Pixelify Sans](https://github.com/eifetx/Pixelify-Sans)
- [Pixellari](https://github.com/zedseven/Pixellari)
- [Poppins](https://www.indiantypefoundry.com/fonts/poppins)
  ([Github](https://github.com/itfoundry/Poppins), geometric)
- [Plein](https://www.fontshare.com/fonts/plein)
- [Plus Jakarta Sans](https://github.com/tokotype/PlusJakartaSans)
  (VF[**wght**])
- Proxima [Nova](https://www.marksimonson.com/fonts/view/proxima-nova)
  and [Vara](https://www.marksimonson.com/fonts/view/proxima-vara)
  (_commercial_)
- [Proza Libre](https://bureauroffa.com/about-proza-libre)
  ([Github](https://github.com/jasperdewaard/Proza-Libre))
- [Public Sans](https://public-sans.digital.gov/)
  ([Github](https://github.com/uswds/public-sans), VF[**wght**])
- [Radio-Canada](https://github.com/cbcrc/radiocanadafonts)
  (VF[wdth,**wght**])
- [Raleway](https://github.com/theleagueof/raleway)
  (VF[**wght**])
- [Readex Pro](https://github.com/ThomasJockin/readexpro) (expanded
  [Lexend](https://github.com/googlefonts/lexend),
  VF[HEXP,**wght**])
- [Rocher Color](https://www.harbortype.com/fonts/rocher-color/)
  ([article](https://www.harbortype.com/rocher-color-making-a-variable-color-font/),
  VF[BVEL,SHDW])
- [Schibsted Grotesk](https://github.com/schibsted/schibsted-grotesk)
  (VF[**wght**])
- [Shantell Sans](https://github.com/arrowtype/shantell-sans)
  (marker-style font like Comic Sans and Inkwell Sans,
  VF[BNCE,INFM,SPAC,_ital_,**wght**])
- [Smiley Sans](https://atelier-anchor.com/typefaces/smiley-sans)
  ([Github](https://github.com/atelier-anchor/smiley-sans))
  (CJK)
- [SN Pro](https://supernotes.app/open-source/sn-pro/)
  ([Github](https://github.com/supernotes/sn-pro))
  (VF[_slnt_,**wght**])
- [Space Grotesk](https://github.com/floriankarsten/space-grotesk)
  (VF[**wght**])
- [TASA Typeface Collection](https://tasatype.localremote.co/)
  ([Github](https://github.com/adrianzwz/TASA-Typeface-Collection),
  VF[**wght**])
- [Teachers](https://github.com/chankfonts/Teachers-fonts)
  (VF[**wght**])
- [Unbounded](https://unbounded.polkadot.network/)
  ([Github](https://github.com/w3f/unbounded))
  (VF[**wght**])
- [Urbanist](https://github.com/coreyhu/Urbanist)
  (VF[_ital_,**wght**])
- [Wix Madefor](https://github.com/wix-incubator/wixmadefor)
  (VF[_ital_,**wght**])
- [Womprat](https://crown.fontdue.com/fonts/womprat) (Star Wars-y,
  _commercial_)
- [Work Sans](http://weiweihuanghuang.github.io/Work-Sans/)
  ([Github](https://github.com/weiweihuanghuang/Work-Sans),
  VF[**wght**])
- [Ysabeau](https://github.com/CatharsisFonts/Ysabeau)
  (VF[**wght**])

## Serif proportional fonts

- [Amstelvar](https://github.com/googlefonts/amstelvar)
  (VF[GRAD,YOPQ,YTAS,YTDE,YTFI,YTLC,YTUC,opsz,wdth,**wght**])
- [Besley](https://indestructibletype.com/Besley.html)
  ([Github](https://github.com/indestructible-type/Besley),
  VF[wdth,**wght**])
- [Bodoni Moda](http://indestructible-type.github.io/Bodoni.html)
  ([Github](https://github.com/indestructible-type/Bodoni),
  VF[opsz,**wght**])
- [Bona Nova](https://github.com/kosmynkab/Bona-Nova)
- [Cardo](https://github.com/googlefonts/CardoFont)
- [Courier Prime](https://quoteunquoteapps.com/courierprime/)
  [Regular](https://github.com/quoteunquoteapps/CourierPrime),
  [Sans](https://github.com/quoteunquoteapps/CourierPrimeSans),
  and [Code](https://github.com/quoteunquoteapps/CourierPrimeCode)
- [Domaine](https://klim.co.nz/collections/domaine/) (_commercial_)
- [EB Garamond](https://github.com/georgd/EB-Garamond)
- [Elstob](https://psb1558.github.io/Elstob-font/)
  ([Github](https://github.com/psb1558/Elstob-font))
  (VF[GRAD,SPAC,_slnt_,opsz,**wght**])
- [ETbb](https://tug.org/FontCatalogue/etbb/)
  ([CTAN](https://ctan.org/pkg/etbb?lang=en))
- Grenze [Regular](https://www.omnibus-type.com/fonts/grenze/)
  ([Github](https://github.com/Omnibus-Type/Grenze)), and
  [Gotisch](https://www.omnibus-type.com/fonts/grenze-gotisch/)
  ([Github](https://github.com/Omnibus-Type/Grenze-Gotisch))
  (VF[**wght**])
- [IM Fell](https://iginomarini.com/fell/the-revival-fonts/) fonts
  ([Google Fonts](https://fonts.google.com/?query=IM+Fell),
  [more](https://iginomarini.com/fell/history/)
  [info](https://www.linyangchen.com/Typography-Fell-Types-font))
- [Junicode](https://github.com/psb1558/Junicode-font)
  (VF[ENLA,wdth,**wght**])
- [Mate](https://github.com/etunni/mate)
- [Playfair](https://github.com/googlefonts/Playfair)
  (VF[opsz,wdth,**wght**])
- [Reforma](https://pampatype.com/reforma)
- [Shakespeare Serif](https://github.com/edent/Shakespeare-Serif-Font)
  (font based on scans of the
  [First Folio](https://en.wikipedia.org/wiki/First_Folio),
  [article](https://shkspr.mobi/blog/2023/07/shakespeare-serif-a-new-font-based-on-the-first-folio/))
- [Spectral](https://github.com/productiontype/spectral)
- [Sprat](https://github.com/EthanNakache/Sprat-type)
  (VF[wdth,**wght**])
- [Wittgenstein](https://github.com/jrgdrs/Wittgenstein)
  (VF[**wght**])

## Symbol fonts

- [Asap Symbol](https://www.omnibus-type.com/fonts/asap-symbol/)
  ([Github](https://github.com/Omnibus-Type/Asap-Symbol))
- [Catrinity](https://catrinity-font.de/) (sans-serif)
- [Chartwell](https://www.vectrotype.com/chartwell) (_commercial_,
  font to render charts using OpenType)
- [DSEG](https://www.keshikan.net/fonts-e.html)
  ([Github](https://github.com/keshikan/DSEG)) (LCD display-like)
- [Eva Icons](https://akveo.github.io/eva-icons/)
  ([Github](https://github.com/akveo/eva-icons))
  (Webfont)
- [Quivira](http://www.quivira-font.com/) (serif)
- [Redacted](https://github.com/christiannaths/redacted-font)
  (for keeping wireframes and prototypes free of distracting text,
  VF[**wght**])
- [Signals](https://significa.co/blog/signals-by-significa)
- [Siji-ng](https://github.com/mxkrsv/siji-ng) (bitmap)
- [Symbola](https://dn-works.com/ufas/)
  (_free for personal use_, may need to email author for font but
  earlier versions are distributed as attachments in PDF files
  [downloadable](https://aur.archlinux.org/packages/ttf-symbola#comment-883110)
  from the author's site that can be extracted using
  [pdfcpu](https://github.com/pdfcpu/pdfcpu) or `pdfdetach` from
  [poppler](https://poppler.freedesktop.org/))

## Script fonts

- [Ballet](https://github.com/Omnibus-Type/Ballet)
  (VF[opsz])
- [Borel](https://github.com/RosaWagner/Borel)

## Font families

- [Avería](http://iotic.com/averia/)
  ([Google Fonts](https://fonts.google.com/?query=averia))
  (font created by averaging other fonts
  [VF](https://github.com/eliheuer/averia-libre-vf)[**wght**])
- [Bagnard](https://github.com/sebsan/Bagnard) and
  [Bagnard Sans](https://github.com/sebsan/Bagnard-Sans)
- [DM](https://github.com/googlefonts/dm-fonts) (sans
  like Poppins, serif like Source Serif Pro, has monospace
  [version](https://github.com/googlefonts/dm-mono) based on
  the sans font, VF[opsz,**wght**])
- [Geist](https://vercel.com/font) Sans and Mono
  ([Github](https://github.com/vercel/geist-font), VF[**wght**])
- [Go](https://go.dev/blog/go-fonts)
- [IBM Plex](https://github.com/IBM/plex)
  (VF[wdth,**wght**])
- [Input](https://input.djr.com/) (_free for personal use_)
- Instrument [Sans](https://github.com/Instrument/instrument-sans)
  (VF[wdth,**wght**]) and
  [Serif](https://github.com/Instrument/instrument-serif)
- Josefin [Sans](https://github.com/googlefonts/josefinsans) and
  [Slab](https://github.com/davelab6/josefinslab)
  (both VF[**wght**])
- [Kurinto Font Folio](https://www.kurinto.com/)
- Lucida ([B&H](https://lucidafonts.com/),
  [TUG](https://tug.org/store/lucida/index.html)) (_commercial_;
  some versions licensed and distributed with specific platforms,
  e.g., _Lucida Sans Unicode_ on Windows, _Lucida Grande_ on macOS)
  ([detail](https://bigelowandholmes.typepad.com/bigelow-holmes/how-and-why-lucida/))
- [Luxi](https://en.wikipedia.org/wiki/Luxi_fonts) Sans, Serif
  and Mono (distributed as `font-bh-ttf-<version>.tar.<gz|xz>`
  [here](https://xorg.freedesktop.org/releases/individual/font/)),
  Luxi Sans and Luxi Mono are similar to Go and Go Mono
- [M+](https://mplusfonts.github.io/)
  ([Github](https://github.com/coz-m/MPLUS_FONTS))
- [New Computer Modern](https://tug.org/FontCatalogue/newcomputermodernroman/)
  ([CTAN](https://ctan.org/pkg/newcomputermodern?lang=en))
- [Noto](https://notofonts.github.io/)
  ([Docs](https://notofonts.github.io/noto-docs/),
  [Github](https://github.com/notofonts))
  fonts, primarily sans-serif proportional with some monospace,
  that together aim to cover all scripts in the Unicode standard
  (some VF[**wght**], some VF[wdth,**wght**])
- [Open Sauce](https://github.com/marcologous/Open-Sauce-Fonts)
- [Recursive](https://github.com/arrowtype/recursive)
  (VF[CASL,CRSV,MONO,_slnt_,**wght**])
- [Red Hat](https://github.com/RedHatOfficial/RedHatFont)
  (VF[**wght**])
- [Roboto](https://fonts.google.com/specimen/Roboto),
  [Condensed](https://fonts.google.com/specimen/Roboto+Condensed),
  [Serif](https://fonts.google.com/specimen/Roboto+Serif),
  [Slab](https://fonts.google.com/specimen/Roboto+Slab),
  [Mono](https://fonts.google.com/specimen/Roboto+Mono),
  [Flex](https://github.com/googlefonts/roboto-flex)
  (VF[GRAD,XOPQ,XTRA,YOPQ,YTAS,YTDE,YTFI,YTLC,YTUC,opsz,_slnt_,wdth,**wght**])
- Spline [Sans](https://github.com/SorkinType/SplineSans),
  [Sans Mono](https://github.com/SorkinType/SplineSansMono)
  (both VF[**wght**])
- Source [Sans](https://github.com/adobe-fonts/source-sans),
  [Serif](https://github.com/adobe-fonts/source-serif),
  [Code Pro](https://github.com/adobe-fonts/source-code-pro)
  (some VF[**wght**], some VF[opsz,**wght**]; CJK
  [Sans](https://github.com/adobe-fonts/source-han-sans),
  [Serif](https://github.com/adobe-fonts/source-han-serif),
  [Mono](https://github.com/adobe-fonts/source-han-mono) variants)

## Links

- [Beautiful Web Type](https://beautifulwebtype.com/)
- [Collletttivo](https://www.collletttivo.it/)
- [Colophon Foundry](https://www.colophon-foundry.org/) (_commercial_)
- [DamienG's typefaces](https://damieng.com/typography/)
- [Fonts In Use](https://fontsinuse.com/)
- [Google Font Updates](https://twitter.com/googlefonts?lang=us)
- [Google Fonts Knowledge](https://fonts.google.com/knowledge)
- [hoard-of-bitfonts](https://github.com/robhagemans/hoard-of-bitfonts)
- [KreativeKorp Relay Fonts](http://www.kreativekorp.com/software/fonts/index.shtml)
- [monospace-font-list](https://github.com/dse/monospace-font-list)
- [Omnibus-Type](https://www.omnibus-type.com/)
- [On snot and fonts](http://luc.devroye.org/fonts.html)
- [Open Foundry](https://open-foundry.com/)
- [Pangram Pangram](https://pangrampangram.com/) (_commercial_)
- [Programming Fonts](https://www.programmingfonts.org/)
  ([Tumblr](https://programmingfonts.tumblr.com/))
- [Programming Fonts](https://github.com/ProgrammingFonts/ProgrammingFonts)
  (parallel project to the above)
- [SIL Language Technology - Fonts](https://software.sil.org/fonts/)
  ([Github](https://github.com/silnrsi))
- [The LaTeX Font Catalogue](https://tug.org/FontCatalogue/)
- [The League of Movable Type](https://www.theleagueofmoveabletype.com/)
  ([Github](https://github.com/theleagueof))
- [The Ultimate Oldschool PC Font Pack](https://int10h.org/oldschool-pc-fonts/)
- [Tunera Type Foundry](https://www.tunera.xyz/)
- [Typeface Trivia](https://github.com/subidit/typophilia) - list of fonts and font trivia
- [Typewolf](https://www.typewolf.com/)
- [Typographica](https://typographica.org/)
  ([Library](https://library.typographica.org/))
- [Typography: Google Fonts Combinations](https://www.behance.net/gallery/35768979/Typography-Google-Fonts-Combinations)
- [U8g2 fonts](https://github.com/olikraus/u8g2/tree/master/tools/font/bdf)
  ([preview](https://github.com/olikraus/u8g2/wiki/fntgrp))
- [Variable fonts](https://variablefonts.typenetwork.com/) - Type Network
- [Variable fonts](https://v-fonts.com/) - A simple resource for finding and trying variable fonts
- [Velvetyne Type Foundry](https://velvetyne.fr/)
  ([Gitlab](https://gitlab.com/velvetyne))

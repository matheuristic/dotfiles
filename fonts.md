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

Bitmap fonts, if distributed in TTF or OTF form, should be used
without antialiasing and at their intended sizes (or multiples of it).

### Freezing font features

For software that don't support toggling of OpenType features, one way
to use those features is to "freeze" those features into a new font,
making them the default. Some tooling to help do that:

- [pyftfeatfreeze](https://github.com/twardoch/fonttools-opentype-feature-freezer)
  (also has a GUI app OTFeatureFreeze, but the Python command-line
  tool basically works the same)
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
- [FontForge](https://fontforge.org/)
  ([Github](https://github.com/fontforge/fontforge)):
  Font editor that can create, convert and display fonts
- [LCDF Typetools](https://www.lcdf.org/type/)
  ([Github](https://github.com/kohler/lcdf-typetools)):
  Font manipulation utilities, see `software.md`
- [monobit](https://github.com/robhagemans/monobit):
  Modify and convert bitmap fonts

## Monospace fonts

- [Ark Pixel Font](https://github.com/TakWolf/ark-pixel-font)
  (bitmap, CJK)
- [BQN386](https://dzaima.github.io/BQN386/)
  ([Github](https://github.com/dzaima/BQN386))
  (derived from [APL385](https://www.apl385.com/fonts/) and
  [APL386](https://abrudz.github.io/APL386/))
- [Berkeley Mono](https://berkeleygraphics.com/typefaces/berkeley-mono/)
  (_commercial_)
- [Comic Code](https://tosche.net/fonts/comic-code) (_commercial_,
  open-source alts [one](https://dtinth.github.io/comic-mono-font/)
  and [two](https://github.com/jesusmgg/comic-shanns-mono))
- [Commit Mono](https://commitmono.com/)
  ([Github](https://github.com/eigilnikolajsen/commit-mono))
- [Cozette](https://github.com/slavfox/Cozette) (bitmap)
- [Fairfax](http://www.kreativekorp.com/software/fonts/fairfax/) (bitmap)
  and [Fairfax HD](http://www.kreativekorp.com/software/fonts/fairfaxhd/)
  (vector) ([Github](https://github.com/kreativekorp/open-relay))
- [Fantasque Sans Mono](https://github.com/belluzj/fantasque-sans)
- [Galmuri](https://github.com/quiple/galmuri) (bitmap, CJK)
- [Iosveka](https://github.com/be5invis/Iosevka)
  ([CJK variant](https://github.com/be5invis/Sarasa-Gothic))
  (on macOS, install individual TTF files rather than the super TTC
  file, see [here](https://github.com/be5invis/Iosevka/issues/1377),
  [derivative](https://github.com/shytikov/pragmasevka) to mimic
  PragmataPro more closely than the SS08 variant)
- [JetBrains Mono](https://github.com/JetBrains/JetBrainsMono)
- [Julia Mono](https://juliamono.netlify.app/)
  ([Github](https://github.com/cormullion/juliamono),
  good Unicode coverage)
- [Monocraft](https://github.com/IdreesInc/Monocraft)
  (bitmap, emulates Minecraft typeface, proportional
  [version](https://github.com/IdreesInc/Minecraft-Font))
- [OCR A](https://sourceforge.net/projects/ocr-a-font/)
  (the TTF file does work correctly on macOS so for that system
  install the PFA file instead, see next font for alternative)
- [OCR B](https://tsukurimashou.osdn.jp/ocr.php.en)
  (also has OCR A with a larger glyph set than the previous font)
- [Pixel Code](https://qwerasd205.github.io/PixelCode/examples/specimen.html)
  ([Github](https://github.com/qwerasd205/PixelCode))
- [PragmataPro](https://fsd.it/shop/fonts/pragmatapro/) (_commercial_,
  good Unicode coverage)
- [Proggy](https://github.com/bluescan/proggyfonts) fonts
- [Space Mono](https://www.colophon-foundry.org/custom-projects/space-mono)
  ([Github](https://github.com/googlefonts/spacemono))
- [Spleen](https://github.com/fcambus/spleen) (bitmap)
- [Terminus](https://terminus-font.sourceforge.net/)
  (bitmap, [TTF version](https://files.ax86.net/terminus-ttf/))
- [Unifont](http://unifoundry.com/unifont/index.html) (bitmap,
  extremely good Unicode coverage)
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
- [Avenir](https://en.wikipedia.org/wiki/Avenir_(typeface))
  and its variants (included with macOS)
- Brandon [Grotesque](https://www.hvdfonts.com/fonts/brandon-grotesque)
  and [Text](https://www.hvdfonts.com/fonts/brandon-text)
  (condensed versions also available, _commercial_)
- [Clarity City](https://github.com/vmware/clarity-city)
  (similar to Gotham but with stricter geometry)
- [Clear Sans](https://github.com/intel/clear-sans)
- [Comic Neue](https://comicneue.com/)
  ([Github](https://github.com/crozynski/comicneue),
  marker-style font mimicking Comic Sans, has angular variant)
- [Cooper Hewitt](https://www.cooperhewitt.org/open-source-at-cooper-hewitt/cooper-hewitt-the-typeface-by-chester-jenkins/)
  ([Github](https://github.com/cooperhewitt/cooperhewitt-typeface))
- [Dungeon Chunk](https://github.com/PriorityInterrupt/dungeon-chunk)
- [Figtree](https://github.com/erikdkennedy/figtree)
- [Finlandica](https://github.com/HelsinkiTypeStudio/Finlandica)
- [FiraGO](https://bboxtype.com/typefaces/FiraGO/)
  ([Github](https://github.com/bBoxType/FiraGO))
- [Gabarito](https://github.com/naipefoundry/gabarito)
- [Georama](https://github.com/productiontype/Georama)
- [Inter](https://github.com/rsms/inter)
  (Helvetica alternative)
- [Jost*](https://github.com/indestructible-type/Jost)
  (Futura alternative)
- [Lato](https://github.com/googlefonts/LatoGFVersion)
  ([source](https://github.com/latofonts/lato-source))
- [Maple Mono](https://github.com/subframe7536/Maple-font)
- [Northrup](https://github.com/mirnovov/northrup)
- [Open Sans](https://github.com/googlefonts/opensans)
- [Outfit](https://github.com/Outfitio/Outfit-Fonts)
- [Pixelify Sans](https://github.com/eifetx/Pixelify-Sans)
- [Pixellari](https://github.com/zedseven/Pixellari)
- [Poppins](https://www.indiantypefoundry.com/fonts/poppins)
  ([Github](https://github.com/itfoundry/Poppins), geometric)
- [Plein](https://www.fontshare.com/fonts/plein)
- [Plus Jakarta Sans](https://github.com/tokotype/PlusJakartaSans)
- Proxima [Nova](https://www.marksimonson.com/fonts/view/proxima-nova)
  and [Vara](https://www.marksimonson.com/fonts/view/proxima-vara)
  (_commercial_)
- [Public Sans](https://public-sans.digital.gov/)
  ([Github](https://github.com/uswds/public-sans))
- [Radio-Canada](https://github.com/cbcrc/radiocanadafonts)
  (parametric axes, see [link](https://web.dev/variable-fonts/))
- [Raleway](https://github.com/theleagueof/raleway)
- [Readex Pro](https://github.com/ThomasJockin/readexpro) (expanded
  [Lexend](https://github.com/googlefonts/lexend))
- [Schibsted Grotesk](https://github.com/schibsted/schibsted-grotesk)
- [Shantell Sans](https://github.com/arrowtype/shantell-sans)
  (marker-style font like Comic Sans and Inkwell Sans,
  parametric axes, see [link](https://web.dev/variable-fonts/))
- [Space Grotesk](https://github.com/floriankarsten/space-grotesk)
- [TASA Typeface Collection](https://tasatype.localremote.co/)
  ([Github](https://github.com/adrianzwz/TASA-Typeface-Collection))
- [Urbanist](https://github.com/coreyhu/Urbanist)
- [Wix Madefor](https://github.com/wix-incubator/wixmadefor)
- [Womprat](https://crown.fontdue.com/fonts/womprat) (Star Wars-y,
  _commercial_)
- [Work Sans](http://weiweihuanghuang.github.io/Work-Sans/)
  ([Github](https://github.com/weiweihuanghuang/Work-Sans))
- [Ysabeau](https://github.com/CatharsisFonts/Ysabeau)

## Serif proportional fonts

- [Amstelvar](https://github.com/googlefonts/amstelvar)
  (parametric axes, see [link](https://web.dev/variable-fonts/))
- [Besley](https://indestructibletype.com/Besley.html)
  ([Github](https://github.com/indestructible-type/Besley))
- [Bodoni Moda](http://indestructible-type.github.io/Bodoni.html)
  ([Github](https://github.com/indestructible-type/Bodoni))
- [Bona Nova](https://github.com/kosmynkab/Bona-Nova)
- [Cardo](https://github.com/googlefonts/CardoFont)
- [CERN-www](https://github.com/djrrb/CERN-www-fonts)
  ((WorldWideWeb)[https://worldwideweb.cern.ch/] browser font clone)
- [Courier Prime](https://quoteunquoteapps.com/courierprime/)
  [Regular](https://github.com/quoteunquoteapps/CourierPrime),
  [Sans](https://github.com/quoteunquoteapps/CourierPrimeSans),
  and [Code](https://github.com/quoteunquoteapps/CourierPrimeCode)
- [Domaine](https://klim.co.nz/collections/domaine/) (_commercial_)
- [EB Garamond](https://github.com/georgd/EB-Garamond)
- [ETbb](https://tug.org/FontCatalogue/etbb/)
  ([CTAN](https://ctan.org/pkg/etbb?lang=en))
- [IM Fell](https://iginomarini.com/fell/the-revival-fonts/) fonts
  ([Google Fonts](https://fonts.google.com/?query=IM+Fell),
  [more](https://iginomarini.com/fell/history/)
  [info](https://www.linyangchen.com/Typography-Fell-Types-font))
- Grenze [Regular](https://www.omnibus-type.com/fonts/grenze/)
  ([Github](https://github.com/Omnibus-Type/Grenze)), and
  [Gotisch](https://www.omnibus-type.com/fonts/grenze-gotisch/)
  ([Github](https://github.com/Omnibus-Type/Grenze-Gotisch))
- [Mate](https://github.com/etunni/mate)
- [Playfair](https://github.com/googlefonts/Playfair)
- [Reforma](https://pampatype.com/reforma)
- [Spectral](https://github.com/productiontype/spectral)
- [Sprat](https://github.com/EthanNakache/Sprat-type)

## Symbol fonts

- [Asap Symbol](https://www.omnibus-type.com/fonts/asap-symbol/)
  ([Github](https://github.com/Omnibus-Type/Asap-Symbol))
- [Catrinity](https://catrinity-font.de/) (sans-serif)
- [DSEG](https://www.keshikan.net/fonts-e.html)
  ([Github](https://github.com/keshikan/DSEG)) (LCD display-like)
- [Quivira](http://www.quivira-font.com/) (serif)
- [Redacted](https://github.com/christiannaths/redacted-font)
  (for keeping wireframes and prototypes free of distracting text)
- [Siji-ng](https://github.com/mxkrsv/siji-ng) (bitmap)
- [Symbola](https://dn-works.com/ufas/) (_free for personal use_)

## Script fonts

- [Ballet](https://github.com/Omnibus-Type/Ballet)
- [Borel](https://github.com/RosaWagner/Borel)

## Font families

- [Bagnard](https://github.com/sebsan/Bagnard) and
  [Bagnard Sans](https://github.com/sebsan/Bagnard-Sans)
- [DM](https://github.com/googlefonts/dm-fonts) (sans
  like Poppins, serif like Source Serif Pro, has monospace
  [version](https://github.com/googlefonts/dm-mono) based on
  the sans font)
- [Go](https://go.dev/blog/go-fonts)
- [IBM Plex](https://github.com/IBM/plex)
- [Input](https://input.djr.com/) (_free for personal use_)
- Instrument [Sans](https://github.com/Instrument/instrument-sans)
  and [Serif](https://github.com/Instrument/instrument-serif)
- Josefin [Sans](https://github.com/googlefonts/josefinsans) and
  [Slab](https://github.com/davelab6/josefinslab)
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
- [Recursive](https://github.com/arrowtype/recursive)
- [Red Hat](https://github.com/RedHatOfficial/RedHatFont)
- [Roboto](https://fonts.google.com/specimen/Roboto),
  [Condensed](https://fonts.google.com/specimen/Roboto+Condensed),
  [Serif](https://fonts.google.com/specimen/Roboto+Serif),
  [Slab](https://fonts.google.com/specimen/Roboto+Slab),
  [Mono](https://fonts.google.com/specimen/Roboto+Mono),
  [Flex](https://github.com/googlefonts/roboto-flex)
  (parametric axes, see [link](https://web.dev/variable-fonts/))
- Spline [Sans](https://github.com/SorkinType/SplineSans),
  [Sans Mono](https://github.com/SorkinType/SplineSansMono)
- Source [Sans](https://github.com/adobe-fonts/source-sans),
  [Serif](https://github.com/adobe-fonts/source-serif),
  [Code Pro](https://github.com/adobe-fonts/source-code-pro) (CJK
  [Sans](https://github.com/adobe-fonts/source-han-sans),
  [Serif](https://github.com/adobe-fonts/source-han-serif),
  [Mono](https://github.com/adobe-fonts/source-han-mono) variants)

## Links

- [Beautiful Web Type](https://beautifulwebtype.com/)
- [Collletttivo](https://www.collletttivo.it/)
- [Colophon Foundry](https://www.colophon-foundry.org/) (_commercial_)
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
- [The League of Movable Type](https://www.theleagueofmoveabletype.com/)
  ([Github](https://github.com/theleagueof))
- [The Ultimate Oldschool PC Font Pack](https://int10h.org/oldschool-pc-fonts/)
- [Tunera Type Foundry](https://www.tunera.xyz/)
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

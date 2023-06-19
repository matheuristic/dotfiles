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

## Monospace fonts

- [APL385](https://www.apl385.com/fonts/)
- [Berkeley Mono](https://berkeleygraphics.com/typefaces/berkeley-mono/)
  (_commercial_)
- [Comic Code](https://tosche.net/fonts/comic-code) (_commercial_)
- [Cozette](https://github.com/slavfox/Cozette) (bitmap)
- [Fantasque Sans Mono](https://github.com/belluzj/fantasque-sans)
- [Intel One Mono](https://github.com/intel/intel-one-mono)
- [Iosveka](https://github.com/be5invis/Iosevka)
  ([CJK variant](https://github.com/be5invis/Sarasa-Gothic))
  (on macOS, install individual TTF files rather than the super TTC
  file, see [here](https://github.com/be5invis/Iosevka/issues/1377))
- [JetBrains Mono](https://github.com/JetBrains/JetBrainsMono)
- [Monocraft](https://github.com/IdreesInc/Monocraft)
  (bitmap, emulates Minecraft typeface, proportional
  [version](https://github.com/IdreesInc/Minecraft-Font))
- [PragmataPro](https://fsd.it/shop/fonts/pragmatapro/) (_commercial_)
- [Twilio Sans Mono](https://github.com/twilio/twilio-sans-mono)
- [Unifont](http://unifoundry.com/unifont/index.html) (bitmap)

## Sans-serif proportional fonts

- [Atkinson Hyperlegible](https://brailleinstitute.org/freefont)
- [Clarity City](https://github.com/vmware/clarity-city)
  (similar to Gotham but with stricter geometry)
- [Clear Sans](https://github.com/intel/clear-sans)
- [Cooper Hewitt](https://www.cooperhewitt.org/open-source-at-cooper-hewitt/cooper-hewitt-the-typeface-by-chester-jenkins/)
  ([Github](https://github.com/cooperhewitt/cooperhewitt-typeface))
- [Figtree](https://github.com/erikdkennedy/figtree)
- [FiraGO](https://bboxtype.com/typefaces/FiraGO/)
  ([Github](https://github.com/bBoxType/FiraGO))
- [Inter](https://github.com/rsms/inter)
  (Helvetica alternative)
- [Jost*](https://github.com/indestructible-type/Jost)
  (Futura alternative)
- [Open Sans](https://github.com/googlefonts/opensans)
- [Outfit](https://github.com/Outfitio/Outfit-Fonts)
- [Poppins](https://www.indiantypefoundry.com/fonts/poppins)
  ([Github](https://github.com/itfoundry/Poppins), geometric)
- [Plein](https://www.fontshare.com/fonts/plein)
- [Plus Jakarta Sans](https://github.com/tokotype/PlusJakartaSans)
- [Public Sans](https://public-sans.digital.gov/)
  ([Github](https://github.com/uswds/public-sans))
- [Readex Pro](https://github.com/ThomasJockin/readexpro) (expanded
  [Lexend](https://github.com/googlefonts/lexend))
- [Space Grotesk](https://github.com/floriankarsten/space-grotesk)
- [Urbanist](https://github.com/coreyhu/Urbanist)
- [Work Sans](http://weiweihuanghuang.github.io/Work-Sans/)
  ([Github](https://github.com/weiweihuanghuang/Work-Sans))
- [Ysabeau](https://github.com/CatharsisFonts/Ysabeau)

## Serif proportional fonts

- [Amstelvar](https://github.com/googlefonts/amstelvar)
  (parametric axes, see [link](https://web.dev/variable-fonts/))
- [Bodoni](http://indestructible-type.github.io/Bodoni.html)
  ([Github](https://github.com/indestructible-type/Bodoni))
- [Courier Prime](https://quoteunquoteapps.com/courierprime/)
  [Regular](https://github.com/quoteunquoteapps/CourierPrime),
  [Sans](https://github.com/quoteunquoteapps/CourierPrimeSans),
  and [Code](https://github.com/quoteunquoteapps/CourierPrimeCode)
- [ETbb](https://tug.org/FontCatalogue/etbb/)
  ([CTAN](https://ctan.org/pkg/etbb?lang=en))
- Grenze [Regular](https://www.omnibus-type.com/fonts/grenze/)
  ([Github](https://github.com/Omnibus-Type/Grenze)), and
  [Gotisch](https://www.omnibus-type.com/fonts/grenze-gotisch/)
  ([Github](https://github.com/Omnibus-Type/Grenze-Gotisch))
- [Reforma](https://pampatype.com/reforma)

## Symbol fonts

- [Asap Symbol](https://www.omnibus-type.com/fonts/asap-symbol/)
  ([Github](https://github.com/Omnibus-Type/Asap-Symbol))
- [Catrinity](https://catrinity-font.de/) (sans-serif)
- [DSEG](https://www.keshikan.net/fonts-e.html)
  ([Github](https://github.com/keshikan/DSEG)) (LCD display-like)
- [Quivira](http://www.quivira-font.com/) (serif)
- [Symbola](https://dn-works.com/ufas/) (_free for personal use_)

## Script fonts

- [Ballet](https://github.com/Omnibus-Type/Ballet)

## Font families

- [DM](https://github.com/googlefonts/dm-fonts) (sans like Poppins, serif like Source Serif Pro, has monospace [version](https://github.com/googlefonts/dm-mono) based on sans font)
- [Go](https://go.dev/blog/go-fonts)
- [IBM Plex](https://github.com/IBM/plex)
- [Input](https://input.djr.com/) (_free for personal use_)
- [Kurinto Font Folio](https://www.kurinto.com/)
- Lucida ([B&H](https://lucidafonts.com/),
  [TUG](https://tug.org/store/lucida/index.html)) (_commercial_;
  some versions licensed and distributed with specific platforms,
  e.g., _Lucida Sans Unicode_ on Windows, _Lucida Grande_ on macOS)
  ([detail](https://bigelowandholmes.typepad.com/bigelow-holmes/how-and-why-lucida/))
- [M+](https://mplusfonts.github.io/)
  ([Github](https://github.com/coz-m/MPLUS_FONTS))
- [New Computer Modern](https://tug.org/FontCatalogue/newcomputermodernroman/)
  ([CTAN](https://ctan.org/pkg/newcomputermodern?lang=en))
- [Recursive](https://github.com/arrowtype/recursive)
- [Red Hat](https://github.com/RedHatOfficial/RedHatFont)
- [Roboto](https://fonts.google.com/specimen/Roboto),
  [Condensed](https://fonts.google.com/specimen/Roboto+Condensed),
  [Serif](https://fonts.google.com/specimen/Roboto+Serif),
  [Slab](https://fonts.google.com/specimen/Roboto+Slab),
  [Mono](https://fonts.google.com/specimen/Roboto+Mono),
  [Flex](https://github.com/googlefonts/roboto-flex)
  (parametric axes, see [link](https://web.dev/variable-fonts/));
  use [Noto](https://fonts.google.com/noto/fonts) variants for CJK and
  other languages not supported by Roboto
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
- [Google Font Updates](https://twitter.com/googlefonts?lang=us)
- [Google Fonts Knowledge](https://fonts.google.com/knowledge)
- [Omnibus-Type](https://www.omnibus-type.com/)
- [On snot and fonts](http://luc.devroye.org/fonts.html)
- [Programming Fonts](https://www.programmingfonts.org/)
  ([Tumblr](https://programmingfonts.tumblr.com/))
- [The League of Movable Type](https://www.theleagueofmoveabletype.com/)
  ([Github](https://github.com/theleagueof))
- [The Ultimate Oldschool PC Font Pack](https://int10h.org/oldschool-pc-fonts/)
- [Typewolf](https://www.typewolf.com/)
- [Typographica](https://typographica.org/)
  ([Library](https://library.typographica.org/))
- [Typography: Google Fonts Combinations](https://www.behance.net/gallery/35768979/Typography-Google-Fonts-Combinations)
- [Variable fonts](https://variablefonts.typenetwork.com/) - Type Network
- [Variable fonts](https://v-fonts.com/) - A simple resource for finding and trying variable fonts
- [Velvetyne Type Foundry](https://velvetyne.fr/)
  ([Gitlab](https://gitlab.com/velvetyne))

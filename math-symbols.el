;;; math-symbols.el --- math symbol input and conversion tool -*- lexical-binding: t -*-

;; Filename: math-symbols.el
;; Description: Math symbol input and TeX conversion tool.
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2013-03-25
;; Version: 0.4
;; Keywords: math symbols, tex, latex
;; URL: https://github.com/kawabata/math-symbols
;;
;;; Commentary:
;;
;; This program let you input/convert math symbols in TeX names.
;; (M-x math-input or M-x math-symbols-from-tex-region).
;; It also provides mathematical stylization function.
;;
;; Example:
;;   "f(x+y)" → "𝑓(𝑥+𝑦)" (M-x math-italic-region)
;;   "Fraktur" → "𝔉𝔯𝔞𝔨𝔱𝔲𝔯" (M-x math-fraktur-region)
;;   "\int g(x^2)dx = \pi e^(ix)" → "∫ 𝑔(𝑥²)𝑑𝑥 = 𝜋 𝑒^(𝑖𝑥)"
;;   (M-x math-symbols-from-tex-region)
;;
;; You should install Math fonts such as "STIX" and add it to your
;; fontset to fully utilize this tool.
;;
;; unimathsymbols.txt
;; 
;; # :Licence:   This work may be distributed and/or modified under the
;; #             conditions of the `LaTeX Project Public License`_,
;; #             either version 1.3 of this license or (at your option)
;; #             any later version.
;;
;; References:
;; - UTR#25 UNICODE SUPPORT FOR MATHEMATICS
;;   (http://www.unicode.org/reports/tr25/tr25-6.html)
;;
;; | styles / scripts         | alphabets | greeks* | numerals |
;; |--------------------------+-----------+---------+----------|
;; | bold                     | yes       | yes     | yes      |
;; | (bold) italic            | yes       | yes     | yes      |
;; | (bold) fraktur           | yes       | no      | no       |
;; | (bold) script            | yes       | no      | no       |
;; | double-struck            | yes       | partial | yes      |
;; | monospace                | yes       | no      | yes      |
;; | sans-serif (italic)      | yes       | no      | yes      |
;; | sans-serif bold (italic) | yes       | yes     | yes      |
;; | subscript                | partial   | no      | yes      |
;; | superscript              | partial   | no      | yes      |
;;
;;  * include greek symbols and nabla (ϵ, ϑ, ϰ, ϕ, ϱ, ϖ, ∇).

;;; Code:

;; data generated from `unimathsymbols.txt'
(defvar math-symbols-tex-table
  #s(hash-table data
    (?# "#" ?$ "$" ?% "%" ?& "&" ?[ "lbrack" ?\ "backslash"
     ?] "rbrack" ?_ "_" ?{ "{" ?} "}" ?~ "sptilde" ?¢ "cent"
     ?£ "pounds" ?¥ "yen" ?¨ "spddot" ?¬ "neg" ?® "circledR"
     ?± "pm" ?µ "Micro" ?× "times" ?ð "eth" ?÷ "div"
     ?ı "imath" ?ȷ "jmath" ?Γ "Gamma" ?Δ "Delta" ?Θ "Theta"
     ?Λ "Lambda" ?Ξ "Xi" ?Π "Pi" ?Σ "Sigma" ?Υ "Upsilon"
     ?Φ "Phi" ?Ψ "Psi" ?Ω "Omega" ?α "alpha" ?β "beta"
     ?γ "gamma" ?δ "delta" ?ε "varepsilon" ?ζ "zeta"
     ?η "eta" ?θ "theta" ?ι "iota" ?κ "kappa" ?λ "lambda"
     ?μ "mu" ?ν "nu" ?ξ "xi" ?π "pi" ?ρ "rho" ?ς "varsigma"
     ?σ "sigma" ?τ "tau" ?υ "upsilon" ?φ "varphi" ?χ "chi"
     ?ψ "psi" ?ω "omega" ?ϐ "varbeta" ?ϑ "vartheta" ?ϕ "phi"
     ?ϖ "varpi" ?Ϙ "Qoppa" ?ϙ "qoppa" ?Ϛ "Stigma"
     ?ϛ "stigma" ?Ϝ "Digamma" ?ϝ "digamma" ?Ϟ "Koppa"
     ?ϟ "koppa" ?Ϡ "Sampi" ?ϡ "sampi" ?ϱ "varrho"
     ?ϵ "epsilon" ?϶ "backepsilon" ?  "quad" ?‖ "|"
     ?† "dagger" ?‡ "ddagger" ?… "ldots" ?′ "prime"
     ?″ "second" ?‴ "third" ?‵ "backprime" ?⁀ "cat"
     ?⁗ "fourth" ?  ":" ?ℂ "mathbb{C}" ?ℇ "Euler"
     ?ℊ "mathcal{g}" ?ℋ "mathcal{H}" ?ℌ "mathfrak{H}"
     ?ℍ "mathbb{H}" ?ℏ "hslash" ?ℐ "mathcal{I}" ?ℑ "Im"
     ?ℒ "mathcal{L}" ?ℓ "ell" ?ℕ "mathbb{N}" ?℘ "wp"
     ?ℙ "mathbb{P}" ?ℚ "mathbb{Q}" ?ℛ "mathcal{R}"
     ?ℜ "Re" ?ℝ "mathbb{R}" ?ℤ "mathbb{Z}" ?Ω "tcohm"
     ?℧ "mho" ?ℨ "mathfrak{Z}" ?Å "Angstroem"
     ?ℬ "mathcal{B}" ?ℭ "mathfrak{C}" ?ℯ "mathcal{e}"
     ?ℰ "mathcal{E}" ?ℱ "mathcal{F}" ?Ⅎ "Finv"
     ?ℳ "mathcal{M}" ?ℴ "mathcal{o}" ?ℵ "aleph" ?ℶ "beth"
     ?ℷ "gimel" ?ℸ "daleth" ?ℼ "mathbb{\\pi}"
     ?ℽ "mathbb{\\gamma}" ?ℾ "mathbb{\\Gamma}"
     ?ℿ "mathbb{\\Pi}" ?⅀ "mathbb{\\Sigma}" ?⅄ "Yup"
     ?ⅅ "CapitalDifferentialD" ?ⅆ "DifferentialD"
     ?ⅇ "ExponetialE" ?ⅈ "ComplexI" ?ⅉ "ComplexJ"
     ?⅋ "invamp" ?← "leftarrow" ?↑ "uparrow"
     ?→ "rightarrow" ?↓ "downarrow" ?↔ "leftrightarrow"
     ?↕ "updownarrow" ?↖ "nwarrow" ?↗ "nearrow"
     ?↘ "searrow" ?↙ "swarrow" ?↚ "nleftarrow"
     ?↛ "nrightarrow" ?↞ "twoheadleftarrow"
     ?↠ "twoheadrightarrow" ?↢ "leftarrowtail"
     ?↣ "rightarrowtail" ?↤ "mapsfrom" ?↥ "MapsUp"
     ?↦ "mapsto" ?↧ "MapsDown" ?↩ "hookleftarrow"
     ?↪ "hookrightarrow" ?↫ "looparrowleft"
     ?↬ "looparrowright" ?↭ "leftrightsquigarrow"
     ?↮ "nleftrightarrow" ?↯ "lightning" ?↰ "Lsh"
     ?↱ "Rsh" ?↲ "dlsh" ?↳ "drsh" ?↶ "curvearrowleft"
     ?↷ "curvearrowright" ?↺ "circlearrowleft"
     ?↻ "circlearrowright" ?↼ "leftharpoonup"
     ?↽ "leftharpoondown" ?↾ "upharpoonright"
     ?↿ "upharpoonleft" ?⇀ "rightharpoonup"
     ?⇁ "rightharpoondown" ?⇂ "downharpoonright"
     ?⇃ "downharpoonleft" ?⇄ "rightleftarrows"
     ?⇅ "updownarrows" ?⇆ "leftrightarrows"
     ?⇇ "leftleftarrows" ?⇈ "upuparrows"
     ?⇉ "rightrightarrows" ?⇊ "downdownarrows"
     ?⇋ "leftrightharpoons" ?⇌ "rightleftharpoons"
     ?⇍ "nLeftarrow" ?⇎ "nLeftrightarrow" ?⇏ "nRightarrow"
     ?⇐ "Leftarrow" ?⇑ "Uparrow" ?⇒ "Rightarrow"
     ?⇓ "Downarrow" ?⇔ "Leftrightarrow" ?⇕ "Updownarrow"
     ?⇖ "Nwarrow" ?⇗ "Nearrow" ?⇘ "Searrow" ?⇙ "Swarrow"
     ?⇚ "Lleftarrow" ?⇛ "Rrightarrow" ?⇜ "leftsquigarrow"
     ?⇝ "rightsquigarrow" ?⇠ "dashleftarrow"
     ?⇢ "dashrightarrow" ?⇤ "LeftArrowBar"
     ?⇥ "RightArrowBar" ?⇵ "downuparrows" ?⇸ "pfun"
     ?⇻ "ffun" ?⇽ "leftarrowtriangle"
     ?⇾ "rightarrowtriangle" ?⇿ "leftrightarrowtriangle"
     ?∀ "forall" ?∁ "complement" ?∂ "partial" ?∃ "exists"
     ?∄ "nexists" ?∅ "varnothing" ?∇ "nabla" ?∈ "in"
     ?∉ "notin" ?∋ "ni" ?∌ "nni" ?∏ "prod" ?∐ "coprod"
     ?∑ "sum" ?∓ "mp" ?∔ "dotplus" ?∕ "slash"
     ?∖ "smallsetminus" ?∗ "ast" ?∘ "circ" ?∙ "bullet"
     ?√ "sqrt" ?∛ "sqrt[3]" ?∜ "sqrt[4]" ?∝ "propto"
     ?∞ "infty" ?∟ "rightangle" ?∠ "angle"
     ?∡ "measuredangle" ?∢ "sphericalangle" ?∣ "mid"
     ?∤ "nmid" ?∥ "parallel" ?∦ "nparallel" ?∧ "wedge"
     ?∨ "vee" ?∩ "cap" ?∪ "cup" ?∫ "int" ?∬ "iint"
     ?∭ "iiint" ?∮ "oint" ?∯ "oiint" ?∰ "oiiint"
     ?∲ "varointclockwise" ?∳ "ointctrclockwise"
     ?∴ "therefore" ?∵ "because" ?∷ "Proportion"
     ?∹ "eqcolon" ?∼ "sim" ?∽ "backsim" ?∿ "AC" ?≀ "wr"
     ?≁ "nsim" ?≂ "eqsim" ?≃ "simeq" ?≄ "nsimeq"
     ?≅ "cong" ?≇ "ncong" ?≈ "approx" ?≉ "napprox"
     ?≊ "approxeq" ?≍ "asymp" ?≎ "Bumpeq" ?≏ "bumpeq"
     ?≐ "doteq" ?≑ "Doteq" ?≒ "fallingdotseq"
     ?≓ "risingdotseq" ?≔ "coloneq" ?≕ "eqcolon"
     ?≖ "eqcirc" ?≗ "circeq" ?≙ "corresponds"
     ?≜ "triangleq" ?≠ "neq" ?≡ "equiv" ?≢ "nequiv"
     ?≤ "leq" ?≥ "geq" ?≦ "leqq" ?≧ "geqq" ?≨ "lneqq"
     ?≩ "gneqq" ?≪ "ll" ?≫ "gg" ?≬ "between"
     ?≭ "notasymp" ?≮ "nless" ?≯ "ngtr" ?≰ "nleq"
     ?≱ "ngeq" ?≲ "lesssim" ?≳ "gtrsim" ?≴ "NotLessTilde"
     ?≵ "NotGreaterTilde" ?≶ "lessgtr" ?≷ "gtrless"
     ?≹ "NotGreaterLess" ?≺ "prec" ?≻ "succ"
     ?≼ "preccurlyeq" ?≽ "succcurlyeq" ?≾ "precsim"
     ?≿ "succsim" ?⊀ "nprec" ?⊁ "nsucc" ?⊂ "subset"
     ?⊃ "supset" ?⊄ "nsubset" ?⊅ "nsupset" ?⊆ "subseteq"
     ?⊇ "supseteq" ?⊈ "nsubseteq" ?⊉ "nsupseteq"
     ?⊊ "subsetneq" ?⊋ "supsetneq" ?⊎ "uplus"
     ?⊏ "sqsubset" ?⊐ "sqsupset" ?⊑ "sqsubseteq"
     ?⊒ "sqsupseteq" ?⊓ "sqcap" ?⊔ "sqcup" ?⊕ "oplus"
     ?⊖ "ominus" ?⊗ "otimes" ?⊘ "oslash" ?⊙ "odot"
     ?⊚ "circledcirc" ?⊛ "circledast" ?⊝ "circleddash"
     ?⊞ "boxplus" ?⊟ "boxminus" ?⊠ "boxtimes" ?⊡ "boxdot"
     ?⊢ "vdash" ?⊣ "dashv" ?⊤ "top" ?⊥ "bot"
     ?⊧ "models" ?⊨ "vDash" ?⊩ "Vdash" ?⊪ "Vvdash"
     ?⊫ "VDash" ?⊬ "nvdash" ?⊭ "nvDash" ?⊮ "nVdash"
     ?⊯ "nVDash" ?⊲ "vartriangleleft" ?⊳ "vartriangleright"
     ?⊴ "trianglelefteq" ?⊵ "trianglerighteq"
     ?⊶ "multimapdotbothA" ?⊷ "multimapdotbothB"
     ?⊸ "multimap" ?⊺ "intercal" ?⊻ "veebar"
     ?⊼ "barwedge" ?⋀ "bigwedge" ?⋁ "bigvee" ?⋂ "bigcap"
     ?⋃ "bigcup" ?⋄ "diamond" ?⋅ "cdot" ?⋆ "star"
     ?⋇ "divideontimes" ?⋈ "bowtie" ?⋉ "ltimes"
     ?⋊ "rtimes" ?⋋ "leftthreetimes" ?⋌ "rightthreetimes"
     ?⋍ "backsimeq" ?⋎ "curlyvee" ?⋏ "curlywedge"
     ?⋐ "Subset" ?⋑ "Supset" ?⋒ "Cap" ?⋓ "Cup"
     ?⋔ "pitchfork" ?⋕ "hash" ?⋖ "lessdot" ?⋗ "gtrdot"
     ?⋘ "lll" ?⋙ "ggg" ?⋚ "lesseqgtr" ?⋛ "gtreqless"
     ?⋞ "curlyeqprec" ?⋟ "curlyeqsucc" ?⋠ "npreceq"
     ?⋡ "nsucceq" ?⋢ "nsqsubseteq" ?⋣ "nsqsupseteq"
     ?⋦ "lnsim" ?⋧ "gnsim" ?⋨ "precnsim" ?⋩ "succnsim"
     ?⋪ "ntriangleleft" ?⋫ "ntriangleright"
     ?⋬ "ntrianglelefteq" ?⋭ "ntrianglerighteq" ?⋮ "vdots"
     ?⋯ "cdots" ?⋰ "iddots" ?⋱ "ddots" ?⋶ "barin"
     ?⌀ "diameter" ?⌈ "lceil" ?⌉ "rceil" ?⌊ "lfloor"
     ?⌋ "rfloor" ?⌐ "invneg" ?⌑ "wasylozenge"
     ?⌜ "ulcorner" ?⌝ "urcorner" ?⌞ "llcorner"
     ?⌟ "lrcorner" ?⌢ "frown" ?⌣ "smile" ?⌹ "APLinv"
     ?⌿ "notslash" ?⍀ "notbackslash" ?⍇ "APLleftarrowbox"
     ?⍈ "APLrightarrowbox" ?⍐ "APLuparrowbox"
     ?⍗ "APLdownarrowbox" ?⍝ "APLcomment" ?⍞ "APLinput"
     ?⍟ "APLlog" ?⏜ "overparen" ?⏝ "underparen"
     ?⏞ "overbrace" ?⏟ "underbrace" ?△ "bigtriangleup"
     ?▴ "blacktriangleup" ?▵ "smalltriangleup" ?▶ "RHD"
     ?▷ "rhd" ?▸ "blacktriangleright"
     ?▹ "smalltriangleright" ?▽ "bigtriangledown"
     ?▾ "blacktriangledown" ?▿ "smalltriangledown" ?◀ "LHD"
     ?◁ "lhd" ?◂ "blacktriangleleft" ?◃ "smalltriangleleft"
     ?◆ "Diamondblack" ?◇ "Diamond" ?◊ "lozenge"
     ?○ "Circle" ?● "CIRCLE" ?◐ "LEFTcircle"
     ?◑ "RIGHTcircle" ?◖ "LEFTCIRCLE" ?◗ "RIGHTCIRCLE"
     ?◫ "boxbar" ?◻ "square" ?◼ "blacksquare"
     ?★ "bigstar" ?☉ "Sun" ?☐ "Square" ?☑ "CheckedBox"
     ?☒ "XBox" ?☕ "steaming" ?☞ "pointright" ?☠ "skull"
     ?☢ "radiation" ?☣ "biohazard" ?☯ "yinyang"
     ?☹ "frownie" ?☺ "smiley" ?☻ "blacksmiley" ?☼ "sun"
     ?☽ "rightmoon" ?☾ "leftmoon" ?☿ "mercury"
     ?♀ "female" ?♁ "earth" ?♂ "male" ?♃ "jupiter"
     ?♄ "saturn" ?♅ "uranus" ?♆ "neptune" ?♇ "pluto"
     ?♈ "aries" ?♉ "taurus" ?♊ "gemini" ?♋ "cancer"
     ?♌ "leo" ?♍ "virgo" ?♎ "libra" ?♏ "scorpio"
     ?♐ "sagittarius" ?♑ "capricornus" ?♒ "aquarius"
     ?♓ "pisces" ?♠ "spadesuit" ?♡ "heartsuit"
     ?♢ "diamondsuit" ?♣ "clubsuit" ?♤ "varspadesuit"
     ?♥ "varheartsuit" ?♦ "vardiamondsuit" ?♧ "varclubsuit"
     ?♩ "quarternote" ?♪ "eighthnote" ?♫ "twonotes"
     ?♬ "sixteenthnote" ?♭ "flat" ?♮ "natural" ?♯ "sharp"
     ?♻ "recycle" ?⚓ "anchor" ?⚔ "swords" ?⚠ "warning"
     ?⚪ "medcirc" ?⚫ "medbullet" ?✎ "pencil"
     ?✓ "checkmark" ?✗ "ballotx" ?✠ "maltese"
     ?➢ "arrowbullet" ?⟂ "perp" ?⟅ "Lbag" ?⟆ "Rbag"
     ?⟐ "Diamonddot" ?⟜ "multimapinv" ?⟦ "llbracket"
     ?⟧ "rrbracket" ?⟨ "langle" ?⟩ "rangle" ?⟪ "lang"
     ?⟫ "rang" ?⟮ "lgroup" ?⟯ "rgroup"
     ?⟵ "longleftarrow" ?⟶ "longrightarrow"
     ?⟷ "longleftrightarrow" ?⟸ "Longleftarrow"
     ?⟹ "Longrightarrow" ?⟺ "Longleftrightarrow"
     ?⟻ "longmapsfrom" ?⟼ "longmapsto" ?⟽ "Longmapsfrom"
     ?⟾ "Longmapsto" ?⤀ "psur" ?⤆ "Mapsfrom"
     ?⤇ "Mapsto" ?⤒ "UpArrowBar" ?⤓ "DownArrowBar"
     ?⤔ "pinj" ?⤕ "finj" ?⤖ "bij" ?⤳ "leadsto"
     ?⥊ "leftrightharpoon" ?⥋ "rightleftharpoon"
     ?⥎ "leftrightharpoonup" ?⥏ "rightupdownharpoon"
     ?⥐ "leftrightharpoondown" ?⥑ "leftupdownharpoon"
     ?⥒ "LeftVectorBar" ?⥓ "RightVectorBar"
     ?⥔ "RightUpVectorBar" ?⥕ "RightDownVectorBar"
     ?⥖ "DownLeftVectorBar" ?⥗ "DownRightVectorBar"
     ?⥘ "LeftUpVectorBar" ?⥙ "LeftDownVectorBar"
     ?⥚ "LeftTeeVector" ?⥛ "RightTeeVector"
     ?⥜ "RightUpTeeVector" ?⥝ "RightDownTeeVector"
     ?⥞ "DownLeftTeeVector" ?⥟ "DownRightTeeVector"
     ?⥠ "LeftUpTeeVector" ?⥡ "LeftDownTeeVector"
     ?⥢ "leftleftharpoons" ?⥣ "upupharpoons"
     ?⥤ "rightrightharpoons" ?⥥ "downdownharpoons"
     ?⥪ "leftbarharpoon" ?⥫ "barleftharpoon"
     ?⥬ "rightbarharpoon" ?⥭ "barrightharpoon"
     ?⥮ "updownharpoons" ?⥯ "downupharpoons"
     ?⥼ "strictfi" ?⥽ "strictif" ?⦀ "VERT" ?⦁ "spot"
     ?⦅ "Lparen" ?⦆ "Rparen" ?⦇ "limg" ?⦈ "rimg"
     ?⦉ "lblot" ?⦊ "rblot" ?⦸ "circledbslash"
     ?⧀ "circledless" ?⧁ "circledgtr" ?⧄ "boxslash"
     ?⧅ "boxbslash" ?⧆ "boxast" ?⧇ "boxcircle"
     ?⧈ "boxbox" ?⧏ "LeftTriangleBar"
     ?⧐ "RightTriangleBar" ?⧟ "multimapboth"
     ?⧫ "blacklozenge" ?⧵ "setminus" ?⧹ "zhide"
     ?⨀ "bigodot" ?⨁ "bigoplus" ?⨂ "bigotimes"
     ?⨄ "biguplus" ?⨅ "bigsqcap" ?⨆ "bigsqcup"
     ?⨉ "varprod" ?⨌ "iiiint" ?⨏ "fint" ?⨖ "sqint"
     ?⨝ "Join" ?⨟ "zcmp" ?⨠ "zpipe" ?⨡ "zproject"
     ?⨾ "fcmp" ?⨿ "amalg" ?⩞ "doublebarwedge"
     ?⩤ "dsub" ?⩥ "rsub" ?⩴ "Coloneqq" ?⩵ "Equal"
     ?⩶ "Same" ?⩽ "leqslant" ?⩾ "geqslant"
     ?⪅ "lessapprox" ?⪆ "gtrapprox" ?⪇ "lneq"
     ?⪈ "gneq" ?⪉ "lnapprox" ?⪊ "gnapprox"
     ?⪋ "lesseqqgtr" ?⪌ "gtreqqless" ?⪕ "eqslantless"
     ?⪖ "eqslantgtr" ?⪡ "NestedLessLess"
     ?⪢ "NestedGreaterGreater" ?⪦ "leftslice"
     ?⪧ "rightslice" ?⪯ "preceq" ?⪰ "succeq"
     ?⪳ "preceqq" ?⪴ "succeqq" ?⪷ "precapprox"
     ?⪸ "succapprox" ?⪹ "precnapprox" ?⪺ "succnapprox"
     ?⪻ "llcurly" ?⪼ "ggcurly" ?⫅ "subseteqq"
     ?⫆ "supseteqq" ?⫋ "subsetneqq" ?⫌ "supsetneqq"
     ?⫪ "Top" ?⫫ "Bot" ?⫴ "interleave"
     ?⫼ "biginterleave" ?⫽ "sslash" ?⫾ "talloblong"
     ?⬛ "blacksquare" ?⬜ "square" ?𝐀 "mathbf{A}"
     ?𝐁 "mathbf{B}" ?𝐂 "mathbf{C}" ?𝐃 "mathbf{D}"
     ?𝐄 "mathbf{E}" ?𝐅 "mathbf{F}" ?𝐆 "mathbf{G}"
     ?𝐇 "mathbf{H}" ?𝐈 "mathbf{I}" ?𝐉 "mathbf{J}"
     ?𝐊 "mathbf{K}" ?𝐋 "mathbf{L}" ?𝐌 "mathbf{M}"
     ?𝐍 "mathbf{N}" ?𝐎 "mathbf{O}" ?𝐏 "mathbf{P}"
     ?𝐐 "mathbf{Q}" ?𝐑 "mathbf{R}" ?𝐒 "mathbf{S}"
     ?𝐓 "mathbf{T}" ?𝐔 "mathbf{U}" ?𝐕 "mathbf{V}"
     ?𝐖 "mathbf{W}" ?𝐗 "mathbf{X}" ?𝐘 "mathbf{Y}"
     ?𝐙 "mathbf{Z}" ?𝐚 "mathbf{a}" ?𝐛 "mathbf{b}"
     ?𝐜 "mathbf{c}" ?𝐝 "mathbf{d}" ?𝐞 "mathbf{e}"
     ?𝐟 "mathbf{f}" ?𝐠 "mathbf{g}" ?𝐡 "mathbf{h}"
     ?𝐢 "mathbf{i}" ?𝐣 "mathbf{j}" ?𝐤 "mathbf{k}"
     ?𝐥 "mathbf{l}" ?𝐦 "mathbf{m}" ?𝐧 "mathbf{n}"
     ?𝐨 "mathbf{o}" ?𝐩 "mathbf{p}" ?𝐪 "mathbf{q}"
     ?𝐫 "mathbf{r}" ?𝐬 "mathbf{s}" ?𝐭 "mathbf{t}"
     ?𝐮 "mathbf{u}" ?𝐯 "mathbf{v}" ?𝐰 "mathbf{w}"
     ?𝐱 "mathbf{x}" ?𝐲 "mathbf{y}" ?𝐳 "mathbf{z}"
     ?𝑨 "mathbfit{A}" ?𝑩 "mathbfit{B}"
     ?𝑪 "mathbfit{C}" ?𝑫 "mathbfit{D}"
     ?𝑬 "mathbfit{E}" ?𝑭 "mathbfit{F}"
     ?𝑮 "mathbfit{G}" ?𝑯 "mathbfit{H}"
     ?𝑰 "mathbfit{I}" ?𝑱 "mathbfit{J}"
     ?𝑲 "mathbfit{K}" ?𝑳 "mathbfit{L}"
     ?𝑴 "mathbfit{M}" ?𝑵 "mathbfit{N}"
     ?𝑶 "mathbfit{O}" ?𝑷 "mathbfit{P}"
     ?𝑸 "mathbfit{Q}" ?𝑹 "mathbfit{R}"
     ?𝑺 "mathbfit{S}" ?𝑻 "mathbfit{T}"
     ?𝑼 "mathbfit{U}" ?𝑽 "mathbfit{V}"
     ?𝑾 "mathbfit{W}" ?𝑿 "mathbfit{X}"
     ?𝒀 "mathbfit{Y}" ?𝒁 "mathbfit{Z}"
     ?𝒂 "mathbfit{a}" ?𝒃 "mathbfit{b}"
     ?𝒄 "mathbfit{c}" ?𝒅 "mathbfit{d}"
     ?𝒆 "mathbfit{e}" ?𝒇 "mathbfit{f}"
     ?𝒈 "mathbfit{g}" ?𝒉 "mathbfit{h}"
     ?𝒊 "mathbfit{i}" ?𝒋 "mathbfit{j}"
     ?𝒌 "mathbfit{k}" ?𝒍 "mathbfit{l}"
     ?𝒎 "mathbfit{m}" ?𝒏 "mathbfit{n}"
     ?𝒐 "mathbfit{o}" ?𝒑 "mathbfit{p}"
     ?𝒒 "mathbfit{q}" ?𝒓 "mathbfit{r}"
     ?𝒔 "mathbfit{s}" ?𝒕 "mathbfit{t}"
     ?𝒖 "mathbfit{u}" ?𝒗 "mathbfit{v}"
     ?𝒘 "mathbfit{w}" ?𝒙 "mathbfit{x}"
     ?𝒚 "mathbfit{y}" ?𝒛 "mathbfit{z}"
     ?𝒜 "mathcal{A}" ?𝒞 "mathcal{C}" ?𝒟 "mathcal{D}"
     ?𝒢 "mathcal{G}" ?𝒥 "mathcal{J}" ?𝒦 "mathcal{K}"
     ?𝒩 "mathcal{N}" ?𝒪 "mathcal{O}" ?𝒫 "mathcal{P}"
     ?𝒬 "mathcal{Q}" ?𝒮 "mathcal{S}" ?𝒯 "mathcal{T}"
     ?𝒰 "mathcal{U}" ?𝒱 "mathcal{V}" ?𝒲 "mathcal{W}"
     ?𝒳 "mathcal{X}" ?𝒴 "mathcal{Y}" ?𝒵 "mathcal{Z}"
     ?𝒶 "mathcal{a}" ?𝒷 "mathcal{b}" ?𝒸 "mathcal{c}"
     ?𝒹 "mathcal{d}" ?𝒻 "mathcal{f}" ?𝒽 "mathcal{h}"
     ?𝒾 "mathcal{i}" ?𝒿 "mathcal{j}" ?𝓀 "mathcal{k}"
     ?𝓁 "mathcal{l}" ?𝓂 "mathcal{m}" ?𝓃 "mathcal{n}"
     ?𝓅 "mathcal{p}" ?𝓆 "mathcal{q}" ?𝓇 "mathcal{r}"
     ?𝓈 "mathcal{s}" ?𝓉 "mathcal{t}" ?𝓊 "mathcal{u}"
     ?𝓋 "mathcal{v}" ?𝓌 "mathcal{w}" ?𝓍 "mathcal{x}"
     ?𝓎 "mathcal{y}" ?𝓏 "mathcal{z}" ?𝔄 "mathfrak{A}"
     ?𝔅 "mathfrak{B}" ?𝔇 "mathfrak{D}"
     ?𝔈 "mathfrak{E}" ?𝔉 "mathfrak{F}"
     ?𝔊 "mathfrak{G}" ?𝔍 "mathfrak{J}"
     ?𝔎 "mathfrak{K}" ?𝔏 "mathfrak{L}"
     ?𝔐 "mathfrak{M}" ?𝔑 "mathfrak{N}"
     ?𝔒 "mathfrak{O}" ?𝔓 "mathfrak{P}"
     ?𝔔 "mathfrak{Q}" ?𝔖 "mathfrak{S}"
     ?𝔗 "mathfrak{T}" ?𝔘 "mathfrak{U}"
     ?𝔙 "mathfrak{V}" ?𝔚 "mathfrak{W}"
     ?𝔛 "mathfrak{X}" ?𝔜 "mathfrak{Y}"
     ?𝔞 "mathfrak{a}" ?𝔟 "mathfrak{b}"
     ?𝔠 "mathfrak{c}" ?𝔡 "mathfrak{d}"
     ?𝔢 "mathfrak{e}" ?𝔣 "mathfrak{f}"
     ?𝔤 "mathfrak{g}" ?𝔥 "mathfrak{h}"
     ?𝔦 "mathfrak{i}" ?𝔧 "mathfrak{j}"
     ?𝔨 "mathfrak{k}" ?𝔩 "mathfrak{l}"
     ?𝔪 "mathfrak{m}" ?𝔫 "mathfrak{n}"
     ?𝔬 "mathfrak{o}" ?𝔭 "mathfrak{p}"
     ?𝔮 "mathfrak{q}" ?𝔯 "mathfrak{r}"
     ?𝔰 "mathfrak{s}" ?𝔱 "mathfrak{t}"
     ?𝔲 "mathfrak{u}" ?𝔳 "mathfrak{v}"
     ?𝔴 "mathfrak{w}" ?𝔵 "mathfrak{x}"
     ?𝔶 "mathfrak{y}" ?𝔷 "mathfrak{z}" ?𝔸 "mathbb{A}"
     ?𝔹 "mathbb{B}" ?𝔻 "mathbb{D}" ?𝔼 "mathbb{E}"
     ?𝔽 "mathbb{F}" ?𝔾 "mathbb{G}" ?𝕀 "mathbb{I}"
     ?𝕁 "mathbb{J}" ?𝕂 "mathbb{K}" ?𝕃 "mathbb{L}"
     ?𝕄 "mathbb{M}" ?𝕆 "mathbb{O}" ?𝕊 "mathbb{S}"
     ?𝕋 "mathbb{T}" ?𝕌 "mathbb{U}" ?𝕍 "mathbb{V}"
     ?𝕎 "mathbb{W}" ?𝕏 "mathbb{X}" ?𝕐 "mathbb{Y}"
     ?𝕒 "mathbb{a}" ?𝕓 "mathbb{b}" ?𝕔 "mathbb{c}"
     ?𝕕 "mathbb{d}" ?𝕖 "mathbb{e}" ?𝕗 "mathbb{f}"
     ?𝕘 "mathbb{g}" ?𝕙 "mathbb{h}" ?𝕚 "mathbb{i}"
     ?𝕛 "mathbb{j}" ?𝕜 "mathbb{k}" ?𝕝 "mathbb{l}"
     ?𝕞 "mathbb{m}" ?𝕟 "mathbb{n}" ?𝕠 "mathbb{o}"
     ?𝕡 "mathbb{p}" ?𝕢 "mathbb{q}" ?𝕣 "mathbb{r}"
     ?𝕤 "mathbb{s}" ?𝕥 "mathbb{t}" ?𝕦 "mathbb{u}"
     ?𝕧 "mathbb{v}" ?𝕨 "mathbb{w}" ?𝕩 "mathbb{x}"
     ?𝕪 "mathbb{y}" ?𝕫 "mathbb{z}" ?𝖠 "mathsf{A}"
     ?𝖡 "mathsf{B}" ?𝖢 "mathsf{C}" ?𝖣 "mathsf{D}"
     ?𝖤 "mathsf{E}" ?𝖥 "mathsf{F}" ?𝖦 "mathsf{G}"
     ?𝖧 "mathsf{H}" ?𝖨 "mathsf{I}" ?𝖩 "mathsf{J}"
     ?𝖪 "mathsf{K}" ?𝖫 "mathsf{L}" ?𝖬 "mathsf{M}"
     ?𝖭 "mathsf{N}" ?𝖮 "mathsf{O}" ?𝖯 "mathsf{P}"
     ?𝖰 "mathsf{Q}" ?𝖱 "mathsf{R}" ?𝖲 "mathsf{S}"
     ?𝖳 "mathsf{T}" ?𝖴 "mathsf{U}" ?𝖵 "mathsf{V}"
     ?𝖶 "mathsf{W}" ?𝖷 "mathsf{X}" ?𝖸 "mathsf{Y}"
     ?𝖹 "mathsf{Z}" ?𝖺 "mathsf{a}" ?𝖻 "mathsf{b}"
     ?𝖼 "mathsf{c}" ?𝖽 "mathsf{d}" ?𝖾 "mathsf{e}"
     ?𝖿 "mathsf{f}" ?𝗀 "mathsf{g}" ?𝗁 "mathsf{h}"
     ?𝗂 "mathsf{i}" ?𝗃 "mathsf{j}" ?𝗄 "mathsf{k}"
     ?𝗅 "mathsf{l}" ?𝗆 "mathsf{m}" ?𝗇 "mathsf{n}"
     ?𝗈 "mathsf{o}" ?𝗉 "mathsf{p}" ?𝗊 "mathsf{q}"
     ?𝗋 "mathsf{r}" ?𝗌 "mathsf{s}" ?𝗍 "mathsf{t}"
     ?𝗎 "mathsf{u}" ?𝗏 "mathsf{v}" ?𝗐 "mathsf{w}"
     ?𝗑 "mathsf{x}" ?𝗒 "mathsf{y}" ?𝗓 "mathsf{z}"
     ?𝗔 "mathsfbf{A}" ?𝗕 "mathsfbf{B}"
     ?𝗖 "mathsfbf{C}" ?𝗗 "mathsfbf{D}"
     ?𝗘 "mathsfbf{E}" ?𝗙 "mathsfbf{F}"
     ?𝗚 "mathsfbf{G}" ?𝗛 "mathsfbf{H}"
     ?𝗜 "mathsfbf{I}" ?𝗝 "mathsfbf{J}"
     ?𝗞 "mathsfbf{K}" ?𝗟 "mathsfbf{L}"
     ?𝗠 "mathsfbf{M}" ?𝗡 "mathsfbf{N}"
     ?𝗢 "mathsfbf{O}" ?𝗣 "mathsfbf{P}"
     ?𝗤 "mathsfbf{Q}" ?𝗥 "mathsfbf{R}"
     ?𝗦 "mathsfbf{S}" ?𝗧 "mathsfbf{T}"
     ?𝗨 "mathsfbf{U}" ?𝗩 "mathsfbf{V}"
     ?𝗪 "mathsfbf{W}" ?𝗫 "mathsfbf{X}"
     ?𝗬 "mathsfbf{Y}" ?𝗭 "mathsfbf{Z}"
     ?𝗮 "mathsfbf{a}" ?𝗯 "mathsfbf{b}"
     ?𝗰 "mathsfbf{c}" ?𝗱 "mathsfbf{d}"
     ?𝗲 "mathsfbf{e}" ?𝗳 "mathsfbf{f}"
     ?𝗴 "mathsfbf{g}" ?𝗵 "mathsfbf{h}"
     ?𝗶 "mathsfbf{i}" ?𝗷 "mathsfbf{j}"
     ?𝗸 "mathsfbf{k}" ?𝗹 "mathsfbf{l}"
     ?𝗺 "mathsfbf{m}" ?𝗻 "mathsfbf{n}"
     ?𝗼 "mathsfbf{o}" ?𝗽 "mathsfbf{p}"
     ?𝗾 "mathsfbf{q}" ?𝗿 "mathsfbf{r}"
     ?𝘀 "mathsfbf{s}" ?𝘁 "mathsfbf{t}"
     ?𝘂 "mathsfbf{u}" ?𝘃 "mathsfbf{v}"
     ?𝘄 "mathsfbf{w}" ?𝘅 "mathsfbf{x}"
     ?𝘆 "mathsfbf{y}" ?𝘇 "mathsfbf{z}"
     ?𝘈 "mathsfit{A}" ?𝘉 "mathsfit{B}"
     ?𝘊 "mathsfit{C}" ?𝘋 "mathsfit{D}"
     ?𝘌 "mathsfit{E}" ?𝘍 "mathsfit{F}"
     ?𝘎 "mathsfit{G}" ?𝘏 "mathsfit{H}"
     ?𝘐 "mathsfit{I}" ?𝘑 "mathsfit{J}"
     ?𝘒 "mathsfit{K}" ?𝘓 "mathsfit{L}"
     ?𝘔 "mathsfit{M}" ?𝘕 "mathsfit{N}"
     ?𝘖 "mathsfit{O}" ?𝘗 "mathsfit{P}"
     ?𝘘 "mathsfit{Q}" ?𝘙 "mathsfit{R}"
     ?𝘚 "mathsfit{S}" ?𝘛 "mathsfit{T}"
     ?𝘜 "mathsfit{U}" ?𝘝 "mathsfit{V}"
     ?𝘞 "mathsfit{W}" ?𝘟 "mathsfit{X}"
     ?𝘠 "mathsfit{Y}" ?𝘡 "mathsfit{Z}"
     ?𝘢 "mathsfit{a}" ?𝘣 "mathsfit{b}"
     ?𝘤 "mathsfit{c}" ?𝘥 "mathsfit{d}"
     ?𝘦 "mathsfit{e}" ?𝘧 "mathsfit{f}"
     ?𝘨 "mathsfit{g}" ?𝘩 "mathsfit{h}"
     ?𝘪 "mathsfit{i}" ?𝘫 "mathsfit{j}"
     ?𝘬 "mathsfit{k}" ?𝘭 "mathsfit{l}"
     ?𝘮 "mathsfit{m}" ?𝘯 "mathsfit{n}"
     ?𝘰 "mathsfit{o}" ?𝘱 "mathsfit{p}"
     ?𝘲 "mathsfit{q}" ?𝘳 "mathsfit{r}"
     ?𝘴 "mathsfit{s}" ?𝘵 "mathsfit{t}"
     ?𝘶 "mathsfit{u}" ?𝘷 "mathsfit{v}"
     ?𝘸 "mathsfit{w}" ?𝘹 "mathsfit{x}"
     ?𝘺 "mathsfit{y}" ?𝘻 "mathsfit{z}"
     ?𝘼 "mathsfbfit{A}" ?𝘽 "mathsfbfit{B}"
     ?𝘾 "mathsfbfit{C}" ?𝘿 "mathsfbfit{D}"
     ?𝙀 "mathsfbfit{E}" ?𝙁 "mathsfbfit{F}"
     ?𝙂 "mathsfbfit{G}" ?𝙃 "mathsfbfit{H}"
     ?𝙄 "mathsfbfit{I}" ?𝙅 "mathsfbfit{J}"
     ?𝙆 "mathsfbfit{K}" ?𝙇 "mathsfbfit{L}"
     ?𝙈 "mathsfbfit{M}" ?𝙉 "mathsfbfit{N}"
     ?𝙊 "mathsfbfit{O}" ?𝙋 "mathsfbfit{P}"
     ?𝙌 "mathsfbfit{Q}" ?𝙍 "mathsfbfit{R}"
     ?𝙎 "mathsfbfit{S}" ?𝙏 "mathsfbfit{T}"
     ?𝙐 "mathsfbfit{U}" ?𝙑 "mathsfbfit{V}"
     ?𝙒 "mathsfbfit{W}" ?𝙓 "mathsfbfit{X}"
     ?𝙔 "mathsfbfit{Y}" ?𝙕 "mathsfbfit{Z}"
     ?𝙖 "mathsfbfit{a}" ?𝙗 "mathsfbfit{b}"
     ?𝙘 "mathsfbfit{c}" ?𝙙 "mathsfbfit{d}"
     ?𝙚 "mathsfbfit{e}" ?𝙛 "mathsfbfit{f}"
     ?𝙜 "mathsfbfit{g}" ?𝙝 "mathsfbfit{h}"
     ?𝙞 "mathsfbfit{i}" ?𝙟 "mathsfbfit{j}"
     ?𝙠 "mathsfbfit{k}" ?𝙡 "mathsfbfit{l}"
     ?𝙢 "mathsfbfit{m}" ?𝙣 "mathsfbfit{n}"
     ?𝙤 "mathsfbfit{o}" ?𝙥 "mathsfbfit{p}"
     ?𝙦 "mathsfbfit{q}" ?𝙧 "mathsfbfit{r}"
     ?𝙨 "mathsfbfit{s}" ?𝙩 "mathsfbfit{t}"
     ?𝙪 "mathsfbfit{u}" ?𝙫 "mathsfbfit{v}"
     ?𝙬 "mathsfbfit{w}" ?𝙭 "mathsfbfit{x}"
     ?𝙮 "mathsfbfit{y}" ?𝙯 "mathsfbfit{z}"
     ?𝙰 "mathtt{A}" ?𝙱 "mathtt{B}" ?𝙲 "mathtt{C}"
     ?𝙳 "mathtt{D}" ?𝙴 "mathtt{E}" ?𝙵 "mathtt{F}"
     ?𝙶 "mathtt{G}" ?𝙷 "mathtt{H}" ?𝙸 "mathtt{I}"
     ?𝙹 "mathtt{J}" ?𝙺 "mathtt{K}" ?𝙻 "mathtt{L}"
     ?𝙼 "mathtt{M}" ?𝙽 "mathtt{N}" ?𝙾 "mathtt{O}"
     ?𝙿 "mathtt{P}" ?𝚀 "mathtt{Q}" ?𝚁 "mathtt{R}"
     ?𝚂 "mathtt{S}" ?𝚃 "mathtt{T}" ?𝚄 "mathtt{U}"
     ?𝚅 "mathtt{V}" ?𝚆 "mathtt{W}" ?𝚇 "mathtt{X}"
     ?𝚈 "mathtt{Y}" ?𝚉 "mathtt{Z}" ?𝚊 "mathtt{a}"
     ?𝚋 "mathtt{b}" ?𝚌 "mathtt{c}" ?𝚍 "mathtt{d}"
     ?𝚎 "mathtt{e}" ?𝚏 "mathtt{f}" ?𝚐 "mathtt{g}"
     ?𝚑 "mathtt{h}" ?𝚒 "mathtt{i}" ?𝚓 "mathtt{j}"
     ?𝚔 "mathtt{k}" ?𝚕 "mathtt{l}" ?𝚖 "mathtt{m}"
     ?𝚗 "mathtt{n}" ?𝚘 "mathtt{o}" ?𝚙 "mathtt{p}"
     ?𝚚 "mathtt{q}" ?𝚛 "mathtt{r}" ?𝚜 "mathtt{s}"
     ?𝚝 "mathtt{t}" ?𝚞 "mathtt{u}" ?𝚟 "mathtt{v}"
     ?𝚠 "mathtt{w}" ?𝚡 "mathtt{x}" ?𝚢 "mathtt{y}"
     ?𝚣 "mathtt{z}" ?𝚤 "imath" ?𝚥 "jmath"
     ?𝚪 "mathbf{\\Gamma}" ?𝚫 "mathbf{\\Delta}"
     ?𝚯 "mathbf{\\Theta}" ?𝚲 "mathbf{\\Lambda}"
     ?𝚵 "mathbf{\\Xi}" ?𝚷 "mathbf{\\Pi}"
     ?𝚺 "mathbf{\\Sigma}" ?𝚼 "mathbf{\\Upsilon}"
     ?𝚽 "mathbf{\\Phi}" ?𝚿 "mathbf{\\Psi}"
     ?𝛀 "mathbf{\\Omega}" ?𝛂 "mathbf{\\alpha}"
     ?𝛃 "mathbf{\\beta}" ?𝛄 "mathbf{\\gamma}"
     ?𝛅 "mathbf{\\delta}" ?𝛆 "mathbf{\\varepsilon}"
     ?𝛇 "mathbf{\\zeta}" ?𝛈 "mathbf{\\eta}"
     ?𝛉 "mathbf{\\theta}" ?𝛊 "mathbf{\\iota}"
     ?𝛋 "mathbf{\\kappa}" ?𝛌 "mathbf{\\lambda}"
     ?𝛍 "mathbf{\\mu}" ?𝛎 "mathbf{\\nu}"
     ?𝛏 "mathbf{\\xi}" ?𝛑 "mathbf{\\pi}"
     ?𝛒 "mathbf{\\rho}" ?𝛓 "mathbf{\\varsigma}"
     ?𝛔 "mathbf{\\sigma}" ?𝛕 "mathbf{\\tau}"
     ?𝛖 "mathbf{\\upsilon}" ?𝛗 "mathbf{\\varphi}"
     ?𝛘 "mathbf{\\chi}" ?𝛙 "mathbf{\\psi}"
     ?𝛚 "mathbf{\\omega}" ?𝛜 "mathbf{\\epsilon}"
     ?𝛝 "mathbf{\\vartheta}" ?𝛟 "mathbf{\\phi}"
     ?𝛠 "mathbf{\\varrho}" ?𝛡 "mathbf{\\varpi}"
     ?𝛤 "Gamma" ?𝛥 "Delta" ?𝛩 "Theta" ?𝛬 "Lambda"
     ?𝛯 "Xi" ?𝛱 "Pi" ?𝛴 "Sigma" ?𝛶 "Upsilon"
     ?𝛷 "Phi" ?𝛹 "Psi" ?𝛺 "Omega" ?𝛼 "alpha"
     ?𝛽 "beta" ?𝛾 "gamma" ?𝛿 "delta"
     ?𝜀 "varepsilon" ?𝜁 "zeta" ?𝜂 "eta"
     ?𝜃 "theta" ?𝜄 "iota" ?𝜅 "kappa" ?𝜆 "lambda"
     ?𝜇 "mu" ?𝜈 "nu" ?𝜉 "xi" ?𝜋 "pi" ?𝜌 "rho"
     ?𝜍 "varsigma" ?𝜎 "sigma" ?𝜏 "tau"
     ?𝜐 "upsilon" ?𝜑 "varphi" ?𝜒 "chi" ?𝜓 "psi"
     ?𝜔 "omega" ?𝜕 "partial" ?𝜖 "epsilon"
     ?𝜗 "vartheta" ?ϰ "varkappa" ?𝜙 "phi" ; ?𝜘
     ?𝜚 "varrho" ?𝜛 "varpi" ?𝜞 "mathbfit{\\Gamma}"
     ?𝜟 "mathbfit{\\Delta}" ?𝜣 "mathbfit{\\Theta}"
     ?𝜦 "mathbfit{\\Lambda}" ?𝜩 "mathbfit{\\Xi}"
     ?𝜫 "mathbfit{\\Pi}" ?𝜮 "mathbfit{\\Sigma}"
     ?𝜰 "mathbfit{\\Upsilon}" ?𝜱 "mathbfit{\\Phi}"
     ?𝜳 "mathbfit{\\Psi}" ?𝜴 "mathbfit{\\Omega}"
     ?𝜶 "mathbfit{\\alpha}" ?𝜷 "mathbfit{\\beta}"
     ?𝜸 "mathbfit{\\gamma}" ?𝜹 "mathbfit{\\delta}"
     ?𝜺 "mathbfit{\\varepsilon}" ?𝜻 "mathbfit{\\zeta}"
     ?𝜼 "mathbfit{\\eta}" ?𝜽 "mathbfit{\\theta}"
     ?𝜾 "mathbfit{\\iota}" ?𝜿 "mathbfit{\\kappa}"
     ?𝝀 "mathbfit{\\lambda}" ?𝝁 "mathbfit{\\mu}"
     ?𝝂 "mathbfit{\\nu}" ?𝝃 "mathbfit{\\xi}"
     ?𝝅 "mathbfit{\\pi}" ?𝝆 "mathbfit{\\rho}"
     ?𝝇 "mathbfit{\\varsigma}" ?𝝈 "mathbfit{\\sigma}"
     ?𝝉 "mathbfit{\\tau}" ?𝝊 "mathbfit{\\upsilon}"
     ?𝝋 "mathbfit{\\varphi}" ?𝝌 "mathbfit{\\chi}"
     ?𝝍 "mathbfit{\\psi}" ?𝝎 "mathbfit{\\omega}"
     ?𝝐 "mathbfit{\\epsilon}" ?𝝑 "mathbfit{\\vartheta}"
     ?𝝓 "mathbfit{\\phi}" ?𝝔 "mathbfit{\\varrho}"
     ?𝝕 "mathbfit{\\varpi}" ?𝝘 "mathsfbf{\\Gamma}"
     ?𝝙 "mathsfbf{\\Delta}" ?𝝝 "mathsfbf{\\Theta}"
     ?𝝠 "mathsfbf{\\Lambda}" ?𝝣 "mathsfbf{\\Xi}"
     ?𝝥 "mathsfbf{\\Pi}" ?𝝨 "mathsfbf{\\Sigma}"
     ?𝝪 "mathsfbf{\\Upsilon}" ?𝝫 "mathsfbf{\\Phi}"
     ?𝝭 "mathsfbf{\\Psi}" ?𝝮 "mathsfbf{\\Omega}"
     ?𝝰 "mathsfbf{\\alpha}" ?𝝱 "mathsfbf{\\beta}"
     ?𝝲 "mathsfbf{\\gamma}" ?𝝳 "mathsfbf{\\delta}"
     ?𝝴 "mathsfbf{\\varepsilon}" ?𝝵 "mathsfbf{\\zeta}"
     ?𝝶 "mathsfbf{\\eta}" ?𝝷 "mathsfbf{\\theta}"
     ?𝝸 "mathsfbf{\\iota}" ?𝝹 "mathsfbf{\\kappa}"
     ?𝝺 "mathsfbf{\\lambda}" ?𝝻 "mathsfbf{\\mu}"
     ?𝝼 "mathsfbf{\\nu}" ?𝝽 "mathsfbf{\\xi}"
     ?𝝿 "mathsfbf{\\pi}" ?𝞀 "mathsfbf{\\rho}"
     ?𝞁 "mathsfbf{\\varsigma}" ?𝞂 "mathsfbf{\\sigma}"
     ?𝞃 "mathsfbf{\\tau}" ?𝞄 "mathsfbf{\\upsilon}"
     ?𝞅 "mathsfbf{\\varphi}" ?𝞆 "mathsfbf{\\chi}"
     ?𝞇 "mathsfbf{\\psi}" ?𝞈 "mathsfbf{\\omega}"
     ?𝞊 "mathsfbf{\\epsilon}" ?𝞋 "mathsfbf{\\vartheta}"
     ?𝞍 "mathsfbf{\\phi}" ?𝞎 "mathsfbf{\\varrho}"
     ?𝞏 "mathsfbf{\\varpi}" ?𝞒 "mathsfbfit{\\Gamma}"
     ?𝞓 "mathsfbfit{\\Delta}" ?𝞗 "mathsfbfit{\\Theta}"
     ?𝞚 "mathsfbfit{\\Lambda}" ?𝞝 "mathsfbfit{\\Xi}"
     ?𝞟 "mathsfbfit{\\Pi}" ?𝞢 "mathsfbfit{\\Sigma}"
     ?𝞤 "mathsfbfit{\\Upsilon}" ?𝞥 "mathsfbfit{\\Phi}"
     ?𝞧 "mathsfbfit{\\Psi}" ?𝞨 "mathsfbfit{\\Omega}"
     ?𝞪 "mathsfbfit{\\alpha}" ?𝞫 "mathsfbfit{\\beta}"
     ?𝞬 "mathsfbfit{\\gamma}" ?𝞭 "mathsfbfit{\\delta}"
     ?𝞮 "mathsfbfit{\\varepsilon}"
     ?𝞯 "mathsfbfit{\\zeta}" ?𝞰 "mathsfbfit{\\eta}"
     ?𝞱 "mathsfbfit{\\theta}" ?𝞲 "mathsfbfit{\\iota}"
     ?𝞳 "mathsfbfit{\\kappa}" ?𝞴 "mathsfbfit{\\lambda}"
     ?𝞵 "mathsfbfit{\\mu}" ?𝞶 "mathsfbfit{\\nu}"
     ?𝞷 "mathsfbfit{\\xi}" ?𝞹 "mathsfbfit{\\pi}"
     ?𝞺 "mathsfbfit{\\rho}" ?𝞻 "mathsfbfit{\\varsigma}"
     ?𝞼 "mathsfbfit{\\sigma}" ?𝞽 "mathsfbfit{\\tau}"
     ?𝞾 "mathsfbfit{\\upsilon}" ?𝞿 "mathsfbfit{\\varphi}"
     ?𝟀 "mathsfbfit{\\chi}" ?𝟁 "mathsfbfit{\\psi}"
     ?𝟂 "mathsfbfit{\\omega}" ?𝟄 "mathsfbfit{\\epsilon}"
     ?𝟅 "mathsfbfit{\\vartheta}" ?𝟇 "mathsfbfit{\\phi}"
     ?𝟈 "mathsfbfit{\\varrho}" ?𝟉 "mathsfbfit{\\varpi}"
     ?𝟎 "mathbf{0}" ?𝟏 "mathbf{1}" ?𝟐 "mathbf{2}"
     ?𝟑 "mathbf{3}" ?𝟒 "mathbf{4}" ?𝟓 "mathbf{5}"
     ?𝟔 "mathbf{6}" ?𝟕 "mathbf{7}" ?𝟖 "mathbf{8}"
     ?𝟗 "mathbf{9}" ?𝟘 "mathbb{0}" ?𝟙 "mathbb{1}"
     ?𝟚 "mathbb{2}" ?𝟛 "mathbb{3}" ?𝟜 "mathbb{4}"
     ?𝟝 "mathbb{5}" ?𝟞 "mathbb{6}" ?𝟟 "mathbb{7}"
     ?𝟠 "mathbb{8}" ?𝟡 "mathbb{9}" ?𝟢 "mathsf{0}"
     ?𝟣 "mathsf{1}" ?𝟤 "mathsf{2}" ?𝟥 "mathsf{3}"
     ?𝟦 "mathsf{4}" ?𝟧 "mathsf{5}" ?𝟨 "mathsf{6}"
     ?𝟩 "mathsf{7}" ?𝟪 "mathsf{8}" ?𝟫 "mathsf{9}"
     ?𝟬 "mathsfbf{0}" ?𝟭 "mathsfbf{1}"
     ?𝟮 "mathsfbf{2}" ?𝟯 "mathsfbf{3}"
     ?𝟰 "mathsfbf{4}" ?𝟱 "mathsfbf{5}"
     ?𝟲 "mathsfbf{6}" ?𝟳 "mathsfbf{7}"
     ?𝟴 "mathsfbf{8}" ?𝟵 "mathsfbf{9}" ?𝟶 "mathtt{0}"
     ?𝟷 "mathtt{1}" ?𝟸 "mathtt{2}" ?𝟹 "mathtt{3}"
     ?𝟺 "mathtt{4}" ?𝟻 "mathtt{5}" ?𝟼 "mathtt{6}"
     ?𝟽 "mathtt{7}" ?𝟾 "mathtt{8}" ?𝟿 "mathtt{9}"))
  "UCS to TeX commands table.  Taken from 'unimathsymbols.txt'.")

(defvar math-symbols-tex-regexp 
  (let (syms)
    (maphash (lambda (_k v) (push v syms)) math-symbols-tex-table)
    (regexp-opt syms)))

(defvar math-symbols-from-tex-table
  (let ((table (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (puthash v k table)) math-symbols-tex-table)
    table))

(defvar math-symbols
  (let (syms)
    (maphash (lambda (k v)
               (unless (string-match "{" v)
                 (push (format "%s (%c)" v k) syms)))
             math-symbols-tex-table) syms))

(defvar math-symbols-style-alist
  '(("BOLD" . math-symbols-bold-table)
    ("BOLD FRAKTUR" . math-symbols-bold-fraktur-table)
    ("BOLD ITALIC" . math-symbols-bold-italic-table) 
    ("BOLD SCRIPT" . math-symbols-bold-script-table) 
    ("DOUBLE-STRUCK" . math-symbols-double-struck-table) 
    ("FRAKTUR" . math-symbols-fraktur-table) ;; "BLACK-LETTER
    ("ITALIC" . math-symbols-italic-table)
    ("MONOSPACE" . math-symbols-monospace-table) 
    ("SANS-SERIF" . math-symbols-sans-serif-table) 
    ("SANS-SERIF BOLD" . math-symbols-sans-serif-bold-table)
    ("SANS-SERIF BOLD ITALIC" . math-symbols-sans-serif-bold-italic-table)
    ("SANS-SERIF ITALIC" . math-symbols-sans-serif-italic-table) 
    ("SCRIPT" . math-symbols-script-table) 
    ("SUBSCRIPT" . math-symbols-subscript-table)
    ("SUPERSCRIPT" . math-symbols-superscript-table)))

(defvar math-symbols-bold-table
  #s(hash-table 
     data
     (?0 ?𝟎 ?1 ?𝟏 ?2 ?𝟐 ?3 ?𝟑 ?4 ?𝟒 ?5 ?𝟓 ?6 ?𝟔 ?7 ?𝟕 ?8 ?𝟖
      ?9 ?𝟗 ?A ?𝐀 ?B ?𝐁 ?C ?𝐂 ?D ?𝐃 ?E ?𝐄 ?F ?𝐅 ?G ?𝐆 ?H ?𝐇 ?I
      ?𝐈 ?J ?𝐉 ?K ?𝐊 ?L ?𝐋 ?M ?𝐌 ?N ?𝐍 ?O ?𝐎 ?P ?𝐏 ?Q ?𝐐 ?R ?𝐑
      ?S ?𝐒 ?T ?𝐓 ?U ?𝐔 ?V ?𝐕 ?W ?𝐖 ?X ?𝐗 ?Y ?𝐘 ?Z ?𝐙 ?a ?𝐚 ?b
      ?𝐛 ?c ?𝐜 ?d ?𝐝 ?e ?𝐞 ?f ?𝐟 ?g ?𝐠 ?h ?𝐡 ?i ?𝐢 ?j ?𝐣 ?k ?𝐤
      ?l ?𝐥 ?m ?𝐦 ?n ?𝐧 ?o ?𝐨 ?p ?𝐩 ?q ?𝐪 ?r ?𝐫 ?s ?𝐬 ?t ?𝐭 ?u
      ?𝐮 ?v ?𝐯 ?w ?𝐰 ?x ?𝐱 ?y ?𝐲 ?z ?𝐳 ?Α ?𝚨 ?Β ?𝚩 ?Γ ?𝚪 ?Δ
      ?𝚫 ?Ε ?𝚬 ?Ζ ?𝚭 ?Η ?𝚮 ?Θ ?𝚯 ?Ι ?𝚰 ?Κ ?𝚱 ?Λ ?𝚲 ?Μ
      ?𝚳 ?Ν ?𝚴 ?Ξ ?𝚵 ?Ο ?𝚶 ?Π ?𝚷 ?Ρ ?𝚸 ?Σ ?𝚺 ?Τ ?𝚻 ?Υ
      ?𝚼 ?Φ ?𝚽 ?Χ ?𝚾 ?Ψ ?𝚿 ?Ω ?𝛀 ?α ?𝛂 ?β ?𝛃 ?γ ?𝛄 ?δ
      ?𝛅 ?ε ?𝛆 ?ζ ?𝛇 ?η ?𝛈 ?θ ?𝛉 ?ι ?𝛊 ?κ ?𝛋 ?λ ?𝛌 ?μ
      ?𝛍 ?ν ?𝛎 ?ξ ?𝛏 ?ο ?𝛐 ?π ?𝛑 ?ρ ?𝛒 ?ς ?𝛓 ?σ ?𝛔 ?τ
      ?𝛕 ?υ ?𝛖 ?φ ?𝛗 ?χ ?𝛘 ?ψ ?𝛙 ?ω ?𝛚 ?ϑ ?𝛝 ?ϕ ?𝛟 ?ϖ ?𝛡
      ?Ϝ ?𝟊 ?ϝ ?𝟋 ?ϰ ?𝛞 ?ϱ ?𝛠 ?ϴ ?𝚹 ?ϵ ?𝛜 ?∇ ?𝛁)))

(defvar math-symbols-bold-fraktur-table
  #s(hash-table 
     data
     (?A ?𝕬 ?B ?𝕭 ?C ?𝕮 ?D ?𝕯 ?E ?𝕰 ?F ?𝕱 ?G ?𝕲 ?H ?𝕳 ?I ?𝕴
      ?J ?𝕵 ?K ?𝕶 ?L ?𝕷 ?M ?𝕸 ?N ?𝕹 ?O ?𝕺 ?P ?𝕻 ?Q ?𝕼 ?R ?𝕽 ?S
      ?𝕾 ?T ?𝕿 ?U ?𝖀 ?V ?𝖁 ?W ?𝖂 ?X ?𝖃 ?Y ?𝖄 ?Z ?𝖅 ?a ?𝖆 ?b ?𝖇
      ?c ?𝖈 ?d ?𝖉 ?e ?𝖊 ?f ?𝖋 ?g ?𝖌 ?h ?𝖍 ?i ?𝖎 ?j ?𝖏 ?k ?𝖐 ?l
      ?𝖑 ?m ?𝖒 ?n ?𝖓 ?o ?𝖔 ?p ?𝖕 ?q ?𝖖 ?r ?𝖗 ?s ?𝖘 ?t ?𝖙 ?u ?𝖚
      ?v ?𝖛 ?w ?𝖜 ?x ?𝖝 ?y ?𝖞 ?z ?𝖟)))

(defvar math-symbols-bold-italic-table
  #s(hash-table
     data
     (?A ?𝑨 ?B ?𝑩 ?C ?𝑪 ?D ?𝑫 ?E ?𝑬 ?F ?𝑭 ?G ?𝑮 ?H ?𝑯 ?I ?𝑰
      ?J ?𝑱 ?K ?𝑲 ?L ?𝑳 ?M ?𝑴 ?N ?𝑵 ?O ?𝑶 ?P ?𝑷 ?Q ?𝑸 ?R ?𝑹 ?S
      ?𝑺 ?T ?𝑻 ?U ?𝑼 ?V ?𝑽 ?W ?𝑾 ?X ?𝑿 ?Y ?𝒀 ?Z ?𝒁 ?a ?𝒂 ?b ?𝒃
      ?c ?𝒄 ?d ?𝒅 ?e ?𝒆 ?f ?𝒇 ?g ?𝒈 ?h ?𝒉 ?i ?𝒊 ?j ?𝒋 ?k ?𝒌 ?l
      ?𝒍 ?m ?𝒎 ?n ?𝒏 ?o ?𝒐 ?p ?𝒑 ?q ?𝒒 ?r ?𝒓 ?s ?𝒔 ?t ?𝒕 ?u ?𝒖
      ?v ?𝒗 ?w ?𝒘 ?x ?𝒙 ?y ?𝒚 ?z ?𝒛 ?Α ?𝜜 ?Β ?𝜝 ?Γ ?𝜞 ?Δ
      ?𝜟 ?Ε ?𝜠 ?Ζ ?𝜡 ?Η ?𝜢 ?Θ ?𝜣 ?Ι ?𝜤 ?Κ ?𝜥 ?Λ ?𝜦 ?Μ
      ?𝜧 ?Ν ?𝜨 ?Ξ ?𝜩 ?Ο ?𝜪 ?Π ?𝜫 ?Ρ ?𝜬 ?Σ ?𝜮 ?Τ ?𝜯 ?Υ
      ?𝜰 ?Φ ?𝜱 ?Χ ?𝜲 ?Ψ ?𝜳 ?Ω ?𝜴 ?α ?𝜶 ?β ?𝜷 ?γ ?𝜸 ?δ
      ?𝜹 ?ε ?𝜺 ?ζ ?𝜻 ?η ?𝜼 ?θ ?𝜽 ?ι ?𝜾 ?κ ?𝜿 ?λ ?𝝀 ?μ
      ?𝝁 ?ν ?𝝂 ?ξ ?𝝃 ?ο ?𝝄 ?π ?𝝅 ?ρ ?𝝆 ?ς ?𝝇 ?σ ?𝝈 ?τ
      ?𝝉 ?υ ?𝝊 ?φ ?𝝋 ?χ ?𝝌 ?ψ ?𝝍 ?ω ?𝝎 ?ϑ ?𝝑 ?ϕ ?𝝓 ?ϖ ?𝝕
      ?ϰ ?𝝒 ?ϱ ?𝝔 ?ϴ ?𝜭 ?ϵ ?𝝐 ?∇ ?𝜵)))

(defvar math-symbols-bold-script-table
  #s(hash-table
     data
     (?A ?𝓐 ?B ?𝓑 ?C ?𝓒 ?D ?𝓓 ?E ?𝓔 ?F ?𝓕 ?G ?𝓖 ?H ?𝓗 ?I ?𝓘
      ?J ?𝓙 ?K ?𝓚 ?L ?𝓛 ?M ?𝓜 ?N ?𝓝 ?O ?𝓞 ?P ?𝓟 ?Q ?𝓠 ?R ?𝓡 ?S
      ?𝓢 ?T ?𝓣 ?U ?𝓤 ?V ?𝓥 ?W ?𝓦 ?X ?𝓧 ?Y ?𝓨 ?Z ?𝓩 ?a ?𝓪 ?b ?𝓫
      ?c ?𝓬 ?d ?𝓭 ?e ?𝓮 ?f ?𝓯 ?g ?𝓰 ?h ?𝓱 ?i ?𝓲 ?j ?𝓳 ?k ?𝓴 ?l
      ?𝓵 ?m ?𝓶 ?n ?𝓷 ?o ?𝓸 ?p ?𝓹 ?q ?𝓺 ?r ?𝓻 ?s ?𝓼 ?t ?𝓽 ?u ?𝓾
      ?v ?𝓿 ?w ?𝔀 ?x ?𝔁 ?y ?𝔂 ?z ?𝔃)))

(defvar math-symbols-double-struck-table
  #s(hash-table
     data
     (?0 ?𝟘 ?1 ?𝟙 ?2 ?𝟚 ?3 ?𝟛 ?4 ?𝟜 ?5 ?𝟝 ?6 ?𝟞 ?7 ?𝟟 ?8 ?𝟠
      ?9 ?𝟡 ?< ?⟪ ?> ?⟫ ?A ?𝔸 ?B ?𝔹 ?C ?ℂ ?D ?𝔻 ?E ?𝔼 ?F ?𝔽 ?G
      ?𝔾 ?H ?ℍ ?I ?𝕀 ?J ?𝕁 ?K ?𝕂 ?L ?𝕃 ?M ?𝕄 ?N ?ℕ ?O ?𝕆 ?P ?ℙ
      ?Q ?ℚ ?R ?ℝ ?S ?𝕊 ?T ?𝕋 ?U ?𝕌 ?V ?𝕍 ?W ?𝕎 ?X ?𝕏 ?Y ?𝕐 ?Z
      ?ℤ ?[ ?⟦ ?] ?⟧ ?a ?𝕒 ?b ?𝕓 ?c ?𝕔 ?d ?𝕕 ?e ?𝕖 ?f ?𝕗 ?g ?𝕘
      ?h ?𝕙 ?i ?𝕚 ?j ?𝕛 ?k ?𝕜 ?l ?𝕝 ?m ?𝕞 ?n ?𝕟 ?o ?𝕠 ?p ?𝕡 ?q
      ?𝕢 ?r ?𝕣 ?s ?𝕤 ?t ?𝕥 ?u ?𝕦 ?v ?𝕧 ?w ?𝕨 ?x ?𝕩 ?y ?𝕪 ?z ?𝕫
      ?{ ?⟬ ?} ?⟭ ?Γ ?ℾ ?Π ?ℿ ?γ ?ℽ ?π ?ℼ)))

(defvar math-symbols-fraktur-table
  #s(hash-table 
     data
     (?A ?𝔄 ?B ?𝔅 ?C ?ℭ ?D ?𝔇 ?E ?𝔈 ?F ?𝔉 ?G ?𝔊 ?H ?ℌ ?I ?ℑ
      ?J ?𝔍 ?K ?𝔎 ?L ?𝔏 ?M ?𝔐 ?N ?𝔑 ?O ?𝔒 ?P ?𝔓 ?Q ?𝔔 ?R ?ℜ ?S
      ?𝔖 ?T ?𝔗 ?U ?𝔘 ?V ?𝔙 ?W ?𝔚 ?X ?𝔛 ?Y ?𝔜 ?Z ?ℨ ?a ?𝔞 ?b ?𝔟
      ?c ?𝔠 ?d ?𝔡 ?e ?𝔢 ?f ?𝔣 ?g ?𝔤 ?h ?𝔥 ?i ?𝔦 ?j ?𝔧 ?k ?𝔨 ?l
      ?𝔩 ?m ?𝔪 ?n ?𝔫 ?o ?𝔬 ?p ?𝔭 ?q ?𝔮 ?r ?𝔯 ?s ?𝔰 ?t ?𝔱 ?u ?𝔲
      ?v ?𝔳 ?w ?𝔴 ?x ?𝔵 ?y ?𝔶 ?z ?𝔷)))

(defvar math-symbols-italic-table
  #s(hash-table
     data
     (?A ?𝐴 ?B ?𝐵 ?C ?𝐶 ?D ?𝐷 ?E ?𝐸 ?F ?𝐹 ?G ?𝐺 ?H ?𝐻 ?I ?𝐼
      ?J ?𝐽 ?K ?𝐾 ?L ?𝐿 ?M ?𝑀 ?N ?𝑁 ?O ?𝑂 ?P ?𝑃 ?Q ?𝑄 ?R ?𝑅 ?S
      ?𝑆 ?T ?𝑇 ?U ?𝑈 ?V ?𝑉 ?W ?𝑊 ?X ?𝑋 ?Y ?𝑌 ?Z ?𝑍 ?a ?𝑎 ?b ?𝑏
      ?c ?𝑐 ?d ?𝑑 ?e ?𝑒 ?f ?𝑓 ?g ?𝑔 ?h ?ℎ ?i ?𝑖 ?j ?𝑗 ?k ?𝑘 ?l
      ?𝑙 ?m ?𝑚 ?n ?𝑛 ?o ?𝑜 ?p ?𝑝 ?q ?𝑞 ?r ?𝑟 ?s ?𝑠 ?t ?𝑡 ?u ?𝑢
      ?v ?𝑣 ?w ?𝑤 ?x ?𝑥 ?y ?𝑦 ?z ?𝑧 ?ı ?𝚤 ?ȷ ?𝚥 ?Α ?𝛢 ?Β ?𝛣
      ?Γ ?𝛤 ?Δ ?𝛥 ?Ε ?𝛦 ?Ζ ?𝛧 ?Η ?𝛨 ?Θ ?𝛩 ?Ι ?𝛪 ?Κ ?𝛫
      ?Λ ?𝛬 ?Μ ?𝛭 ?Ν ?𝛮 ?Ξ ?𝛯 ?Ο ?𝛰 ?Π ?𝛱 ?Ρ ?𝛲 ?Σ ?𝛴
      ?Τ ?𝛵 ?Υ ?𝛶 ?Φ ?𝛷 ?Χ ?𝛸 ?Ψ ?𝛹 ?Ω ?𝛺 ?α ?𝛼 ?β ?𝛽
      ?γ ?𝛾 ?δ ?𝛿 ?ε ?𝜀 ?ζ ?𝜁 ?η ?𝜂 ?θ ?𝜃 ?ι ?𝜄 ?κ ?𝜅
      ?λ ?𝜆 ?μ ?𝜇 ?ν ?𝜈 ?ξ ?𝜉 ?ο ?𝜊 ?π ?𝜋 ?ρ ?𝜌 ?ς ?𝜍
      ?σ ?𝜎 ?τ ?𝜏 ?υ ?𝜐 ?φ ?𝜑 ?χ ?𝜒 ?ψ ?𝜓 ?ω ?𝜔 ?ϑ ?𝜗
      ?ϕ ?𝜙 ?ϖ ?𝜛 ?ϰ ?𝜘 ?ϱ ?𝜚 ?ϴ ?𝛳 ?ϵ ?𝜖 ?∇ ?𝛻)))

(defvar math-symbols-monospace-table
  #s(hash-table
     data
     (?0 ?𝟶 ?1 ?𝟷 ?2 ?𝟸 ?3 ?𝟹 ?4 ?𝟺 ?5 ?𝟻 ?6 ?𝟼 ?7 ?𝟽 ?8 ?𝟾
      ?9 ?𝟿 ?A ?𝙰 ?B ?𝙱 ?C ?𝙲 ?D ?𝙳 ?E ?𝙴 ?F ?𝙵 ?G ?𝙶 ?H ?𝙷 ?I
      ?𝙸 ?J ?𝙹 ?K ?𝙺 ?L ?𝙻 ?M ?𝙼 ?N ?𝙽 ?O ?𝙾 ?P ?𝙿 ?Q ?𝚀 ?R ?𝚁
      ?S ?𝚂 ?T ?𝚃 ?U ?𝚄 ?V ?𝚅 ?W ?𝚆 ?X ?𝚇 ?Y ?𝚈 ?Z ?𝚉 ?a ?𝚊 ?b
      ?𝚋 ?c ?𝚌 ?d ?𝚍 ?e ?𝚎 ?f ?𝚏 ?g ?𝚐 ?h ?𝚑 ?i ?𝚒 ?j ?𝚓 ?k ?𝚔
      ?l ?𝚕 ?m ?𝚖 ?n ?𝚗 ?o ?𝚘 ?p ?𝚙 ?q ?𝚚 ?r ?𝚛 ?s ?𝚜 ?t ?𝚝 ?u
      ?𝚞 ?v ?𝚟 ?w ?𝚠 ?x ?𝚡 ?y ?𝚢 ?z ?𝚣)))

(defvar math-symbols-sans-serif-table
  #s(hash-table 
     data
     (?( ?⟮ ?) ?⟯ ?0 ?𝟢 ?1 ?𝟣 ?2 ?𝟤 ?3 ?𝟥 ?4 ?𝟦 ?5 ?𝟧 ?6 ?𝟨
      ?7 ?𝟩 ?8 ?𝟪 ?9 ?𝟫 ?< ?⟨ ?> ?⟩ ?A ?𝖠 ?B ?𝖡 ?C ?𝖢 ?D ?𝖣 ?E
      ?𝖤 ?F ?𝖥 ?G ?𝖦 ?H ?𝖧 ?I ?𝖨 ?J ?𝖩 ?K ?𝖪 ?L ?𝖫 ?M ?𝖬 ?N ?𝖭
      ?O ?𝖮 ?P ?𝖯 ?Q ?𝖰 ?R ?𝖱 ?S ?𝖲 ?T ?𝖳 ?U ?𝖴 ?V ?𝖵 ?W ?𝖶 ?X
      ?𝖷 ?Y ?𝖸 ?Z ?𝖹 ?a ?𝖺 ?b ?𝖻 ?c ?𝖼 ?d ?𝖽 ?e ?𝖾 ?f ?𝖿 ?g ?𝗀
      ?h ?𝗁 ?i ?𝗂 ?j ?𝗃 ?k ?𝗄 ?l ?𝗅 ?m ?𝗆 ?n ?𝗇 ?o ?𝗈 ?p ?𝗉 ?q
      ?𝗊 ?r ?𝗋 ?s ?𝗌 ?t ?𝗍 ?u ?𝗎 ?v ?𝗏 ?w ?𝗐 ?x ?𝗑 ?y ?𝗒 ?z
      ?𝗓)))

(defvar math-symbols-sans-serif-bold-table
  #s(hash-table 
     data
     (?0 ?𝟬 ?1 ?𝟭 ?2 ?𝟮 ?3 ?𝟯 ?4 ?𝟰 ?5 ?𝟱 ?6 ?𝟲 ?7 ?𝟳 ?8 ?𝟴
      ?9 ?𝟵 ?A ?𝗔 ?B ?𝗕 ?C ?𝗖 ?D ?𝗗 ?E ?𝗘 ?F ?𝗙 ?G ?𝗚 ?H ?𝗛 ?I
      ?𝗜 ?J ?𝗝 ?K ?𝗞 ?L ?𝗟 ?M ?𝗠 ?N ?𝗡 ?O ?𝗢 ?P ?𝗣 ?Q ?𝗤 ?R ?𝗥
      ?S ?𝗦 ?T ?𝗧 ?U ?𝗨 ?V ?𝗩 ?W ?𝗪 ?X ?𝗫 ?Y ?𝗬 ?Z ?𝗭 ?a ?𝗮 ?b
      ?𝗯 ?c ?𝗰 ?d ?𝗱 ?e ?𝗲 ?f ?𝗳 ?g ?𝗴 ?h ?𝗵 ?i ?𝗶 ?j ?𝗷 ?k ?𝗸
      ?l ?𝗹 ?m ?𝗺 ?n ?𝗻 ?o ?𝗼 ?p ?𝗽 ?q ?𝗾 ?r ?𝗿 ?s ?𝘀 ?t ?𝘁 ?u
      ?𝘂 ?v ?𝘃 ?w ?𝘄 ?x ?𝘅 ?y ?𝘆 ?z ?𝘇 ?Α ?𝝖 ?Β ?𝝗 ?Γ ?𝝘 ?Δ
      ?𝝙 ?Ε ?𝝚 ?Ζ ?𝝛 ?Η ?𝝜 ?Θ ?𝝝 ?Ι ?𝝞 ?Κ ?𝝟 ?Λ ?𝝠 ?Μ
      ?𝝡 ?Ν ?𝝢 ?Ξ ?𝝣 ?Ο ?𝝤 ?Π ?𝝥 ?Ρ ?𝝦 ?Σ ?𝝨 ?Τ ?𝝩 ?Υ
      ?𝝪 ?Φ ?𝝫 ?Χ ?𝝬 ?Ψ ?𝝭 ?Ω ?𝝮 ?α ?𝝰 ?β ?𝝱 ?γ ?𝝲 ?δ
      ?𝝳 ?ε ?𝝴 ?ζ ?𝝵 ?η ?𝝶 ?θ ?𝝷 ?ι ?𝝸 ?κ ?𝝹 ?λ ?𝝺 ?μ
      ?𝝻 ?ν ?𝝼 ?ξ ?𝝽 ?ο ?𝝾 ?π ?𝝿 ?ρ ?𝞀 ?ς ?𝞁 ?σ ?𝞂 ?τ
      ?𝞃 ?υ ?𝞄 ?φ ?𝞅 ?χ ?𝞆 ?ψ ?𝞇 ?ω ?𝞈 ?ϑ ?𝞋 ?ϕ ?𝞍 ?ϖ ?𝞏
      ?ϰ ?𝞌 ?ϱ ?𝞎 ?ϴ ?𝝧 ?ϵ ?𝞊 ?∇ ?𝝯)))

(defvar math-symbols-sans-serif-bold-italic-table
  #s(hash-table
     data
     (?A ?𝘼 ?B ?𝘽 ?C ?𝘾 ?D ?𝘿 ?E ?𝙀 ?F ?𝙁 ?G ?𝙂 ?H ?𝙃 ?I ?𝙄
      ?J ?𝙅 ?K ?𝙆 ?L ?𝙇 ?M ?𝙈 ?N ?𝙉 ?O ?𝙊 ?P ?𝙋 ?Q ?𝙌 ?R ?𝙍 ?S
      ?𝙎 ?T ?𝙏 ?U ?𝙐 ?V ?𝙑 ?W ?𝙒 ?X ?𝙓 ?Y ?𝙔 ?Z ?𝙕 ?a ?𝙖 ?b ?𝙗
      ?c ?𝙘 ?d ?𝙙 ?e ?𝙚 ?f ?𝙛 ?g ?𝙜 ?h ?𝙝 ?i ?𝙞 ?j ?𝙟 ?k ?𝙠 ?l
      ?𝙡 ?m ?𝙢 ?n ?𝙣 ?o ?𝙤 ?p ?𝙥 ?q ?𝙦 ?r ?𝙧 ?s ?𝙨 ?t ?𝙩 ?u ?𝙪
      ?v ?𝙫 ?w ?𝙬 ?x ?𝙭 ?y ?𝙮 ?z ?𝙯 ?Α ?𝞐 ?Β ?𝞑 ?Γ ?𝞒 ?Δ
      ?𝞓 ?Ε ?𝞔 ?Ζ ?𝞕 ?Η ?𝞖 ?Θ ?𝞗 ?Ι ?𝞘 ?Κ ?𝞙 ?Λ ?𝞚 ?Μ
      ?𝞛 ?Ν ?𝞜 ?Ξ ?𝞝 ?Ο ?𝞞 ?Π ?𝞟 ?Ρ ?𝞠 ?Σ ?𝞢 ?Τ ?𝞣 ?Υ
      ?𝞤 ?Φ ?𝞥 ?Χ ?𝞦 ?Ψ ?𝞧 ?Ω ?𝞨 ?α ?𝞪 ?β ?𝞫 ?γ ?𝞬 ?δ
      ?𝞭 ?ε ?𝞮 ?ζ ?𝞯 ?η ?𝞰 ?θ ?𝞱 ?ι ?𝞲 ?κ ?𝞳 ?λ ?𝞴 ?μ
      ?𝞵 ?ν ?𝞶 ?ξ ?𝞷 ?ο ?𝞸 ?π ?𝞹 ?ρ ?𝞺 ?ς ?𝞻 ?σ ?𝞼 ?τ
      ?𝞽 ?υ ?𝞾 ?φ ?𝞿 ?χ ?𝟀 ?ψ ?𝟁 ?ω ?𝟂 ?ϑ ?𝟅 ?ϕ ?𝟇 ?ϖ ?𝟉
      ?ϰ ?𝟆 ?ϱ ?𝟈 ?ϴ ?𝞡 ?ϵ ?𝟄 ?∇ ?𝞩)))

(defvar math-symbols-sans-serif-italic-table
  #s(hash-table 
     data
     (?A ?𝘈 ?B ?𝘉 ?C ?𝘊 ?D ?𝘋 ?E ?𝘌 ?F ?𝘍 ?G ?𝘎 ?H ?𝘏 ?I ?𝘐
      ?J ?𝘑 ?K ?𝘒 ?L ?𝘓 ?M ?𝘔 ?N ?𝘕 ?O ?𝘖 ?P ?𝘗 ?Q ?𝘘 ?R ?𝘙 ?S
      ?𝘚 ?T ?𝘛 ?U ?𝘜 ?V ?𝘝 ?W ?𝘞 ?X ?𝘟 ?Y ?𝘠 ?Z ?𝘡 ?a ?𝘢 ?b ?𝘣
      ?c ?𝘤 ?d ?𝘥 ?e ?𝘦 ?f ?𝘧 ?g ?𝘨 ?h ?𝘩 ?i ?𝘪 ?j ?𝘫 ?k ?𝘬 ?l
      ?𝘭 ?m ?𝘮 ?n ?𝘯 ?o ?𝘰 ?p ?𝘱 ?q ?𝘲 ?r ?𝘳 ?s ?𝘴 ?t ?𝘵 ?u ?𝘶
      ?v ?𝘷 ?w ?𝘸 ?x ?𝘹 ?y ?𝘺 ?z ?𝘻)))

(defvar math-symbols-script-table
  #s(hash-table
     data
     (?A ?𝒜 ?B ?ℬ ?C ?𝒞 ?D ?𝒟 ?E ?ℰ ?F ?ℱ ?G ?𝒢 ?H ?ℋ ?I ?ℐ
      ?J ?𝒥 ?K ?𝒦 ?L ?ℒ ?M ?ℳ ?N ?𝒩 ?O ?𝒪 ?P ?𝒫 ?Q ?𝒬 ?R ?ℛ ?S
      ?𝒮 ?T ?𝒯 ?U ?𝒰 ?V ?𝒱 ?W ?𝒲 ?X ?𝒳 ?Y ?𝒴 ?Z ?𝒵 ?a ?𝒶 ?b ?𝒷
      ?c ?𝒸 ?d ?𝒹 ?e ?ℯ ?f ?𝒻 ?g ?ℊ ?h ?𝒽 ?i ?𝒾 ?j ?𝒿 ?k ?𝓀 ?l
      ?ℓ ?m ?𝓂 ?n ?𝓃 ?o ?ℴ ?p ?𝓅 ?q ?𝓆 ?r ?𝓇 ?s ?𝓈 ?t ?𝓉 ?u
      ?𝓊 ?v ?𝓋 ?w ?𝓌 ?x ?𝓍 ?y ?𝓎 ?z ?𝓏)))

(defvar math-symbols-subscript-table
  #s(hash-table 
     data
     (?( ?₍ ?) ?₎ ?+ ?₊ ?0 ?₀ ?1 ?₁ ?2 ?₂ ?3 ?₃ ?4 ?₄ ?5
      ?₅ ?6 ?₆ ?7 ?₇ ?8 ?₈ ?9 ?₉ ?= ?₌ ?a ?ₐ ?e ?ₑ ?h ?ₕ ?i ?ᵢ
      ?j ?ⱼ ?k ?ₖ ?l ?ₗ ?m ?ₘ ?n ?ₙ ?o ?ₒ ?p ?ₚ ?r ?ᵣ ?s ?ₛ ?t
      ?ₜ ?u ?ᵤ ?v ?ᵥ ?x ?ₓ ?ə ?ₔ ?β ?ᵦ ?γ ?ᵧ ?ρ ?ᵨ ?φ ?ᵩ
      ?χ ?ᵪ ?− ?₋
      ;; exceptional case
      ?- ?₋)))

(defvar math-symbols-superscript-table
  #s(hash-table
     data
     (?( ?⁽ ?) ?⁾ ?+ ?⁺ ?0 ?⁰ ?1 ?¹ ?2 ?² ?3 ?³ ?4 ?⁴ ?5
      ?⁵ ?6 ?⁶ ?7 ?⁷ ?8 ?⁸ ?9 ?⁹ ?= ?⁼ ?A ?ᴬ ?B ?ᴮ ?D ?ᴰ ?E ?ᴱ
      ?G ?ᴳ ?H ?ᴴ ?I ?ᴵ ?J ?ᴶ ?K ?ᴷ ?L ?ᴸ ?M ?ᴹ ?N ?ᴺ ?O ?ᴼ ?P
      ?ᴾ ?R ?ᴿ ?T ?ᵀ ?U ?ᵁ ?V ?ⱽ ?W ?ᵂ ?a ?ᵃ ?b ?ᵇ ?c ;; ª
      ?ᶜ ?d ?ᵈ ?e ?ᵉ ?f ?ᶠ ?g ?ᵍ ?h ?ʰ ?i ?ⁱ ?j ?ʲ ?k ?ᵏ ?l ?ˡ
      ?m ?ᵐ ?n ?ⁿ ?o ?ᵒ ?p ?ᵖ ?r ?ʳ ?s ?ˢ ?t ?ᵗ ?u ?ᵘ ?v ?ᵛ ;; º
      ?w ?ʷ ?x ?ˣ ?y ?ʸ ?z ?ᶻ ?Æ ?ᴭ ?ð ?ᶞ ?Ħ ?ꟸ ?ŋ ?ᵑ ?œ
      ?ꟹ ?Ǝ ?ᴲ ?ƫ ?ᶵ ?Ȣ ?ᴽ ?ɐ ?ᵄ ?ɑ ?ᵅ ?ɒ ?ᶛ ?ɔ ?ᵓ ?ɕ ?ᶝ ?ə
      ?ᵊ ?ɛ ?ᵋ ?ɜ ?ᵌ ?ɟ ?ᶡ ?ɡ ?ᶢ ?ɣ ?ˠ ?ɥ ?ᶣ ?ɦ ?ʱ ?ɨ ?ᶤ ?ɩ
      ?ᶥ ?ɪ ?ᶦ ?ɭ ?ᶩ ?ɯ ?ᵚ ?ɰ ?ᶭ ?ɱ ?ᶬ ?ɲ ?ᶮ ?ɳ ?ᶯ ?ɴ ?ᶰ ?ɵ ?ᶱ
      ?ɸ ?ᶲ ?ɹ ?ʴ ?ɻ ?ʵ ?ʁ ?ʶ ?ʂ ?ᶳ ?ʃ ?ᶴ ?ʉ ?ᶶ ?ʊ ?ᶷ ?ʋ ?ᶹ ?ʌ
      ?ᶺ ?ʐ ?ᶼ ?ʑ ?ᶽ ?ʒ ?ᶾ ?ʕ ?ˤ ?ʝ ?ᶨ ?ʟ ?ᶫ ?β ?ᵝ ?γ ?ᵞ ?δ
      ?ᵟ ?θ ?ᶿ ?φ ?ᵠ ?χ ?ᵡ ?н ?ᵸ ?ნ ?ჼ ?ᴂ ?ᵆ ?ᴖ ?ᵔ ?ᴗ ?ᵕ
      ?ᴜ ?ᶸ ?ᴝ ?ᵙ ?ᴥ ?ᵜ ?ᵻ ?ᶧ ?ᶅ ?ᶪ ?− ?⁻ ?ⵡ ?ⵯ ?一 ?㆒ ?丁 ?㆜
      ?三 ?㆔ ?上 ?㆖ ?下 ?㆘ ?丙 ?㆛ ?中 ?㆗ ?乙 ?㆚ ?二 ?㆓
      ?人 ?㆟ ?四 ?㆕ ?地 ?㆞ ?天 ?㆝ ?甲 ?㆙ ?ꝯ ?ꝰ
      ;; exceptional case
      ?- ?⁻)))

(defvar math-symbols-variations
  '("∩︀" ; INTERSECTION with serifs
    "∪︀" ; UNION with serifs
    "≨︀" ; LESS-THAN BUT NOT EQUAL TO - with vertical stroke
    "≩︀" ; GREATER-THAN BUT NOT EQUAL TO - with vertical stroke
    "≲︀" ; LESS-THAN OR EQUIVALENT TO - following the slant of the lower leg
    "≳︀" ; GREATER-THAN OR EQUIVALENT TO - following the slant of the lower leg
    "⊊︀" ; SUBSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
    "⊋︀" ; SUPERSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
    "⊓︀" ; SQUARE CAP with serifs
    "⊔︀" ; SQUARE CUP with serifs
    "⊕︀" ; CIRCLED PLUS with white rim
    "⊗︀" ; CIRCLED TIMES with white rim
    "⊜︀" ; CIRCLED EQUALS - equal sign inside and touching the circle
    "⋚︀" ; LESS-THAN slanted EQUAL TO OR GREATER-THAN
    "⋛︀" ; GREATER-THAN slanted EQUAL TO OR LESS-THAN
    "⨼︀" ; INTERIOR PRODUCT - tall variant with narrow foot
    "⨽︀" ; RIGHTHAND INTERIOR PRODUCT - tall variant with narrow foot
    "⪝︀" ; SIMILAR OR LESS-THAN - following the slant of the upper leg - or less-than
    "⪞︀" ; SIMILAR OR GREATER-THAN - following the slant of the upper leg - or greater-than
    "⪬︀" ; SMALLER THAN OR slanted EQUAL
    "⪭︀" ; LARGER THAN OR slanted EQUAL
    "⫋︀" ; SUBSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
    "⫌︀" ; SUPERSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
    ))

(defvar math-symbols-style-names
  (mapcar 'car math-symbols-style-alist))

(defvar math-symbols-superscript-regexp
  (let ((table (eval (assoc-default "SUPERSCRIPT" math-symbols-style-alist)))
        result)
    (maphash (lambda (k _v) (push k result)) table)
    (regexp-opt (mapcar 'char-to-string result))))

(defvar math-symbols-superscript-to-regexp
  (let ((table (eval (assoc-default "SUPERSCRIPT" math-symbols-style-alist)))
        result)
    (maphash (lambda (_k v) (push v result)) table)
    (regexp-opt (mapcar 'char-to-string result))))

(defvar math-symbols-subscript-regexp
  (let ((table (eval (assoc-default "SUBSCRIPT" math-symbols-style-alist)))
        result)
    (maphash (lambda (k _v) (push k result)) table)
    (regexp-opt (mapcar 'char-to-string result))))

(defvar math-symbols-subscript-to-regexp
  (let ((table (eval (assoc-default  "SUBSCRIPT" math-symbols-style-alist)))
        result)
    (maphash (lambda (_k v) (push v result)) table)
    (regexp-opt (mapcar 'char-to-string result))))

;; functions
(defun math-symbols-style-table (style)
  (or (eval (assoc-default style math-symbols-style-alist))
      (error "Not proper style!")))

;;;###autoload
(defun math-symbols-stylize-region (from to style)
  "Mathematically Stylize REGION.  STYLE may be one of `math-symbols-style-names'"
  (interactive (list (region-beginning) (region-end) 
                     (completing-read "Style: " math-symbols-style-names)))
  (let ((table (math-symbols-style-table style)) char)
    (save-excursion
      (save-restriction
        (narrow-to-region from to)
        (goto-char (point-min))
        (while (not (eobp))
          (setq char (gethash (char-after (point)) table))
          (if (null char) (forward-char)
            (delete-char 1) (insert char)))))))

(defun math-symbols-stylize (style key)
  (interactive (list (completing-read "Style: " math-symbols-style-names)
                     (read-key "Key:" )))
  (let* ((table (math-symbols-style-table style))
         (char (gethash key table)))
    (if char (insert char) (message "Math symbol not found!"))))

;;;###autoload
(defun math-symbols-bold-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD"))

;;;###autoload
(defun math-symbols-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "ITALIC"))

;;;###autoload
(defun math-symbols-bold-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD ITALIC"))

;;;###autoload
(defun math-symbols-script-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SCRIPT"))

;;;###autoload
(defun math-symbols-bold-script-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD SCRIPT"))

;;;###autoload
(defun math-symbols-fraktur-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "FRAKTUR"))

;;;###autoload
(defun math-symbols-bold-fraktur-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD FRAKTUR"))

;;;###autoload
(defun math-symbols-double-struck-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "DOUBLE-STRUCK"))

;;;###autoload
(defun math-symbols-sans-serif-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF"))

;;;###autoload
(defun math-symbols-sans-serif-bold-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF BOLD"))

;;;###autoload
(defun math-symbols-sans-serif-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF ITALIC"))

;;;###autoload
(defun math-symbols-sans-serif-bold-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF BOLD ITALIC"))

;;;###autoload
(defun math-symbols-monospace-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "MONOSPACE"))

;;;###autoload
(defun math-symbols-superscript-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SUPERSCRIPT"))
(defun math-symbols-superscript-string (string)
  (with-temp-buffer
    (insert string)
    (math-symbols-superscript-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun math-symbols-subscript-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SUBSCRIPT"))
(defun math-symbols-subscript-string (string)
  (with-temp-buffer
    (insert string)
    (math-symbols-subscript-region (point-min) (point-max))
    (buffer-string)))

(defun math-symbols-super/subscript-from-tex-region (from to)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\^{\\(" math-symbols-superscript-regexp "+\\)}") nil t)
        (replace-match (math-symbols-superscript-string
                        (buffer-substring (match-beginning 1) (match-end 1)))))
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\^\\(" math-symbols-superscript-regexp "\\)") nil t)
        (replace-match (math-symbols-superscript-string
                        (buffer-substring (match-beginning 1) (match-end 1)))))
      (goto-char (point-min))
      (while (re-search-forward
              (concat "_{\\(" math-symbols-subscript-regexp "+\\)}") nil t)
        (replace-match (math-symbols-subscript-string
                        (buffer-substring (match-beginning 1) (match-end 1)))))
      (goto-char (point-min))
      (while (re-search-forward
              (concat "_\\(" math-symbols-subscript-regexp "\\)") nil t)
        (replace-match (math-symbols-subscript-string
                        (buffer-substring (match-beginning 1) (match-end 1)))))
      )))

(defun math-symbols-super/subscript-to-tex-region (from to)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward
              (concat math-symbols-superscript-to-regexp "+") nil t)
        (let ((length (length (match-string 0)))
              (chars (save-match-data
                       (ucs-normalize-NFKC-string 
                        (buffer-substring (match-beginning 0) (match-end 0))))))
          (replace-match
           (concat "^" (if (< 1 length) "{") chars (if (< 1 length) "}")))))
      (goto-char (point-min))
      (while (re-search-forward
              (concat math-symbols-subscript-to-regexp "+") nil t)
        (let ((length (length (match-string 0)))
              (chars (save-match-data
                       (ucs-normalize-NFKC-string 
                        (buffer-substring (match-beginning 0) (match-end 0))))))
          (replace-match
           (concat "_" (if (< 1 length) "{") chars (if (< 1 length) "}"))))))))

;;;###autoload
(defun math-symbols-insert (name)
  "Interactively input math characters from symbols."
  (interactive
   (let ((completion-ignore-case nil))
     (list (completing-read "Symbol (press tab to list): " math-symbols))))
  (when (string-match "(\\(.\\))$" name)
    (insert (match-string 1 name))))

;;;###autoload
(defun math-symbols-from-tex-region (from to)
  "Convert TeX commands in REGION to math symbols.
For example, '\Phi' will be converted to '𝛷'."
  (interactive "r*")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward (concat "\\\\" math-symbols-tex-regexp) nil t)
        (let ((tex (match-string 0)))
          (replace-match (char-to-string (gethash (substring tex 1)
                                                  math-symbols-from-tex-table))
                         t t)))
      (math-symbols-super/subscript-from-tex-region (point-min) (point-max))
      (math-symbols-italic-region (point-min) (point-max)))))

;;;###autoload
(defun math-symbols-to-tex-region (from to)
  "Convert math symbols to TeX command in REGION.
For example, `𝒫' will be converted to `mathcal{P}'."
  (interactive "r*")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((char (char-after (point)))
               (tex (gethash char math-symbols-tex-table)))
          (if (null tex) (forward-char)
            (delete-char 1) (insert "\\" tex))))
      (math-symbols-super/subscript-to-tex-region (point-min) (point-max))
      (ucs-normalize-NFKC-region (point-min) (point-max)))))

(provide 'math-symbols)

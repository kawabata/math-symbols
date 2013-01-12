;;; math-symbols.el --- math symbol input and conversion tool

;; Filename: math-symbols.el
;; Description: Math symbol input and TeX conversion tool.
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2013-01-11
;; Version: 0.2
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
;;   "Fractur" → "𝔉𝔯𝔞𝔠𝔱𝔲𝔯" (M-x math-fraktur-region)
;;   "black" → "𝒷ℓ𝒶𝒸𝓀" (M-x math-script-region)
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

;;; Code:

(require 'cl)

;; data generated from `unimathsymbols.txt'
(defvar math-symbols-tex-table
  #s(hash-table test eql data
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
     ?𝜗 "vartheta" ?𝜘 "varkappa" ?𝜙 "phi"
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
    (maphash (lambda (k v) (add-to-list 'syms v)) math-symbols-tex-table)
    (regexp-opt syms)))

(defvar math-symbols-from-tex-table
  (let ((table (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (puthash v k table)) math-symbols-tex-table)
    table))

(defvar math-symbols
  (let (syms)
    (maphash (lambda (k v) 
               (unless (string-match "{" v)
                 (add-to-list 'syms (format "%s (%c)" v k))))
             math-symbols-tex-table) syms))

(defvar math-symbols-style-names
  '("BOLD"
    "ITALIC"
    "BOLD ITALIC"
    "SCRIPT"
    "BOLD SCRIPT"
    "FRAKTUR"
    "BOLD FRAKTUR"
    "DOUBLE-STRUCK"
    "SANS-SERIF"
    "SANS-SERIF BOLD"
    "SANS-SERIF ITALIC"
    "SANS-SERIF BOLD ITALIC"
    "MONOSPACE"
    "BLACK-LETTER"
    "SUPERSCRIPT"
    "SUBSCRIPT"))

(defvar math-symbols-style-parenthesis
  '(("DOUBLE-STRUCK" . #s(hash-table data (?[ ?⟦ ?] ?⟧ ?< ?⟪ ?> ?⟫ ?{ ?⟬ ?} ?⟭)))
    ("SANS-SERIF" . #s(hash-table data (?<  ?⟨ ?> ?⟩ ?( ?⟮ ?) ?⟯))))
  "Default math parenthesis for each style.")

(defvar math-symbolize-table (make-hash-table :test 'equal))

;; functions

(defun math-symbols-style-table (style)
  (when (not (member style math-symbols-style-names))
    (error "Not proper style!"))
  (let ((table (gethash style math-symbolize-table)))
    (when (null table)
      (setq table
            (cond ((equal style "SUPERSCRIPT") 
                   (math-symbols-style-table-by-decomposition 'super))
                  ((equal style "SUBSCRIPT") 
                   (math-symbols-style-table-by-decomposition 'sub))
                  (t
                   (math-symbols-style-table-by-name style))))
      (puthash style table math-symbolize-table))
    table))

(defun math-symbols-style-table-by-decomposition (symbol)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item (ucs-names))
      (let ((decomp (get-char-code-property (cdr item) 'decomposition)))
        (if (equal (car decomp) symbol) (puthash (cadr decomp) (cdr item) table))))
    table))

(defun math-symbols-style-table-by-name (style)
  (let ((table (or (cdr (assoc style math-symbols-style-parenthesis))
                  (make-hash-table :test 'equal))))
    (dolist (item (remove-if-not 
                   `(lambda (x) (string-match 
                                 ,(concat
                                   "^\\(MATHEMATICAL \\)?" style 
                                   " \\(CAPITAL\\|SMALL\\|DIGIT\\|NABLA\\|"
                                   "PARTIAL DIFFRENTIAL\\|"
                                   "\\(EPSILON\\|THETA\\|KAPPA\\|PHI\\|RHO\\|PI\\) SYMBOL"
                                   "\\)") (car x)))
                   (ucs-names)))
      (puthash (cadr (get-char-code-property (cdr item) 'decomposition))
               (cdr item) table))
    table))

;;;###autoload
(defun math-symbols-stylize-region (from to style)
  "Mathematically Stylize REGION.  STYLE may be one of `math-symbols-style-names'"
  (interactive (list (region-beginning) (region-end) 
                     (completing-read "Style: " math-symbols-style-names)))
  (let ((table (math-symbols-style-table style)))
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
(defun math-bold-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD"))

;;;###autoload
(defun math-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "ITALIC"))

;;;###autoload
(defun math-bold-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD ITALIC"))

;;;###autoload
(defun math-script-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SCRIPT"))

;;;###autoload
(defun math-bold-script-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD SCRIPT"))

;;;###autoload
(defun math-fraktur-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "FRAKTUR"))

;;;###autoload
(defun math-bold-fraktur-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BOLD FRAKTUR"))

;;;###autoload
(defun math-double-struck-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "DOUBLE-STRUCK"))

;;;###autoload
(defun math-sans-serif-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF"))

;;;###autoload
(defun math-sans-serif-bold-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF BOLD"))

;;;###autoload
(defun math-sans-serif-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF ITALIC"))

;;;###autoload
(defun math-sans-serif-bold-italic-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SANS-SERIF BOLD ITALIC"))

;;;###autoload
(defun math-monospace-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "MONOSPACE"))

;;;###autoload
(defun math-black-letter-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "BLACK-LETTER"))

;;;###autoload
(defun math-superscript-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SUPERSCRIPT"))

;;;###autoload
(defun math-subscript-region (from to)
  (interactive "r*")
  (math-symbols-stylize-region from to "SUBSCRIPT"))

;;;###autoload
(defun math-insert (name)
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
                         t t))))))

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
            (delete-char 1) (insert "\\" tex)))))))

(provide 'math-symbols)

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
     ?𝜗 "vartheta" ?ϰ "varkappa" ?𝜙 "phi" ;;  ?𝜘
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
    "FRAKTUR" ;;     "BLACK-LETTER"
    "BOLD FRAKTUR"
    "DOUBLE-STRUCK"
    "DOUBLE-STRUCK ITALIC"
    "SANS-SERIF"
    "SANS-SERIF BOLD"
    "SANS-SERIF ITALIC"
    "SANS-SERIF BOLD ITALIC"
    "MONOSPACE"
    "SUPERSCRIPT"
    "SUBSCRIPT"))

(defvar math-symbolize-table
  #s(hash-table
     test equal data
     ("BOLD" 
      #s(hash-table 
         data
         (?9 ?𝟗 ?8 ?𝟖 ?7 ?𝟕 ?6 ?𝟔 ?5 ?𝟓 ?4 ?𝟒 ?3 ?𝟑 ?2 ?𝟐 ?1 ?𝟏
         ?0 ?𝟎 ?ϝ ?𝟋 ?Ϝ ?𝟊 ?ϖ ?𝛡 ?ϱ ?𝛠 ?ϕ ?𝛟 ?ϰ ?𝛞 ?ϑ ?𝛝 ?ϵ ?𝛜 ?ω
         ?𝛚 ?ψ ?𝛙 ?χ ?𝛘 ?φ ?𝛗 ?υ ?𝛖 ?τ ?𝛕 ?σ ?𝛔 ?ς ?𝛓 ?ρ
         ?𝛒 ?π ?𝛑 ?ο ?𝛐 ?ξ ?𝛏 ?ν ?𝛎 ?μ ?𝛍 ?λ ?𝛌 ?κ ?𝛋 ?ι
         ?𝛊 ?θ ?𝛉 ?η ?𝛈 ?ζ ?𝛇 ?ε ?𝛆 ?δ ?𝛅 ?γ ?𝛄 ?β ?𝛃 ?α
         ?𝛂 ?∇ ?𝛁 ?Ω ?𝛀 ?Ψ ?𝚿 ?Χ ?𝚾 ?Φ ?𝚽 ?Υ ?𝚼 ?Τ ?𝚻 ?Σ
         ?𝚺 ?ϴ ?𝚹 ?Ρ ?𝚸 ?Π ?𝚷 ?Ο ?𝚶 ?Ξ ?𝚵 ?Ν ?𝚴 ?Μ ?𝚳 ?Λ
         ?𝚲 ?Κ ?𝚱 ?Ι ?𝚰 ?Θ ?𝚯 ?Η ?𝚮 ?Ζ ?𝚭 ?Ε ?𝚬 ?Δ ?𝚫 ?Γ
         ?𝚪 ?Β ?𝚩 ?Α ?𝚨 ?z ?𝐳 ?y ?𝐲 ?x ?𝐱 ?w ?𝐰 ?v ?𝐯 ?u ?𝐮 ?t
         ?𝐭 ?s ?𝐬 ?r ?𝐫 ?q ?𝐪 ?p ?𝐩 ?o ?𝐨 ?n ?𝐧 ?m ?𝐦 ?l ?𝐥 ?k ?𝐤
         ?j ?𝐣 ?i ?𝐢 ?h ?𝐡 ?g ?𝐠 ?f ?𝐟 ?e ?𝐞 ?d ?𝐝 ?c ?𝐜 ?b ?𝐛 ?a
         ?𝐚 ?Z ?𝐙 ?Y ?𝐘 ?X ?𝐗 ?W ?𝐖 ?V ?𝐕 ?U ?𝐔 ?T ?𝐓 ?S ?𝐒 ?R ?𝐑
         ?Q ?𝐐 ?P ?𝐏 ?O ?𝐎 ?N ?𝐍 ?M ?𝐌 ?L ?𝐋 ?K ?𝐊 ?J ?𝐉 ?I ?𝐈 ?H
         ?𝐇 ?G ?𝐆 ?F ?𝐅 ?E ?𝐄 ?D ?𝐃 ?C ?𝐂 ?B ?𝐁 ?A ?𝐀))
      "BOLD FRAKTUR"
      #s(hash-table 
         data
         (?z ?𝖟 ?y ?𝖞 ?x ?𝖝 ?w ?𝖜 ?v ?𝖛 ?u ?𝖚 ?t ?𝖙 ?s ?𝖘 ?r ?𝖗
          ?q ?𝖖 ?p ?𝖕 ?o ?𝖔 ?n ?𝖓 ?m ?𝖒 ?l ?𝖑 ?k ?𝖐 ?j ?𝖏 ?i ?𝖎
          ?h ?𝖍 ?g ?𝖌 ?f ?𝖋 ?e ?𝖊 ?d ?𝖉 ?c ?𝖈 ?b ?𝖇 ?a ?𝖆 ?Z ?𝖅
          ?Y ?𝖄 ?X ?𝖃 ?W ?𝖂 ?V ?𝖁 ?U ?𝖀 ?T ?𝕿 ?S ?𝕾 ?R ?𝕽 ?Q ?𝕼
          ?P ?𝕻 ?O ?𝕺 ?N ?𝕹 ?M ?𝕸 ?L ?𝕷 ?K ?𝕶 ?J ?𝕵 ?I ?𝕴 ?H ?𝕳
          ?G ?𝕲 ?F ?𝕱 ?E ?𝕰 ?D ?𝕯 ?C ?𝕮 ?B ?𝕭 ?A ?𝕬))
      "BOLD ITALIC" 
      #s(hash-table
         data
         (?ϖ ?𝝕 ?ϱ ?𝝔 ?ϕ ?𝝓 ?ϰ ?𝝒 ?ϑ ?𝝑 ?ϵ ?𝝐 ?ω ?𝝎 ?ψ ?𝝍 ?χ
         ?𝝌 ?φ ?𝝋 ?υ ?𝝊 ?τ ?𝝉 ?σ ?𝝈 ?ς ?𝝇 ?ρ ?𝝆 ?π ?𝝅 ?ο
         ?𝝄 ?ξ ?𝝃 ?ν ?𝝂 ?μ ?𝝁 ?λ ?𝝀 ?κ ?𝜿 ?ι ?𝜾 ?θ ?𝜽 ?η
         ?𝜼 ?ζ ?𝜻 ?ε ?𝜺 ?δ ?𝜹 ?γ ?𝜸 ?β ?𝜷 ?α ?𝜶 ?∇ ?𝜵 ?Ω
         ?𝜴 ?Ψ ?𝜳 ?Χ ?𝜲 ?Φ ?𝜱 ?Υ ?𝜰 ?Τ ?𝜯 ?Σ ?𝜮 ?ϴ ?𝜭 ?Ρ
         ?𝜬 ?Π ?𝜫 ?Ο ?𝜪 ?Ξ ?𝜩 ?Ν ?𝜨 ?Μ ?𝜧 ?Λ ?𝜦 ?Κ ?𝜥 ?Ι
         ?𝜤 ?Θ ?𝜣 ?Η ?𝜢 ?Ζ ?𝜡 ?Ε ?𝜠 ?Δ ?𝜟 ?Γ ?𝜞 ?Β ?𝜝 ?Α
         ?𝜜 ?z ?𝒛 ?y ?𝒚 ?x ?𝒙 ?w ?𝒘 ?v ?𝒗 ?u ?𝒖 ?t ?𝒕 ?s ?𝒔 ?r ?𝒓
         ?q ?𝒒 ?p ?𝒑 ?o ?𝒐 ?n ?𝒏 ?m ?𝒎 ?l ?𝒍 ?k ?𝒌 ?j ?𝒋 ?i ?𝒊 ?h
         ?𝒉 ?g ?𝒈 ?f ?𝒇 ?e ?𝒆 ?d ?𝒅 ?c ?𝒄 ?b ?𝒃 ?a ?𝒂 ?Z ?𝒁 ?Y ?𝒀
         ?X ?𝑿 ?W ?𝑾 ?V ?𝑽 ?U ?𝑼 ?T ?𝑻 ?S ?𝑺 ?R ?𝑹 ?Q ?𝑸 ?P ?𝑷 ?O
         ?𝑶 ?N ?𝑵 ?M ?𝑴 ?L ?𝑳 ?K ?𝑲 ?J ?𝑱 ?I ?𝑰 ?H ?𝑯 ?G ?𝑮 ?F ?𝑭
         ?E ?𝑬 ?D ?𝑫 ?C ?𝑪 ?B ?𝑩 ?A ?𝑨))
      "BOLD SCRIPT" 
      #s(hash-table
         data
         (?z ?𝔃 ?y ?𝔂 ?x ?𝔁 ?w ?𝔀 ?v ?𝓿 ?u ?𝓾 ?t ?𝓽 ?s ?𝓼 ?r ?𝓻
         ?q ?𝓺 ?p ?𝓹 ?o ?𝓸 ?n ?𝓷 ?m ?𝓶 ?l ?𝓵 ?k ?𝓴 ?j ?𝓳 ?i ?𝓲 ?h
         ?𝓱 ?g ?𝓰 ?f ?𝓯 ?e ?𝓮 ?d ?𝓭 ?c ?𝓬 ?b ?𝓫 ?a ?𝓪 ?Z ?𝓩 ?Y ?𝓨
         ?X ?𝓧 ?W ?𝓦 ?V ?𝓥 ?U ?𝓤 ?T ?𝓣 ?S ?𝓢 ?R ?𝓡 ?Q ?𝓠 ?P ?𝓟 ?O
         ?𝓞 ?N ?𝓝 ?M ?𝓜 ?L ?𝓛 ?K ?𝓚 ?J ?𝓙 ?I ?𝓘 ?H ?𝓗 ?G ?𝓖 ?F ?𝓕
         ?E ?𝓔 ?D ?𝓓 ?C ?𝓒 ?B ?𝓑 ?A ?𝓐))
      "DOUBLE-STRUCK" 
      #s(hash-table
         data
         (?[ ?⟦ ?] ?⟧ ?< ?⟪ ?> ?⟫ ?{ ?⟬ ?} ?⟭ ?9 ?𝟡 ?8 ?𝟠 ?7 ?𝟟
         ?6 ?𝟞 ?5 ?𝟝 ?4 ?𝟜 ?3 ?𝟛 ?2 ?𝟚 ?1 ?𝟙 ?0 ?𝟘 ?z ?𝕫 ?y ?𝕪 ?x
         ?𝕩 ?w ?𝕨 ?v ?𝕧 ?u ?𝕦 ?t ?𝕥 ?s ?𝕤 ?r ?𝕣 ?q ?𝕢 ?p ?𝕡 ?o ?𝕠
         ?n ?𝕟 ?m ?𝕞 ?l ?𝕝 ?k ?𝕜 ?j ?𝕛 ?i ?𝕚 ?h ?𝕙 ?g ?𝕘 ?f ?𝕗 ?e
         ?𝕖 ?d ?𝕕 ?c ?𝕔 ?b ?𝕓 ?a ?𝕒 ?Y ?𝕐 ?X ?𝕏 ?W ?𝕎 ?V ?𝕍 ?U ?𝕌
         ?T ?𝕋 ?S ?𝕊 ?O ?𝕆 ?M ?𝕄 ?L ?𝕃 ?K ?𝕂 ?J ?𝕁 ?I ?𝕀 ?G ?𝔾 ?F
         ?𝔽 ?E ?𝔼 ?D ?𝔻 ?B ?𝔹 ?A ?𝔸 ?Π ?ℿ ?Γ ?ℾ ?γ ?ℽ ?π ?ℼ
         ?Z ?ℤ ?R ?ℝ ?Q ?ℚ ?P ?ℙ ?N ?ℕ ?H ?ℍ ?C ?ℂ))
      "FRAKTUR" ;; "BLACK-LETTER" 
      #s(hash-table 
         data
         (?z ?𝔷 ?y ?𝔶 ?x ?𝔵 ?w ?𝔴 ?v ?𝔳 ?u ?𝔲 ?t ?𝔱 ?s ?𝔰 ?r ?𝔯
         ?q ?𝔮 ?p ?𝔭 ?o ?𝔬 ?n ?𝔫 ?m ?𝔪 ?l ?𝔩 ?k ?𝔨 ?j ?𝔧 ?i ?𝔦 ?h
         ?𝔥 ?g ?𝔤 ?f ?𝔣 ?e ?𝔢 ?d ?𝔡 ?c ?𝔠 ?b ?𝔟 ?a ?𝔞 ?Z ?ℨ ?Y ?𝔜
         ?X ?𝔛 ?W ?𝔚 ?V ?𝔙 ?U ?𝔘 ?T ?𝔗 ?S ?𝔖 ?R ?ℜ ?Q ?𝔔 ?P ?𝔓 ?O
         ?𝔒 ?N ?𝔑 ?M ?𝔐 ?L ?𝔏 ?K ?𝔎 ?J ?𝔍 ?I ?ℑ ?H ?ℌ ?G ?𝔊 ?F ?𝔉
         ?E ?𝔈 ?D ?𝔇 ?C ?ℭ ?B ?𝔅 ?A ?𝔄))
      "ITALIC"
      #s(hash-table
         data
         (?ϖ ?𝜛 ?ϱ ?𝜚 ?ϕ ?𝜙 ?ϰ ?𝜘 ?ϑ ?𝜗 ?ϵ ?𝜖 ?ω ?𝜔 ?ψ ?𝜓 ?χ
         ?𝜒 ?φ ?𝜑 ?υ ?𝜐 ?τ ?𝜏 ?σ ?𝜎 ?ς ?𝜍 ?ρ ?𝜌 ?π ?𝜋 ?ο
         ?𝜊 ?ξ ?𝜉 ?ν ?𝜈 ?μ ?𝜇 ?λ ?𝜆 ?κ ?𝜅 ?ι ?𝜄 ?θ ?𝜃 ?η
         ?𝜂 ?ζ ?𝜁 ?ε ?𝜀 ?δ ?𝛿 ?γ ?𝛾 ?β ?𝛽 ?α ?𝛼 ?∇ ?𝛻 ?Ω
         ?𝛺 ?Ψ ?𝛹 ?Χ ?𝛸 ?Φ ?𝛷 ?Υ ?𝛶 ?Τ ?𝛵 ?Σ ?𝛴 ?ϴ ?𝛳 ?Ρ
         ?𝛲 ?Π ?𝛱 ?Ο ?𝛰 ?Ξ ?𝛯 ?Ν ?𝛮 ?Μ ?𝛭 ?Λ ?𝛬 ?Κ ?𝛫 ?Ι
         ?𝛪 ?Θ ?𝛩 ?Η ?𝛨 ?Ζ ?𝛧 ?Ε ?𝛦 ?Δ ?𝛥 ?Γ ?𝛤 ?Β ?𝛣 ?Α
         ?𝛢 ?ȷ ?𝚥 ?ı ?𝚤 ?z ?𝑧 ?y ?𝑦 ?x ?𝑥 ?w ?𝑤 ?v ?𝑣 ?u ?𝑢 ?t
         ?𝑡 ?s ?𝑠 ?r ?𝑟 ?q ?𝑞 ?p ?𝑝 ?o ?𝑜 ?n ?𝑛 ?m ?𝑚 ?l ?𝑙 ?k ?𝑘
         ?j ?𝑗 ?i ?𝑖 ?g ?𝑔 ?f ?𝑓 ?e ?𝑒 ?d ?𝑑 ?c ?𝑐 ?b ?𝑏 ?a ?𝑎 ?Z
         ?𝑍 ?Y ?𝑌 ?X ?𝑋 ?W ?𝑊 ?V ?𝑉 ?U ?𝑈 ?T ?𝑇 ?S ?𝑆 ?R ?𝑅 ?Q ?𝑄
         ?P ?𝑃 ?O ?𝑂 ?N ?𝑁 ?M ?𝑀 ?L ?𝐿 ?K ?𝐾 ?J ?𝐽 ?I ?𝐼 ?H ?𝐻 ?G
         ?𝐺 ?F ?𝐹 ?E ?𝐸 ?D ?𝐷 ?C ?𝐶 ?B ?𝐵 ?A ?𝐴))
      "MONOSPACE" 
      #s(hash-table
         data
         (?9 ?𝟿 ?8 ?𝟾 ?7 ?𝟽 ?6 ?𝟼 ?5 ?𝟻 ?4 ?𝟺 ?3 ?𝟹 ?2 ?𝟸 ?1 ?𝟷
         ?0 ?𝟶 ?z ?𝚣 ?y ?𝚢 ?x ?𝚡 ?w ?𝚠 ?v ?𝚟 ?u ?𝚞 ?t ?𝚝 ?s ?𝚜 ?r
         ?𝚛 ?q ?𝚚 ?p ?𝚙 ?o ?𝚘 ?n ?𝚗 ?m ?𝚖 ?l ?𝚕 ?k ?𝚔 ?j ?𝚓 ?i ?𝚒
         ?h ?𝚑 ?g ?𝚐 ?f ?𝚏 ?e ?𝚎 ?d ?𝚍 ?c ?𝚌 ?b ?𝚋 ?a ?𝚊 ?Z ?𝚉 ?Y
         ?𝚈 ?X ?𝚇 ?W ?𝚆 ?V ?𝚅 ?U ?𝚄 ?T ?𝚃 ?S ?𝚂 ?R ?𝚁 ?Q ?𝚀 ?P ?𝙿
         ?O ?𝙾 ?N ?𝙽 ?M ?𝙼 ?L ?𝙻 ?K ?𝙺 ?J ?𝙹 ?I ?𝙸 ?H ?𝙷 ?G ?𝙶 ?F
         ?𝙵 ?E ?𝙴 ?D ?𝙳 ?C ?𝙲 ?B ?𝙱 ?A ?𝙰))
      "SANS-SERIF" 
      #s(hash-table 
         data
         (?< ?⟨ ?> ?⟩ ?( ?⟮ ?) ?⟯ ?9 ?𝟫 ?8 ?𝟪 ?7 ?𝟩 ?6 ?𝟨 ?5 ?𝟧
         ?4 ?𝟦 ?3 ?𝟥 ?2 ?𝟤 ?1 ?𝟣 ?0 ?𝟢 ?z ?𝗓 ?y ?𝗒 ?x ?𝗑 ?w ?𝗐 ?v
         ?𝗏 ?u ?𝗎 ?t ?𝗍 ?s ?𝗌 ?r ?𝗋 ?q ?𝗊 ?p ?𝗉 ?o ?𝗈 ?n ?𝗇 ?m ?𝗆
         ?l ?𝗅 ?k ?𝗄 ?j ?𝗃 ?i ?𝗂 ?h ?𝗁 ?g ?𝗀 ?f ?𝖿 ?e ?𝖾 ?d ?𝖽 ?c
         ?𝖼 ?b ?𝖻 ?a ?𝖺 ?Z ?𝖹 ?Y ?𝖸 ?X ?𝖷 ?W ?𝖶 ?V ?𝖵 ?U ?𝖴 ?T ?𝖳
         ?S ?𝖲 ?R ?𝖱 ?Q ?𝖰 ?P ?𝖯 ?O ?𝖮 ?N ?𝖭 ?M ?𝖬 ?L ?𝖫 ?K ?𝖪 ?J
         ?𝖩 ?I ?𝖨 ?H ?𝖧 ?G ?𝖦 ?F ?𝖥 ?E ?𝖤 ?D ?𝖣 ?C ?𝖢 ?B ?𝖡 ?A
         ?𝖠))
      "SANS-SERIF BOLD"
      #s(hash-table 
         data
         (?9 ?𝟵 ?8 ?𝟴 ?7 ?𝟳 ?6 ?𝟲 ?5 ?𝟱 ?4 ?𝟰 ?3 ?𝟯 ?2 ?𝟮 ?1 ?𝟭
         ?0 ?𝟬 ?ϖ ?𝞏 ?ϱ ?𝞎 ?ϕ ?𝞍 ?ϰ ?𝞌 ?ϑ ?𝞋 ?ϵ ?𝞊 ?ω ?𝞈 ?ψ ?𝞇
         ?χ ?𝞆 ?φ ?𝞅 ?υ ?𝞄 ?τ ?𝞃 ?σ ?𝞂 ?ς ?𝞁 ?ρ ?𝞀 ?π ?𝝿
         ?ο ?𝝾 ?ξ ?𝝽 ?ν ?𝝼 ?μ ?𝝻 ?λ ?𝝺 ?κ ?𝝹 ?ι ?𝝸 ?θ ?𝝷
         ?η ?𝝶 ?ζ ?𝝵 ?ε ?𝝴 ?δ ?𝝳 ?γ ?𝝲 ?β ?𝝱 ?α ?𝝰 ?∇ ?𝝯
         ?Ω ?𝝮 ?Ψ ?𝝭 ?Χ ?𝝬 ?Φ ?𝝫 ?Υ ?𝝪 ?Τ ?𝝩 ?Σ ?𝝨 ?ϴ ?𝝧
         ?Ρ ?𝝦 ?Π ?𝝥 ?Ο ?𝝤 ?Ξ ?𝝣 ?Ν ?𝝢 ?Μ ?𝝡 ?Λ ?𝝠 ?Κ ?𝝟
         ?Ι ?𝝞 ?Θ ?𝝝 ?Η ?𝝜 ?Ζ ?𝝛 ?Ε ?𝝚 ?Δ ?𝝙 ?Γ ?𝝘 ?Β ?𝝗
         ?Α ?𝝖 ?z ?𝘇 ?y ?𝘆 ?x ?𝘅 ?w ?𝘄 ?v ?𝘃 ?u ?𝘂 ?t ?𝘁 ?s ?𝘀
         ?r ?𝗿 ?q ?𝗾 ?p ?𝗽 ?o ?𝗼 ?n ?𝗻 ?m ?𝗺 ?l ?𝗹 ?k ?𝗸 ?j ?𝗷 ?i
         ?𝗶 ?h ?𝗵 ?g ?𝗴 ?f ?𝗳 ?e ?𝗲 ?d ?𝗱 ?c ?𝗰 ?b ?𝗯 ?a ?𝗮 ?Z ?𝗭
         ?Y ?𝗬 ?X ?𝗫 ?W ?𝗪 ?V ?𝗩 ?U ?𝗨 ?T ?𝗧 ?S ?𝗦 ?R ?𝗥 ?Q ?𝗤 ?P
         ?𝗣 ?O ?𝗢 ?N ?𝗡 ?M ?𝗠 ?L ?𝗟 ?K ?𝗞 ?J ?𝗝 ?I ?𝗜 ?H ?𝗛 ?G ?𝗚
         ?F ?𝗙 ?E ?𝗘 ?D ?𝗗 ?C ?𝗖 ?B ?𝗕 ?A ?𝗔))
      "SANS-SERIF BOLD ITALIC"
      #s(hash-table
         data
         (?ϖ ?𝟉 ?ϱ ?𝟈 ?ϕ ?𝟇 ?ϰ ?𝟆 ?ϑ ?𝟅 ?ϵ ?𝟄 ?ω ?𝟂 ?ψ ?𝟁 ?χ
         ?𝟀 ?φ ?𝞿 ?υ ?𝞾 ?τ ?𝞽 ?σ ?𝞼 ?ς ?𝞻 ?ρ ?𝞺 ?π ?𝞹 ?ο
         ?𝞸 ?ξ ?𝞷 ?ν ?𝞶 ?μ ?𝞵 ?λ ?𝞴 ?κ ?𝞳 ?ι ?𝞲 ?θ ?𝞱 ?η
         ?𝞰 ?ζ ?𝞯 ?ε ?𝞮 ?δ ?𝞭 ?γ ?𝞬 ?β ?𝞫 ?α ?𝞪 ?∇ ?𝞩 ?Ω
         ?𝞨 ?Ψ ?𝞧 ?Χ ?𝞦 ?Φ ?𝞥 ?Υ ?𝞤 ?Τ ?𝞣 ?Σ ?𝞢 ?ϴ ?𝞡 ?Ρ
         ?𝞠 ?Π ?𝞟 ?Ο ?𝞞 ?Ξ ?𝞝 ?Ν ?𝞜 ?Μ ?𝞛 ?Λ ?𝞚 ?Κ ?𝞙 ?Ι
         ?𝞘 ?Θ ?𝞗 ?Η ?𝞖 ?Ζ ?𝞕 ?Ε ?𝞔 ?Δ ?𝞓 ?Γ ?𝞒 ?Β ?𝞑 ?Α
         ?𝞐 ?z ?𝙯 ?y ?𝙮 ?x ?𝙭 ?w ?𝙬 ?v ?𝙫 ?u ?𝙪 ?t ?𝙩 ?s ?𝙨 ?r ?𝙧
         ?q ?𝙦 ?p ?𝙥 ?o ?𝙤 ?n ?𝙣 ?m ?𝙢 ?l ?𝙡 ?k ?𝙠 ?j ?𝙟 ?i ?𝙞 ?h
         ?𝙝 ?g ?𝙜 ?f ?𝙛 ?e ?𝙚 ?d ?𝙙 ?c ?𝙘 ?b ?𝙗 ?a ?𝙖 ?Z ?𝙕 ?Y ?𝙔
         ?X ?𝙓 ?W ?𝙒 ?V ?𝙑 ?U ?𝙐 ?T ?𝙏 ?S ?𝙎 ?R ?𝙍 ?Q ?𝙌 ?P ?𝙋 ?O
         ?𝙊 ?N ?𝙉 ?M ?𝙈 ?L ?𝙇 ?K ?𝙆 ?J ?𝙅 ?I ?𝙄 ?H ?𝙃 ?G ?𝙂 ?F ?𝙁
         ?E ?𝙀 ?D ?𝘿 ?C ?𝘾 ?B ?𝘽 ?A ?𝘼))
      "SANS-SERIF ITALIC" 
      #s(hash-table 
         data
         (?z ?𝘻 ?y ?𝘺 ?x ?𝘹 ?w ?𝘸 ?v ?𝘷 ?u ?𝘶 ?t ?𝘵 ?s ?𝘴 ?r ?𝘳
         ?q ?𝘲 ?p ?𝘱 ?o ?𝘰 ?n ?𝘯 ?m ?𝘮 ?l ?𝘭 ?k ?𝘬 ?j ?𝘫 ?i ?𝘪 ?h
         ?𝘩 ?g ?𝘨 ?f ?𝘧 ?e ?𝘦 ?d ?𝘥 ?c ?𝘤 ?b ?𝘣 ?a ?𝘢 ?Z ?𝘡 ?Y ?𝘠
         ?X ?𝘟 ?W ?𝘞 ?V ?𝘝 ?U ?𝘜 ?T ?𝘛 ?S ?𝘚 ?R ?𝘙 ?Q ?𝘘 ?P ?𝘗 ?O
         ?𝘖 ?N ?𝘕 ?M ?𝘔 ?L ?𝘓 ?K ?𝘒 ?J ?𝘑 ?I ?𝘐 ?H ?𝘏 ?G ?𝘎 ?F ?𝘍
         ?E ?𝘌 ?D ?𝘋 ?C ?𝘊 ?B ?𝘉 ?A ?𝘈))
      "SCRIPT" 
      #s(hash-table
         data
         (?z ?𝓏 ?y ?𝓎 ?x ?𝓍 ?w ?𝓌 ?v ?𝓋 ?u ?𝓊 ?t ?𝓉 ?s ?𝓈 ?r ?𝓇
         ?q ?𝓆 ?p ?𝓅 ?n ?𝓃 ?m ?𝓂 ?l ?ℓ ?k ?𝓀 ?j ?𝒿 ?i ?𝒾 ?h ?𝒽
         ?f ?𝒻 ?d ?𝒹 ?c ?𝒸 ?b ?𝒷 ?a ?𝒶 ?Z ?𝒵 ?Y ?𝒴 ?X ?𝒳 ?W ?𝒲 ?V
         ?𝒱 ?U ?𝒰 ?T ?𝒯 ?S ?𝒮 ?Q ?𝒬 ?P ?𝒫 ?O ?𝒪 ?N ?𝒩 ?K ?𝒦 ?J ?𝒥
         ?G ?𝒢 ?D ?𝒟 ?C ?𝒞 ?A ?𝒜 ?o ?ℴ ?M ?ℳ ?F ?ℱ ?E ?ℰ ?e ?ℯ ?B
         ?ℬ ?R ?ℛ nil ?℘ ?L ?ℒ ?I ?ℐ ?H ?ℋ ?g ?ℊ))
      "SUBSCRIPT"
      #s(hash-table 
         data
         (?j ?ⱼ ?t ?ₜ ?s ?ₛ ?p ?ₚ ?n ?ₙ ?m ?ₘ ?l ?ₗ ?k ?ₖ ?h ?ₕ
         ?ə ?ₔ ?x ?ₓ ?o ?ₒ ?e ?ₑ ?a ?ₐ ?) ?₎ ?( ?₍ ?= ?₌ ?− ?₋ ?+
         ?₊ ?9 ?₉ ?8 ?₈ ?7 ?₇ ?6 ?₆ ?5 ?₅ ?4 ?₄ ?3 ?₃ ?2 ?₂ ?1
         ?₁ ?0 ?₀ ?χ ?ᵪ ?φ ?ᵩ ?ρ ?ᵨ ?γ ?ᵧ ?β ?ᵦ ?v ?ᵥ ?u ?ᵤ
         ?r ?ᵣ ?i ?ᵢ))
      "SUPERSCRIPT"
      #s(hash-table
         data
         (?M ?ᴹ ?œ ?ꟹ ?Ħ ?ꟸ ?ꝯ ?ꝰ ?人 ?㆟ ?地 ?㆞ ?天 ?㆝ ?丁 
         ?㆜ ?丙 ?㆛ ?乙 ?㆚ ?甲 ?㆙ ?下 ?㆘ ?中 ?㆗ ?上 ?㆖ ?四 
         ?㆕ ?三 ?㆔ ?二 ?㆓ ?一 ?㆒ ?ⵡ ?ⵯ ?V ?ⱽ ?T ?ᵀ ?S ?℠ ?n ?ⁿ
         ?) ?⁾ ?( ?⁽ ?= ?⁼ ?− ?⁻ ?+ ?⁺ ?9 ?⁹ ?8 ?⁸ ?7 ?⁷ ?6 ?⁶ ?5
         ?⁵ ?4 ?⁴ ?i ?ⁱ ?0 ?⁰ ?θ ?ᶿ ?ʒ ?ᶾ ?ʑ ?ᶽ ?ʐ ?ᶼ ?z ?ᶻ ?ʌ
         ?ᶺ ?ʋ ?ᶹ ?ᴜ ?ᶸ ?ʊ ?ᶷ ?ʉ ?ᶶ ?ƫ ?ᶵ ?ʃ ?ᶴ ?ʂ ?ᶳ ?ɸ ?ᶲ ?ɵ ?ᶱ
         ?ɴ ?ᶰ ?ɳ ?ᶯ ?ɲ ?ᶮ ?ɰ ?ᶭ ?ɱ ?ᶬ ?ʟ ?ᶫ ?ᶅ ?ᶪ ?ɭ ?ᶩ ?ʝ ?ᶨ ?ᵻ
         ?ᶧ ?ɪ ?ᶦ ?ɩ ?ᶥ ?ɨ ?ᶤ ?ɥ ?ᶣ ?ɡ ?ᶢ ?ɟ ?ᶡ ?f ?ᶠ ?ɜ ?ᵌ ?ð
         ?ᶞ ?ɕ ?ᶝ ?c ?ᶜ ?ɒ ?ᶛ ?н ?ᵸ ?χ ?ᵡ ?φ ?ᵠ ?δ ?ᵟ ?γ ?ᵞ
         ?β ?ᵝ ?ᴥ ?ᵜ ?v ?ᵛ ?ɯ ?ᵚ ?ᴝ ?ᵙ ?u ?ᵘ ?t ?ᵗ ?p ?ᵖ ?ᴗ ?ᵕ
         ?ᴖ ?ᵔ ?ɔ ?ᵓ ?o ?º ?ŋ ?ᵑ ?m ?ᵐ ?k ?ᵏ ?g ?ᵍ ?ɛ ?ᵋ ?ə ?ᵊ
         ?e ?ᵉ ?d ?ᵈ ?b ?ᵇ ?ᴂ ?ᵆ ?ɑ ?ᵅ ?ɐ ?ᵄ ?a ?ª ?W ?ᵂ ?U ?ᵁ
         ?R ?ᴿ ?P ?ᴾ ?Ȣ ?ᴽ ?O ?ᴼ ?N ?ᴺ ?L ?ᴸ ?K ?ᴷ ?J ?ᴶ ?I ?ᴵ ?H
         ?ᴴ ?G ?ᴳ ?Ǝ ?ᴲ ?E ?ᴱ ?D ?ᴰ ?B ?ᴮ ?Æ ?ᴭ ?A ?ᴬ ?ნ ?ჼ ?ʕ
         ?ˤ ?x ?ˣ ?s ?ˢ ?l ?ˡ ?ɣ ?ˠ ?y ?ʸ ?w ?ʷ ?ʁ ?ʶ ?ɻ ?ʵ ?ɹ ?ʴ
         ?r ?ʳ ?j ?ʲ ?ɦ ?ʱ ?h ?ʰ ?1 ?¹ ?3 ?³ ?2 ?²)))))

;; functions
(defun math-symbols-style-table (style)
  (or (gethash style math-symbolize-table)
      (error "Not proper style!")))

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

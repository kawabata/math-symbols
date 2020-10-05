;;; math-symbols.el --- Math Symbol Input methods and conversion tools -*- lexical-binding: t -*-

;; Filename: math-symbols.el
;; Description: Math Symbol Input methods and conversion tools
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2013-03-25
;; Version: 2.201006
;; Keywords: i18n languages tex
;; Human-Keywords: math symbols
;; URL: https://github.com/kawabata/math-symbols

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; -*- mode: org -*-
;; * Description
;;
;; This utility lets you input math symbols by TeX names
;; with the following commands.
;;
;; - M-x toggle-input-method + math-symbols-tex
;; - M-x math-symbols-from-tex-region
;; - M-x math-symbols-insert
;; - M-x math-symbols-ivy
;;
;; Also, you can convert character to TeX names by the following command.
;; - M-x math-symbols-to-tex-region
;; - M-x math-symbols-to-tex-unicode-region
;;
;; You can also input various styled mathematical symbols by input
;; methods, or by specifying region or string.
;;
;; - M-x toggle-input-method + math-symbols-italic (or script, etc.)
;; - M-x math-symbols-italic-region
;; - (math-symbols-italic-string "target string")
;;
;;
;; * Examples:
;;
;; ** TeX to Char Conversion
;; : "\int g(x^2)dx = \pi e^(ix)" → "∫ 𝑔(𝑥²)𝑑𝑥 = 𝜋 𝑒^(𝑖𝑥)"
;; : (M-x math-symbols-from-tex-region)
;; : "f(x+y)" → "𝑓(𝑥+𝑦)" (M-x math-symbols-italic-region)
;;
;; ** Character to Math-Character Conversion
;; : "Fraktur" → "𝔉𝔯𝔞𝔨𝔱𝔲𝔯" (M-x math-symbols-fraktur-region)
;; : "black" → "𝒷ℓ𝒶𝒸𝓀" (M-x math-symbols-script-region)
;; : "Quo Vadis" → "ℚ𝕦𝕠 𝕍𝕒𝕕𝕚𝕤" (M-x math-symbols-double-struck-region)
;; : "3+(2-1)=4" → "³⁺⁽²-¹⁾⁼⁴" (M-x math-symbols-superscript-region)
;;
;; * Required Font
;;
;; You should install Math fonts such as "STIX" to your system, and
;; then add it to your fontset to fully utilize this tool.  Recent
;; MacOS includes this font by default.  You can freely download them
;; from [[STIX website][http://www.stixfonts.org]].
;;
;; * Licenses
;;
;; This program incorporates `unimathsymbols.txt' data file which is
;; licensed under "LaTeX Project Public License".  This program is
;; GPL.
;;
;; * Math Symbols Support Table
;;
;; | styles / scripts         | alphabets | greeks※ | numerals |
;; |--------------------------+-----------+----------+----------|
;; | bold                     | yes       | yes      | yes      |
;; | (bold) italic            | yes       | yes      | yes      |
;; | (bold) fraktur           | yes       | no       | no       |
;; | (bold) script            | yes       | no       | no       |
;; | double-struck            | yes       | partial  | yes      |
;; | monospace                | yes       | no       | yes      |
;; | sans-serif (italic)      | yes       | no       | yes      |
;; | sans-serif bold (italic) | yes       | yes      | yes      |
;; | subscript                | partial   | no       | yes      |
;; | superscript              | partial   | no       | yes      |
;;
;;  ※ `greeks' include greek symbols and nabla (ϵ, ϑ, ϰ, ϕ, ϱ, ϖ, ∇).
;;
;; * References
;;
;; - UTR#25 UNICODE SUPPORT FOR MATHEMATICS
;;   (http://www.unicode.org/reports/tr25/tr25-6.html)

;;; Code:

(eval-when-compile (require 'cl))
(require 'robin)

;;;; TeX Data
;; generate table from from `unimathsymbols.txt'
(eval-and-compile
(defvar math-symbols-tex-table
  (let* ((directory (file-name-directory (or byte-compile-current-file
                                             load-file-name
                                             buffer-file-name)))
         (unimath-file (expand-file-name "unimathsymbols.txt" directory))
         (table (make-hash-table :test 'equal)))
    (unless (file-exists-p unimath-file)
      (error "Data file not found!"))
    (with-temp-buffer
      (insert-file-contents unimath-file)
      (while (re-search-forward
              "^[0-9A-F]+^\\(.\\)^\\([^^]+\\)?^\\([^^]+\\)?^" nil t)
        (let* ((char (string-to-char (match-string 1)))
               (tex (match-string 2))
               (tex-unicode (match-string 3))
               )
          (when (or tex tex-unicode)
            (puthash char (cons tex tex-unicode) table)))))
    table)
  "UCS to TeX commands table.  Taken from 'unimathsymbols.txt'."))

(defvar math-symbols-tex-regexp
  (eval-when-compile
    (let (syms)
      (maphash (lambda (_k v)
                 (when (car v) (push (car v) syms))) math-symbols-tex-table)
      (regexp-opt syms)))
  "Regexp to match TeX mathematical notation.")

(defvar math-symbols-tex-unicode-regexp
  (eval-when-compile
    (let (syms)
      (maphash (lambda (_k v)
                 (when (cdr v) (push (cdr v) syms))) math-symbols-tex-table)
      (regexp-opt syms)))
  "Regexp to match TeX Unicode-math package notation.")


(defvar math-symbols-from-tex-table
  (eval-when-compile
    (let ((table (make-hash-table :test 'equal)))
      (maphash (lambda (k v)
                 (when (car v) (puthash (car v) k table))
                 (when (cdr v) (puthash (cdr v) k table)))
               math-symbols-tex-table)
      table))
  "Table from TeX (including Unicode-math Package) notation to Char.")

;;;; Style Data

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

(eval-and-compile
(defvar math-symbols-subscript-table
  #s(hash-table
     data
     (?( ?₍ ?) ?₎ ?+ ?₊ ?0 ?₀ ?1 ?₁ ?2 ?₂ ?3 ?₃ ?4 ?₄ ?5
      ?₅ ?6 ?₆ ?7 ?₇ ?8 ?₈ ?9 ?₉ ?= ?₌ ?a ?ₐ ?e ?ₑ ?h ?ₕ ?i ?ᵢ
      ?j ?ⱼ ?k ?ₖ ?l ?ₗ ?m ?ₘ ?n ?ₙ ?o ?ₒ ?p ?ₚ ?r ?ᵣ ?s ?ₛ ?t
      ?ₜ ?u ?ᵤ ?v ?ᵥ ?x ?ₓ ?ə ?ₔ ?β ?ᵦ ?γ ?ᵧ ?ρ ?ᵨ ?φ ?ᵩ
      ?χ ?ᵪ ?－ ?₋
      ;; exceptional case
      ?- ?₋))))

(eval-and-compile
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
      ?ᴜ ?ᶸ ?ᴝ ?ᵙ ?ᴥ ?ᵜ ?ᵻ ?ᶧ ?ᶅ ?ᶪ ?－ ?⁻ ?ⵡ ?ⵯ ?一 ?㆒ ?丁 ?㆜
      ?三 ?㆔ ?上 ?㆖ ?下 ?㆘ ?丙 ?㆛ ?中 ?㆗ ?乙 ?㆚ ?二 ?㆓
      ?人 ?㆟ ?四 ?㆕ ?地 ?㆞ ?天 ?㆝ ?甲 ?㆙ ?ꝯ ?ꝰ
      ;; exceptional case
      ?- ?⁻))))

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

(defvar math-symbols-subscript-regexp
  (eval-when-compile
    (regexp-opt
     (loop for key being the hash-keys of math-symbols-superscript-table
           collect (char-to-string key)))))

(defvar math-symbols-subscript-to-regexp
  (eval-when-compile
    (regexp-opt
     (loop for key being the hash-values of math-symbols-superscript-table
           collect (char-to-string key)))))

(defvar math-symbols-superscript-regexp
  (eval-when-compile
    (regexp-opt
     (loop for key being the hash-keys of math-symbols-subscript-table
           collect (char-to-string key)))))

(defvar math-symbols-superscript-to-regexp
  (eval-when-compile
    (regexp-opt
     (loop for key being the hash-values of math-symbols-subscript-table
           collect (char-to-string key)))))

;;;; Style Setup

(dolist (s '(bold italic bold-italic script bold-script fraktur bold-fraktur
              double-struck sans-serif sans-serif-bold sans-serif-italic
              sans-serif-bold-italic monospace superscript subscript))
  (eval
   `(robin-define-package
     ,(format "math-symbols-%s" s)
     ,(format "Input method for math symbols %s style." s)
     ,@(cl-loop for k being the hash-keys of
                (symbol-value
                 (intern (format "math-symbols-%s-table" s)))
                using (hash-values v)
                collect (list (char-to-string k) v)))))

(defun math-symbols-string (style string)
  (with-temp-buffer
    (insert string)
    (robin-convert-region (point-min) (point-max) style)
    (buffer-string)))

;;;###autoload
(defun math-symbols-input-activate (name)
  "Activating Math Input method."
  (robin-use-package name))

;;;;; code generator
;;
;;(dolist (s '(bold italic bold-italic script bold-script fraktur bold-fraktur
;;              double-struck sans-serif sans-serif-bold sans-serif-italic
;;              sans-serif-bold-italic monospace superscript subscript))
;;  (let ((title (mapconcat (lambda (p) (capitalize (substring p 0 2)))
;;                          (split-string (format "%s" s) "-") "")))
;;    (insert "\n;;;###autoload\n")
;;    (insert (format "(register-input-method \"math-symbols-%s\" \"math\"\n" s))
;;    (insert (format "                       'math-symbols-input-activate \"m%s\")\n" title))
;;    (insert ";;;###autoload\n")
;;    (insert (format "(defun math-symbols-%s-region (from to)\n" s))
;;    (insert (format "  \"Convert REGION to %s style.\"\n" s))
;;    (insert (format "  (interactive \"r*P\") (robin-convert-region from to \"math-symbols-%s\"))\n" s))
;;    (insert ";;;###autoload\n")
;;    (insert (format "(defun math-symbols-%s-string (string)\n" s))
;;    (insert (format "  \"Convert STRING to %s style.\"\n" s))
;;    (insert (format "  (math-symbols-string \"math-symbols-%s\" string))\n" s))))

;;;;; generated code

;;;###autoload
(register-input-method "math-symbols-bold" "math"
                       'math-symbols-input-activate "mBo")
;;;###autoload
(defun math-symbols-bold-region (from to)
  "Convert REGION to bold style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-bold"))
;;;###autoload
(defun math-symbols-bold-string (string)
  "Convert STRING to bold style."
  (math-symbols-string "math-symbols-bold" string))

;;;###autoload
(register-input-method "math-symbols-italic" "math"
                       'math-symbols-input-activate "mIt")
;;;###autoload
(defun math-symbols-italic-region (from to)
  "Convert REGION to italic style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-italic"))
;;;###autoload
(defun math-symbols-italic-string (string)
  "Convert STRING to italic style."
  (math-symbols-string "math-symbols-italic" string))

;;;###autoload
(register-input-method "math-symbols-bold-italic" "math"
                       'math-symbols-input-activate "mBoIt")
;;;###autoload
(defun math-symbols-bold-italic-region (from to)
  "Convert REGION to bold-italic style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-bold-italic"))
;;;###autoload
(defun math-symbols-bold-italic-string (string)
  "Convert STRING to bold-italic style."
  (math-symbols-string "math-symbols-bold-italic" string))

;;;###autoload
(register-input-method "math-symbols-script" "math"
                       'math-symbols-input-activate "mSc")
;;;###autoload
(defun math-symbols-script-region (from to)
  "Convert REGION to script style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-script"))
;;;###autoload
(defun math-symbols-script-string (string)
  "Convert STRING to script style."
  (math-symbols-string "math-symbols-script" string))

;;;###autoload
(register-input-method "math-symbols-bold-script" "math"
                       'math-symbols-input-activate "mBoSc")
;;;###autoload
(defun math-symbols-bold-script-region (from to)
  "Convert REGION to bold-script style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-bold-script"))
;;;###autoload
(defun math-symbols-bold-script-string (string)
  "Convert STRING to bold-script style."
  (math-symbols-string "math-symbols-bold-script" string))

;;;###autoload
(register-input-method "math-symbols-fraktur" "math"
                       'math-symbols-input-activate "mFr")
;;;###autoload
(defun math-symbols-fraktur-region (from to)
  "Convert REGION to fraktur style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-fraktur"))
;;;###autoload
(defun math-symbols-fraktur-string (string)
  "Convert STRING to fraktur style."
  (math-symbols-string "math-symbols-fraktur" string))

;;;###autoload
(register-input-method "math-symbols-bold-fraktur" "math"
                       'math-symbols-input-activate "mBoFr")
;;;###autoload
(defun math-symbols-bold-fraktur-region (from to)
  "Convert REGION to bold-fraktur style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-bold-fraktur"))
;;;###autoload
(defun math-symbols-bold-fraktur-string (string)
  "Convert STRING to bold-fraktur style."
  (math-symbols-string "math-symbols-bold-fraktur" string))

;;;###autoload
(register-input-method "math-symbols-double-struck" "math"
                       'math-symbols-input-activate "mDoSt")
;;;###autoload
(defun math-symbols-double-struck-region (from to)
  "Convert REGION to double-struck style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-double-struck"))
;;;###autoload
(defun math-symbols-double-struck-string (string)
  "Convert STRING to double-struck style."
  (math-symbols-string "math-symbols-double-struck" string))

;;;###autoload
(register-input-method "math-symbols-sans-serif" "math"
                       'math-symbols-input-activate "mSaSe")
;;;###autoload
(defun math-symbols-sans-serif-region (from to)
  "Convert REGION to sans-serif style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-sans-serif"))
;;;###autoload
(defun math-symbols-sans-serif-string (string)
  "Convert STRING to sans-serif style."
  (math-symbols-string "math-symbols-sans-serif" string))

;;;###autoload
(register-input-method "math-symbols-sans-serif-bold" "math"
                       'math-symbols-input-activate "mSaSeBo")
;;;###autoload
(defun math-symbols-sans-serif-bold-region (from to)
  "Convert REGION to sans-serif-bold style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-sans-serif-bold"))
;;;###autoload
(defun math-symbols-sans-serif-bold-string (string)
  "Convert STRING to sans-serif-bold style."
  (math-symbols-string "math-symbols-sans-serif-bold" string))

;;;###autoload
(register-input-method "math-symbols-sans-serif-italic" "math"
                       'math-symbols-input-activate "mSaSeIt")
;;;###autoload
(defun math-symbols-sans-serif-italic-region (from to)
  "Convert REGION to sans-serif-italic style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-sans-serif-italic"))
;;;###autoload
(defun math-symbols-sans-serif-italic-string (string)
  "Convert STRING to sans-serif-italic style."
  (math-symbols-string "math-symbols-sans-serif-italic" string))

;;;###autoload
(register-input-method "math-symbols-sans-serif-bold-italic" "math"
                       'math-symbols-input-activate "mSaSeBoIt")
;;;###autoload
(defun math-symbols-sans-serif-bold-italic-region (from to)
  "Convert REGION to sans-serif-bold-italic style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-sans-serif-bold-italic"))
;;;###autoload
(defun math-symbols-sans-serif-bold-italic-string (string)
  "Convert STRING to sans-serif-bold-italic style."
  (math-symbols-string "math-symbols-sans-serif-bold-italic" string))

;;;###autoload
(register-input-method "math-symbols-monospace" "math"
                       'math-symbols-input-activate "mMo")
;;;###autoload
(defun math-symbols-monospace-region (from to)
  "Convert REGION to monospace style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-monospace"))
;;;###autoload
(defun math-symbols-monospace-string (string)
  "Convert STRING to monospace style."
  (math-symbols-string "math-symbols-monospace" string))

;;;###autoload
(register-input-method "math-symbols-superscript" "math"
                       'math-symbols-input-activate "mSu")
;;;###autoload
(defun math-symbols-superscript-region (from to)
  "Convert REGION to superscript style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-superscript"))
;;;###autoload
(defun math-symbols-superscript-string (string)
  "Convert STRING to superscript style."
  (math-symbols-string "math-symbols-superscript" string))

;;;###autoload
(register-input-method "math-symbols-subscript" "math"
                       'math-symbols-input-activate "mSu")
;;;###autoload
(defun math-symbols-subscript-region (from to)
  "Convert REGION to subscript style."
  (interactive "r*P") (robin-convert-region from to "math-symbols-subscript"))
;;;###autoload
(defun math-symbols-subscript-string (string)
  "Convert STRING to subscript style."
  (math-symbols-string "math-symbols-subscript" string))

;; generated codes ends here.


;;;; TeX to Unicode Input Methods

(eval
 `(robin-define-package
   "math-symbols-tex"
   "Robin Package for Math Symbols Tex Unicode style."
   ,@(cl-loop for k being the hash-keys of math-symbols-from-tex-table
              using (hash-values v)
              collect (list k (char-to-string v)))))
;;;###autoload
(register-input-method "math-symbols-tex" "math"
                       'math-symbols-input-activate "mt")

;;;; Unicode to/from TeX Commands

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
(defun math-symbols-to-tex-region (from to &optional unicode)
  "Convert math symbols to TeX command in REGION.
For example, `𝒫' will be converted to `mathcal{P}'.
Optional argument UNICODE specifies to use unicode-math package."
  (interactive "r*")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((char (char-after (point)))
               (tex (gethash char math-symbols-tex-table))
               (tex (if unicode (cdr tex) (car tex))))
          (if (null tex) (forward-char)
            (delete-char 1) (insert "\\" tex))))
      (math-symbols-super/subscript-to-tex-region (point-min) (point-max))
      (ucs-normalize-NFKC-region (point-min) (point-max)))))

;;;###autoload
(defun math-symbols-to-tex-unicode-region (from to)
  (interactive "r*")
  (math-symbols-to-tex-region from to t))

;;;; simple interface
(defvar math-symbols-name-char-list
  (let (result)
    (maphash (lambda (k v)
               (push (format "%s (%c)" k v) result))
             math-symbols-from-tex-table)
    result))

;;;###autoload
(defun math-symbols-insert (name)
  "Interactively input math characters from symbols."
  (interactive
   (let ((completion-ignore-case nil))
     (list (completing-read "Symbol (press tab to list): "
                            math-symbols-name-char-list))))
  (when (string-match "(\\(.\\))$" name)
    (insert (match-string 1 name))))

;;;###autoload
(defun math-symbols-ivy ()
  "Ivy interface for math-symbols.
Use double key to match backslash or space."
  (interactive)
  (ivy-read "math-symbols: " math-symbols-name-char-list
            :action (lambda (f) (string-match "(\\(.\\))$" f) (insert (match-string 1 f)))
            :require-match t))

;;;;;; helm interface
;;(defvar math-symbols-len
;;  (loop for key being the hash-keys of math-symbols-from-tex-table
;;        maximize (length key)))
;;
;;(defvar math-symbols-helm-source
;;  '((name . "Math Symbols")
;;    (init . math-symbols-helm-init)
;;    (candidate-number-limit . 9999)
;;    (candidates-in-buffer)
;;    (mode-line . helm-mode-line-string)
;;    (action . (("Insert" . math-symbols-helm-insert-char))))
;;  "Source for collecting math symbols.")
;;
;;(defun math-symbols-helm-init ()
;;  "Initialize an helm buffer with math symbols."
;;  (with-current-buffer (helm-candidate-buffer
;;                        (get-buffer-create "*math-symbols helm*"))
;;    (loop for key being the hash-keys of math-symbols-from-tex-table
;;          for val = (gethash key math-symbols-from-tex-table)
;;          for len = (length key)
;;          for diff = (+ (- math-symbols-len len) 2)
;;          unless (string= "" key)
;;          do (insert (concat key ":" (make-string diff ? ))
;;                     val "\n"))))
;;
;;(defun math-symbols-helm-insert-char (candidate)
;;  (with-helm-current-buffer
;;    (insert
;;     (replace-regexp-in-string
;;      " " ""
;;      (cadr (split-string candidate ":"))))))
;;
;;;;;###autoload
;;(defun math-symbols-helm ()
;;  (interactive)
;;  (helm :sources 'math-symbols-helm-source
;;        :keymap helm-map))

(provide 'math-symbols)

;;; math-symbols.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+2.%02y%02m%02d\\\\?\n"
;; End:

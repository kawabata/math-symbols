VERSION=`sed -n "s/;; Version: //p" math-symbols.el`
rm *.tar
echo '(define-package "math-symbols" "'$VERSION'" "math symbol input and conversion tool" (quote ((helm "1.0"))))' > math-symbols-pkg.el
mkdir math-symbols-$VERSION
cp math-symbols.el math-symbols-pkg.el unimathsymbols.txt README math-symbols-$VERSION
tar cvf math-symbols-$VERSION.tar math-symbols-$VERSION

set -e
trap "exit" INT
cabal build --ghc-options="-rtsopts"
for nlines in 100000
do
    echo "#lines: $nlines"
    f=hyphenated-h${nlines}.txt
    head -n $nlines < hyphenated.txt > $f
    for nj in `seq 1 12`
    do
        /usr/bin/env time -f %e ./runinparallel.sh $nj $f > /dev/null
        rm ${f}.*
    done
    rm $f
done

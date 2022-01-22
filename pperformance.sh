set -e
trap "exit" INT
cabal build --ghc-options="-threaded -rtsopts"
for nlines in 10000 100000
do
    echo "#lines: $nlines"
    head -n $nlines < hyphenated.txt > hyphenated-h${nlines}.txt

    for nj in `seq 1 12`
    do
        /usr/bin/env time -f %e \
            cabal run --ghc-options="-threaded -rtsopts" palantype-ops \
                -- makeSteno --file-input hyphenated-h${nlines}.txt \
                   +RTS -N$nj -A2M \
            > /dev/null
    done

    rm hyphenated-h${nlines}.txt
done

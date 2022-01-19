set -e
trap "exit" INT
cabal build
for nlines in 10000 20000 50000 100000
do
    echo "#lines: $nlines"
    f=hyphenated-h${nlines}.txt
    head -n $nlines < hyphenated.txt > $f
    for nj in `seq 1 12`
    do
        /usr/bin/env time -f %e ./runinparallel.sh $nj $f > /dev/null
        rm ${f}.*
    done

done

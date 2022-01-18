set -e
trap "exit" INT
cabal build
#for nlines in 10000
for nlines in 10000 20000 50000 100000
do
    echo "#lines: $nlines"
    head -n $nlines < hyphenated.txt > hyphenated-h${nlines}.txt

    for nj in `seq 1 8`
    do
        #njd=$nj/2
        #ngc=$((njd>0 ? $njd : 1))
        #echo "#jobs: $nj, qn $ngc"
        /usr/bin/env time -f %e cabal run palantype-ops -- stenoDict --file-input hyphenated-h${nlines}.txt +RTS -N$nj -A64M -qn1 > /dev/null
    done

done

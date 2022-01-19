#!/usr/bin/env bash
set -e
trap "exit" INT
if [ "$#" -ne 2 ]
then
    echo "Usage: $0 nj hyphenated.txt"
    exit 1
fi
cabal build
[ -f makeSteno-noparse.txt ] && rm makeSteno-noparse.txt
nj=$1
file=$2
fsplit=${file}.
split --number=l/$nj $file $fsplit
for f in ${fsplit}*
do
    [ -f ${f}.json ] && rm ${f}.json
    cabal run palantype-ops -- makeSteno --file-input $f --file-output-json ${f}.json & > /dev/null
done
wait
echo -n "lines in file makeSteno-noparse.txt: "
wc -l < makeSteno-noparse.txt

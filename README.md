# palantype-tools

Create a steno dictionary for palantype style steno systems,
i.e. a steno system that works well with any keyboard,
as long as it supports n-key roll-over (cf. https://en.wikipedia.org/wiki/Rollover_(keyboard)).

## Workflow summary

Get inital hyphenation information.
By default, read "entries.txt" and write "hyphenated-prepared.txt":

    $ palantype-ops prepare

Use files with verified hyphenation information to read "german.utf8.dic",
hyphenate each word and write the output to "hyphenated.txt":

    $ palantype-ops hyphenate -h hyphenated-prepared.txt -h hyphenated-checked-DE.txt

Sort the file "hyphenated.txt" by word frequency, such that the most
frequent word is on top. This is required for the next step.
In case of conflicts, more frequent words are expected to come first.

    $ palantype-ops sort -i hyphenated.txt

Make the steno expressions, can be run concurrently. By default, read
"hyphenated.txt" and build the steno dict.
This process requires about 6 GB of RAM.
With 6 jobs on an AMD Ryzen 5 3600, 6 cores/12 threads, it takes 18 minutes.
By default, write "palantype-DE.json", "palantype-DE-min.json",
and "palantype-DE-doc.json":

    $ palantype-ops makeSteno +RTS -N
    $ cabal run --ghc-options="-threaded" palantype-ops -- makeSteno +RTS -N

Sorting relevant files based on word frequency information:

    $ palantype-ops sort -i makeSteno-collisions.txt -i makeSteno-lostwords.txt -i makeSteno-duplicates.txt

## Full Workflow

### 1. Research the language

Languages differ in important details.
`Palantype DE`, for the German language, is built around the following assumptions:

  1. Ortographic syllables are the basis for a steno chord.
     I.e. they are not too small which would result in inefficient steno,
     nor are they too big, which would result in impossible chords
     that need to be split up.
  2. A syllable follows the pattern of cc-vv-cc,
     where cc represents zero or more consonants and vv represents one or more vowels.
  3. Ortography, rather than pronounciation is the basis for the steno code.
     I am unsure to whether or not that is true for english steno.
     Working with ortography is necessary condition for the creation of a
     steno dictionary by some algorithm (which doesn't have a concept of pronounciation).
  4. English and french foreign words are rare enough within German to be dealt with as exceptions.
     This allows to base the steno of foreign words on pronounciation.
     The alternative would imply additional rules for each foreign language.

The frequency of so called phonems is important.
Even though Palantype DE is built based on ortography,
decisions need to be made on how to map all existing letters of some language
on the 30 (or 32) steno keys.

Some choices are obvious and follow the nature of the language,
e.g. the German "sch" is treated as a single steno key 'ʃ'.
Other choices are entirely pragmatic, e.g. the suffixes "-en" and "-s" have their
own steno keys based on their high frequency.

The steno home row is filled with keys that are used with high frequency.
Those are those keys that are involved in phonems that have high frequency.
Otherwise chords get unnecessary difficult.

### 2. Create a preliminary steno keyboard layout

For German, there are only 12 keys available to implement all consonants that
appear in the onset of a syllable.
*Any* consonant can appear in the onset of a syllable, thus 12 keys implement
21 consonants, not counting special consonants (like the aforementioned sch/ʃ).
The reason is, there are four fingers on the left hand and each finger
has home, top, and bottom row.
The two thumbs are used to implement all possible vowel combinations.
The four fingers of the right hand implement the consonants, again,
for the coda of the syllable.

The keyboard layout is encoded like this:

https://github.com/rubenmoor/my-palantype/blob/main/src/Palantype/DE/Keys.hs

### 3. Define primitives

Any letter that possibly exists in some word needs to appear in the
`primitives dictionary` at least once.

Cf. https://github.com/rubenmoor/palantype-tools/blob/main/primitives.json5

Here is specified how, for Palantype DE, "d" is implemented via the steno key
"D" and "t" is implemented by either the code "+D" or "D".

The greediness number is documented within the file.
Greediness levels allow to create various steno codes for one and the same word.
The user only has to memorize one of them for efficient typing.
By allowing several levels of greediness, the algorithm creates alternatives
that are sometimes simply convenient and offer more options,
but they are also essential to resolve collisions.

A collision happens when two different words of natural language result
in identical steno. Note that two different syllables may result in identical
steno all the time without problem. Plover will always fit steno chords
together to reach the longest dictionary entry.
However, words are required to have a unique representation as steno chords.
These tools automatically detect and resolve collisions.
A contested steno code is awarded to the most common word, in general.


### 4. Organize a word list

Get one or more files that contain all words of the language.
I found an open source list of words for German here:

https://sourceforge.net/projects/germandict/

A list of 2 Mio. words that proved invaluable.
Also the UNI Leipzig provided me with a list of 5 Mio. words, with frequency information.

The format of the file should be simply one word per line, no comments.

Optionally, if you have frequency information, you can sort your word list and get started
with only the most frequent words.
This way you can produce quick results on a limited set of words.
You can then check the output and iterate quickly (e.g. change the steno layout)
before you move on to the complete set of all words.

### 5. Hyphenate

You can feed a list of words into `palantype-ops` to produce hyphenation information.

    $> palantype-ops hyphenate --help
    $> palantype-ops hyphenate --file-input words.txt

In my experience, the algorithmic hyphenation isn't 100% accurate.
Checking and correctiong thousands of words for correct hyphenation is tiring, but for the most frequent words
it's probably worthwhile.
For more than 10'000 thousand words, a complete revision doesn't seem sensible.
Rather, be prepared to encounter weird or wrong steno once in a while.
When the cause of the weird steno is bad hyphenation, you can add the correct hyphenation
manually to a file, e.g. `hyphenated-checked.txt`. You can add this file to
the aforementioned command to make sure, your corrections take precedence
over the algorithmic hyphenation:

    $> palantype-ops hyphenate -h hyphenated-checked --file-input words.txt

### 6. Sort the word list

In order to put words that have a high natural frequency
on top, you can sort any file. You need the frequency information, however.
The order of the words also affects the collision resolution, i.e.
ambiguous steno code. Usually, there are various ways to type a word and
collisions can be resolved. The higher up the word in the list,
the more efficient steno it will get in case of a collision.

    $> palantype-ops sort --help
    $> palantype-ops sort --file-frequency-data freq.txt

### 7. Build the language-specific dictionary

You can take the first 100'000 words from your sorted file and build a
dictionary like this:

    $> head -n 1000000 < hyphenated-sorted.txt > hyphenated-top100k.txt
    $> palantype-ops stenoDict -i hyphenated-top100k.txt


> In order to make use of parallel computing, use
>
>     $> palantype-ops stenoDict -i hyphenated-top100k.txt +RTS -N
>
> where the -N option will run a number of processes equal to the number of
> cores on your system. Explicitly specify the desired number of jobs by
> putting a number there, e.g. -N4.

Check the output for potential problems.

- `buildDict-noparse.txt` contains words that could not be translated in to
  steno chords, typically because primitives are missing.
- `buildDict-collisions.txt` contains words that resulted in steno that
  is already in use for other words.

Keep building small dictionary on a limited set of words and iteratively improve

    1. the list of steno keys
    2. the steno key layout
    3. the primitives
    4. the exceptions

Once you reach some 100'000+ words in the steno dictionary, think about publishing
your system as plover plug-in.

> Note that you can sort the dictionary, too. Just use the .json-file as input for the
> sort command above. Sorting can be useful, e.g. for humans who read the file, but is never necessary.

### 7. Build language-independent dictionary

WIP

At any time, you can build the dictionary of commands that are independent of language like this

    $> TODO

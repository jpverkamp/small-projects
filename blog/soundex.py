#!/usr/bin/env python3

import collections
import re
import sys

def soundex(word):
    '''
    Convert a word to its corresponding soundex code.

    1) Save the first letter, drop any later H or W
    2) Replace consontants with digits (as in letter_to_soundex)
    3) Collapse duplicate adjacent numbers
    4) Remove vowels except first letter
    5) If the first symbol is a digit, replace with the stored first letter
    6) Fix to 4 characters long, pad right with zeros
    '''

    soundex_to_letters = {
        1: 'BFPV',
        2: 'CGJKQSXZ',
        3: 'DT',
        4: 'L',
        5: 'MN',
        6: 'R',
    }

    letter_to_soundex = {
        letter: soundex
        for soundex, letters in soundex_to_letters.items()
        for letter in letters
    }

    def numeric(letter):
        if letter in letter_to_soundex:
            return str(letter_to_soundex[letter])
        else:
            return letter

    word = word.upper()
    first_letter = word[0]

    word = first_letter + re.sub(r'[HW]', '', word)
    word = re.sub(r'.', lambda m : numeric(m.group(0)), word)
    word = re.sub(r'(.)\1*', r'\1', word)
    word = first_letter + re.sub(r'[AEIOUY]', '', word[1:])
    word = word[:4] + ('0' * max(0, 4 - len(word)))

    return word

def soundex_groups(words):
    '''Return groups of words by soundex codes.'''

    soundex_groups = collections.defaultdict(set)

    for word in words:
        code = soundex(word)
        soundex_groups[code].add(word)

    for code in list(soundex_groups):
        if len(soundex_groups[code]) == 1:
            del soundex_groups[code]

    return dict(soundex_groups)

if __name__ == '__main__':
    for word in sys.argv[1:]:
        print(word, soundex(word))

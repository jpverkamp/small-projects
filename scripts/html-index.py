#!/usr/bin/env python

import os, sys

display_all = False
short_mode = False
force_replace = False

def usage():
    print '''
Usage: html-index [short|long|force|all]
  force - overwrite old index.htm without asking
  help - display this message and exit
  long (default) - links <li><a ...>...</a>
  short - links use [<a ...>...</a>
  all - display dot files
'''.strip()
    sys.exit(0)

for arg in sys.argv[1:]:
    arg = arg.lower()
    if arg == 'short':
        short_mode = True
    elif arg == 'long':
        short_mode = False
    elif arg == 'force':
        force_replace = True
    elif arg == 'help':
        usage()
    elif arg == 'all':
        display_all = True
    else:
        print 'Unknown option %s\n' % arg
        usage()

if not force_replace and os.path.exists('index.htm'):
    while True:
        print 'index.htm already exists, overwrite (yes or no)?',
        yn = raw_input()
        if yn == 'yes':
            break
        elif yn == 'no':
            sys.exit(0)
        else:
            continue

with open('index.htm', 'w') as f:
    if not short_mode: f.write('<ul>\n')
    for s in sorted(os.listdir('.')):
        if not display_all and s and s[0] == '.':
            continue

        if s != 'index.htm':
            if short_mode:
                f.write('[<a href="%s">%s</a>]\n' % (s, s.replace('.htm', '')))
            else:
                f.write('<li><a href="%s">%s</a></li>\n' % (s, s.replace('.htm', '')))
    if not short_mode: f.write('</ul>\n')

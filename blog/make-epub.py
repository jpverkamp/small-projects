# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#     (1) Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
# 
#     (2) Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
# 
#     (3)The name of the author may not be used to
#     endorse or promote products derived from this software without
#     specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

#!/usr/bin/env python

from __future__ import print_function

import os
import sys
import codecs

from glob import glob
from tempfile import mkdtemp as tempdir
from markdown2 import markdown
from epub import EpubBook

def main():
   # Get the source directory
   if len(sys.argv) == 2:
      path = sys.argv[1]
   else:
      path = '.'

   path = os.path.abspath(path)
   target = os.path.basename(path) + '.epub'
   temp = tempdir()
   title = os.path.basename(path)
   
   print('Source: %s' % path)
   print('Target: %s' % target)
   print('  Temp: %s' % temp)
   print()

   # Start an epub
   book = EpubBook()
   book.setTitle(title)
   book.addCreator('JP Verkamp')
   book.addTitlePage()
   book.addTocPage()

   # Run through the files and find the sections
   levels = []
   sections = []
   buffer = ''
   index = '1'
   header = ''
   min_depth = False
   for file in files(path):
      lines = skip_bom(open(file, 'r').read()).split('\n')
      for line in lines:
         if line.startswith('#'):
            if header:
               buffer = ''.join([c for c in buffer if ord(c) < 128])
               sections.append((index, header, buffer))
            
            buffer = line

            pounds, header = line.split(' ', 1)
            depth = pounds.count('#')

            if not min_depth or depth < min_depth:
               min_depth = depth

            if depth == len(levels):
               levels[-1] += 1
            elif depth < len(levels):
               levels = levels[:depth]
               levels[-1] += 1
            else:
               while depth > len(levels):
                  levels.append(1)

            index = '.'.join([str(s) for s in levels])
         else:
            buffer += line + '\n'
   if buffer:
      buffer = ''.join([c for c in buffer if ord(c) < 128])
      sections.append((index, header, buffer))

   # Generate the book contents
   i = 0
   for index, header, content in sections:
      #if min_depth:
      #   index = '.'.join(index.split('.')[min_depth:])

      print('%s = %s' % (index, header))
      i += 1
      item = book.addHtml('', '%04d.html' % i, html(header, markdown(content)))
      book.addSpineItem(item)
      book.addTocMapNode(item.destPath, header, index.count('.') + 1)

   # Create the book
   book.createBook(temp)
   EpubBook.createArchive(temp, target)

# Get all files
def files(path):
   files = glob(os.path.join(path, '[0-9]*txt'))
   if not files:
      files = glob(os.path.join(path, '*', '[0-9]*txt'))
   for file in sorted(files):
      yield file

# Return minimal HTML
def html(title, content):
   return """<!DOCTYPE html PUBLIC "-//W3C//DTD XHtml 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head><title>%s</title></head>
<body><p>%s</p></body>
</html>""" % (title, content)

# Helper function to skip the byte order mark in a unicode string if present
def skip_bom(str):
   return str[3:] if str[:3] == '\xef\xbb\xbf' else str

if __name__ == '__main__':
   main()
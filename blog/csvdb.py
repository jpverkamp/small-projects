#!/usr/bin/env python

import os
import sqlite3
import sys

loaded_tables = []

def display_help():
   '''Display the help string.'''
   print ('''
Enter a SQL query or one of the special commands below.

Current tables:
%s

Special commands:
quit - exit
help - display this screen
cols {table} - print out the columns for the given table
''' % ('\n'.join(loaded_tables))).strip()

def usage():
   '''Print usage and exit.'''

   print '''
Usage: csvdb {csv files}
'''.strip()
   sys.exit(0)

def load_file(db, filename):
   '''Load a file into the given database.'''

   # Get the table name
   tablename = os.path.splitext(os.path.basename(filename))[0]

   # Load data, assume first row has headers
   with open(filename, 'r') as f:
      data = [line.split(',') for line in f.read().split('\n')]
   keys = data[0]
   data = data[1:]

   # Filter out any rows with the incorrect number of entries
   data = [line for line in data if len(line) == len(keys)]

   # Create the table
   sql = 'CREATE TABLE "' + tablename + '" ("' + '" text, "'.join(keys) + '" text)'
   db.execute(sql)
   for line in data:
      line = [item.replace('"', "'") for item in line]
      sql = 'INSERT INTO  "' + tablename + '" VALUES ("' + '","'.join(line) + '")'
      db.execute(sql)
   db.commit()

   # Remember the table
   loaded_tables.append(tablename)
   print '%d rows loaded from %s as %s' % (len(data), filename, tablename)

def main():
   '''Run the main program.'''

   # Create an in-memory database
   db = sqlite3.connect(':memory:')

   # Load any files
   for filename in sys.argv[1:]:
      load_file(db, filename)

   # Be helpful
   display_help()
   print

   # Run the REPL
   while True:
      cmd = raw_input('~ ')

      # Special commands
      if cmd.lower() in ['quit', 'exit', 'q']:
         break
      elif cmd.lower() in ['help', '?']:
         display_help()
      elif cmd.lower() in ['tables', 'files']:
         print ', '.join(loaded_tables)
      elif cmd.lower().startswith('cols'):
         table = ' '.join(cmd.split(' ')[1:])
         result = db.execute('select * from "%s"' % table)
         print ', '.join([col[0] for col in result.description])

      # Otherwise assume a query
      else:
         try:
            results = db.execute(cmd)
            cols = [col[0] for col in results.description]
            rows = list(results)
            longest = [len(str(col)) for col in cols]
            for row in rows:
               for i, col in enumerate(row):
                  longest[i] = max(longest[i], len(str(col)))
            longest = [i + 2 for i in longest]

            for row in [cols] + rows:
               for i, col in enumerate(row):
                  print (('%%%ds' % longest[i]) % col),
               print

         except Exception, e:
            print 'Error: %s' % e

      print

   # All done
   print 'Have a nice day!'

if __name__ == '__main__':
   if len(sys.argv) == 1:
      usage()
   main()

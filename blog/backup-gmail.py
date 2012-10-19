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

import codecs, datetime, email, email.utils
import imaplib, os, re, sys, time

from pprint import pprint

username = '#####'
password = '#####'

imap_host = 'imap.gmail.com'
imap_port = 993

mailbox = '[Gmail]/All Mail'

output_dir = '#####'
safe_chars =''.join(chr(c) if chr(c).isupper() 
                           or chr(c).islower() 
                           else '_' for c in range(256))

collapse = re.compile(r'_+')

id_filename = 'ids.txt'

if not os.path.exists(output_dir):
    os.makedirs(output_dir)

if os.path.exists(output_dir + id_filename):
    with open(output_dir + id_filename, 'r') as f:
        read_ids = f.read().split('\n')
else:

    read_ids = []

print('Authenticating')
mail = imaplib.IMAP4_SSL(imap_host, imap_port)
mail.login(username, password)

print('Switching to %s' % mailbox)
(state, count) = mail.select(mailbox)
count = int(count[0])
print('%s messages to read' % count)

print('Fetching ids')
result, data = mail.uid('search', None, "ALL")
ids = data[0].split()


# result, data = mail.uid('search', None, '(SENTBEFORE %s)' % datetime.date(2008, 01, 01).strftime('%d-%b-%Y'))
# ids = data[0].split()
# with open(output_dir + id_filename, 'w') as f:
#     for id in ids:
#         f.write('%s\n' % id)
#
# sys.exit(0)


id_file = open(output_dir + id_filename, 'a')

for id in ids:
    if id in read_ids:
        continue

    try:
        result, data = mail.uid('fetch', id, '(RFC822)')
        data = data[0][1]
        msg = email.message_from_string(data)


        msg_from = msg['From']
        msg_subj = msg['Subject'] if msg['Subject'] else '(no subject)'
        msg_date = datetime.datetime.fromtimestamp(time.mktime(email.utils.parsedate(msg['Date'])))

        dir = (output_dir + '%04d/%02d/') % (msg_date.year, msg_date.month)
        if not os.path.exists(dir):
            os.makedirs(dir)

        filename = '%04d%02d%02d-%02d%02d%02d-%s' % (msg_date.year, msg_date.month, msg_date.day, msg_date.hour, \
msg_date.minute, msg_date.second, collapse.sub('_', msg_subj.translate(safe_chars)).strip('_'))

        print('%s: %s' % (id, filename))

        with open(dir + filename, 'w') as f:
            f.write(data)

    except Exception, ex:
        print('%s: %s' % (id, ex))

    id_file.write('%s\n' % id)
    id_file.flush()
    read_ids.append(id)

id_file.close()
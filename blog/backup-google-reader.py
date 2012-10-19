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

import urllib, urllib2

username = '#####'
password = '#####'

# Authenticate to obtain SID
auth_url = 'https://www.google.com/accounts/ClientLogin'
auth_req_data = urllib.urlencode({'Email': username, 'Passwd': password, 'servi\
ce': 'reader'})
auth_req = urllib2.Request(auth_url, data=auth_req_data)
auth_resp = urllib2.urlopen(auth_req)
auth_resp_content = auth_resp.read()
auth_resp_dict = dict(x.split('=') for x in auth_resp_content.split('\n') if x)
auth_token = auth_resp_dict["Auth"]

# Create a cookie in the header using the SID
header = {}
header['Authorization'] = 'GoogleLogin auth=%s' % auth_token

reader_url = 'https://www.google.com/reader/subscriptions/export?hl=en'
reader_req = urllib2.Request(reader_url, None, header)
reader_resp = urllib2.urlopen(reader_req)
reader_resp_content = reader_resp.read()

print reader_resp_content
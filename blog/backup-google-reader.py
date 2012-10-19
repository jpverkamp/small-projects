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

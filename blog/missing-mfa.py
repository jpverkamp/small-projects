#!/usr/bin/env python3

import requests
import os
import sys

try:
	token = os.environ['GITHUB_TOKEN']
	organization = sys.argv[1]
except:
	print('''Usage:
	missing-mfa.py [organization]
	$GITHUB_TOKEN must be set with proper permission''')
	sys.exit(0)

headers = {'Authorization': 'token {}'.format(token)}

def api_iterator(endpoint):
	url = 'https://api.github.com' + endpoint

	while True:
		response = requests.get(url, headers = headers)
		yield from response.json()

		if 'next' in response.links:
			url = response.links['next']['url']
		else:
			return

for user in api_iterator('/orgs/{}/members?filter=2fa_disabled'.format(organization)):
	print(user['login'])

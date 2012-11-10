#!/usr/bin/env python

import sys
from collections import defaultdict as ddict

def ip_to_int(ip):
    try:
        return ip[0] * 16777216 + ip[1] * 65536 + ip[2] * 256 + ip[3]
    except:
        return 0

batch_mode = '--batch' in sys.argv
if batch_mode: sys.argv.remove('--batch')

ip_file = 'GeoIPCountryWhois.csv'
if '--ip' in sys.argv:
    ip = sys.argv.index('--ip')
    ip_file = sys.argv[ip+1]
    del sys.argv[ip:ip+2]
    
files = sys.argv[1:]

ipdb = []
with open(ip_file, 'r') as fin:
    for line in fin:
        parts = [part.strip('"') for part in line.strip().split(',')]
        if len(parts) == 6:
            from_ip, to_ip, from_int, to_int, cc, country = parts
            ipdb.append((int(from_int), int(to_int), country))
            
countries = ddict(lambda : 0)
for file in files:
    if file == '-':
        fin = sys.stdin
    else:
        fin = open(file, 'r')

    for line in fin:
        try:
            ip = map(int, line.strip().split('.'))
        except:
            continue

        ip_int = ip_to_int(ip)
        answer = None
        for from_int, to_int, country in ipdb:
            if from_int <= ip_int <= to_int:
                answer = country
                break

        if not batch_mode:
            print('%s,%s' % (line.strip(), answer))

        countries[answer] += 1

    fin.close()

if batch_mode:
    for entry in sorted(countries.items()):
        print('%s,%s' % entry)
        

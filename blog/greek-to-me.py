#!/usr/bin/env python

import collections
import bs4
import graphviz
import pprint
import re
import requests
import sys

# --- Scrape data form wikipedia ---

content = requests.get('https://en.wikipedia.org/wiki/Greek_to_me').text
soup = bs4.BeautifulSoup(content)

table = soup.find('table', {'class': 'wikitable sortable'})

pairs = collections.defaultdict(set)

for row in table.findAll('tr'):
    cols = row.findAll('td')
    if not cols:
        continue

    if len(cols) == 5:
        srcs = [src.strip() for src in cols[0].text.split(',')]

    for i, src in enumerate(srcs):
        if src in ['Cantonese', 'Mandarin']:
            srcs[i] = u'Chinese'

    dsts = [dst.strip() for dst in cols[-1].text.split(',')]
    for i, dst in enumerate(dsts):
        dsts[i] = re.sub(r'[\[(].*?[\])]', '', dst)

    for src in srcs:
        if ' ' in src: continue

        for dst in dsts:
            if ' ' in dst: continue

            pairs[src].add(dst)

# --- Find cycles in the graph ---

def cycle(node, seen):

    for neighbor in pairs[node]:
        new_seen = seen + [neighbor]

        if neighbor in seen:
            yield new_seen[new_seen.index(neighbor):]
        else:
            for recur in cycle(neighbor, new_seen):
                yield recur

def reorder(cycle):
    if cycle[0] == cycle[-1]:
        cycle = cycle[1:]

    smallest = min(cycle)
    for el in list(cycle):
        if el == smallest:
            break
        else:
            cycle = cycle[1:] + [cycle[0]]

    return cycle

seen = set()

for src in pairs.keys():
    for result in cycle(src, [src]):
        result = reorder(result)
        if not str(result) in seen:
            print(result)
            seen.add(str(result))

# --- Render a nice graph ---

g = graphviz.Digraph()
for src in pairs.keys():
    for dst in pairs[src]:
        g.edge(src, dst)

g.graph_attr['overlap'] = 'false'
g.graph_attr['splines'] = 'true'

g.format = 'png'
g.engine = 'neato'

g.render('greek-to-me')

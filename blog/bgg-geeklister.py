import requests
import sys

from xml.etree import ElementTree as et

rankings = {}

for list in sys.argv[1:]:
    response = requests.get(f'https://www.boardgamegeek.com/xmlapi/geeklist/{list}')
    xml = et.fromstring(response.text)
    for item_el in xml.findall('item'):
        game_id = item_el.attrib['objectid']
        game_name = item_el.attrib['objectname']
        sys.stderr.write(f'Loading {game_name} ({game_id})\n')

        game_response = requests.get(f'https://www.boardgamegeek.com/xmlapi/boardgame/{game_id}?stats=1')
        game_xml = et.fromstring(game_response.text)

        for rank_el in game_xml.findall('.//rank'):
            try:
                rank_name = rank_el.attrib['friendlyname']
                rank_value = int(rank_el.attrib['value'])
            except ValueError:
                continue

            if rank_name not in rankings:
                rankings[rank_name] = []

            rankings[rank_name].append((rank_value, game_name))

for ranking, games in rankings.items():
    print(f'===== {ranking} =====')
    for i, (score, game) in enumerate(sorted(games), 1):
        print(f'{i: <5} {score: <5} {game}')
    print()

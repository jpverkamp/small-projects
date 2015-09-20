#!/usr/bin/env python3

import difflib
import fractions
import inflect
import json
import os
import re
import sys

singular = inflect.engine().singular_noun

unit_conversions = [
	('¼', '1/4'), ('½', '1/2'), ('¾', '3/4'), ('⅓', '1/3'), ('⅔', '2/3'),
	('⅕', '1/5'), ('⅖', '2/5'), ('⅗', '3/5'), ('⅘', '4/5'), ('⅙', '1/6'),
	('⅚', '5/6'), ('⅛', '1/8'), ('⅜', '3/8'), ('⅝', '5/8'), ('⅞', '7/8'),
]

# Load price database
if os.path.isfile('prices.json'):
    with open('prices.json', 'r') as fin:
        prices = json.load(fin)
else:
    prices = {}

# Save prices (call after changes)
def save_prices():
    with open('prices.json', 'w') as fout:
        json.dump(prices, fout, indent = 4, sort_keys = True)

# Check for matching item
def check_known_items(item):
    for known_item in prices:
        if difflib.SequenceMatcher(None, item, known_item).ratio() > 0.9:
            y_or_n = input('Is {item} the same as {known_item}?'.format(
                item = item,
                known_item = known_item
            ))
            if y_or_n.strip().lower()[0] == 'y':
                return known_item

    return False

# Get new item
def get_item_price(item, unit, qty):

    if not item in prices:
        known_item = check_known_items(item)
        if known_item:
            item = known_item

    if not item in prices:
        prices[item] = {}

    if not unit in prices[item] and prices[item]:
        conversion_unit = input('\nI don\'t recognize {unit} of {item}. Can you convert from:\n{list}\n\nEither type a unit or "no" if none apply: '.format(
            unit = unit,
            item = item,
            list = '- ' + '\n- '.join(prices[item].keys()),
        ))
        if conversion_unit.lower().strip() != 'no':
            conversion_qty = input('How many {new_unit} are in 1 {old_unit}? '.format(
                old_unit = conversion_unit,
                new_unit = unit,
            ))
            conversion_qty = float(conversion_qty)

            new_price = prices[item][conversion_unit] / conversion_qty
            prices[item][unit] = new_price
            save_prices()

    if not unit in prices[item]:
        price = input('\nHow much would {qty} {unit} of {item} cost? '.format(
            unit = unit,
            qty = qty,
            item = item,
        ))

        normalized_price = float(price) / qty
        prices[item][unit] = normalized_price
        save_prices()

    return prices[item][unit] * qty

def get_recipe_price():
    items = []
    while True:
        line = sys.stdin.readline()
        line = line.lower().strip()

        if not line:
            break

        if line == 'quit':
            sys.exit(0)

        line = re.sub(r'of', '', line)
        line = re.sub(r'\(.*?\)', '', line)
        line = re.sub(r'\s+', ' ', line)

        parts = line.split(' ', 2)
        if len(parts) == 1:
            parts = ['1', 'each', parts[0]]
        elif len(parts) == 2:
            parts = [parts[0], 'each', parts[1]]
        qty, unit, item = parts

        for (src, dst) in unit_conversions:
            qty = qty.replace(src, dst)

        try:
            qty = float(qty)
        except:
            qty = float(fractions.Fraction(qty))

        unit = singular(unit) or unit
        item = singular(item) or item

        items.append((qty, unit, item))

    total_price = 0
    failed_items = []
    for (qty, unit, item) in items:
        try:
            total_price += get_item_price(item, unit, qty)
        except ValueError:
            failed_items.append(item)

    if failed_items:
        print('Failed to get prices for: {}'.format(', '.join(failed_items)))
    return total_price

# While true
while True:
    print('Enter a recipe, in the format {qty} {unit} {item}, one item per line, blank line to finish')
    print('Type "quit" to exit')
    total_price = get_recipe_price()
    print('Total price: ${price:.2f}'.format(price = total_price))
    print()

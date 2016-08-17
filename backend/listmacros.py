#!/usr/bin/env python3
import json

db = {}
with open('types.json', 'r') as f:
    db = json.load(f)

def listmacros(db):
    if db[0] == 'Tip':
        return
    listmacros(db[4])
    print(db[2])
    listmacros(db[5])

listmacros(db['macromap'])

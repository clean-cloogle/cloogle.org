#!/usr/bin/env python3
import glob
import json
import re
from subprocess import call

def extract_keywords(lines):
    keywords = [l[10:-1].split(';') for l in lines if l[:10] == 'Keywords: ']
    keywords = sum(keywords, [])
    keywords = [re.escape(kw.strip()).replace(r'\%', r'.*') for kw in keywords]
    return keywords

def extract_description(lines):
    desc = ''

    reading = False
    for l in lines:
        if l[:3] == '## ':
            break
        if reading:
            desc += l
        if l[:10] == 'Keywords: ':
            reading = True

    return desc

def extract_solutions(lines):
    sols = []

    reading = False
    for l in lines:
        if l[:-1] == '## Examples':
            break
        if reading:
            if l[:2] == '- ':
                sols.append(l[2:-1])
            elif l[:2] == '  ':
                sols[-1] += ' ' + l[2:-1]
        if l[:-1] == '## Solutions':
            reading = True

    return sols

def extract_examples(lines):
    exs = ['']

    reading = False
    for l in lines:
        if reading:
            if l[:-1] == '---':
                exs.append('')
            else:
                exs[-1] += l
        if l[:-1] == '## Examples':
            reading = True

    return [ex for ex in exs]

def index(fname):
    with open(fname, 'r') as f:
        lines = f.readlines()

        title = lines[0][2:-1]
        keywords = extract_keywords(lines)
        description = extract_description(lines)
        solutions = extract_solutions(lines)
        examples = extract_examples(lines)

        return {
            'key': fname[:-3],
            'title': title,
            'keywords': keywords,
            'description': description,
            'solutions': solutions,
            'examples': examples
            }

if __name__ == '__main__':
    call(['git', 'clone', 'https://github.com/clean-cloogle/common-problems'])

    problems = []

    for f in glob.glob('common-problems/*.md'):
        if f != 'README.md':
            problems.append(index(f))

    with open('common-problems.json', 'w') as f:
        f.write(json.dumps(problems))

    call(['rm', '-r', '-f', 'common-problems'])

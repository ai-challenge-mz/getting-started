#!/usr/bin/env python

import collections
import fileinput
import random
import sys

Planet = collections.namedtuple('Fleet', 'id x y production owner population')


def worlds():
    world = {'planets': [], 'id': None}
    while True:
        line = sys.stdin.readline().rstrip()
        if line == '.':
            yield world
            world = {'planets': [], 'id': None}
        elif line.startswith('P '):
            args = [int(x) for x in line[2:].split()]
            world['planets'].append(Planet(*args))
        elif line.startswith('Y '):
            world['id'] = int(line[2:])


def react(world):
    my_planets = [p for p in world['planets'] if p.owner == world['id']]
    enemies = [p for p in world['planets'] if p.owner != world['id']]
    for p in my_planets:
        if p.population > 2 ** abs(p.production):
            yield 'B {0}'.format(p.id)
        elif p.population > 18 and p.production > 1:
            target = enemies[random.randint(0, len(enemies) - 1)]
            yield 'F {0} {1} {2}'.format(p.id, target.id, p.production - 1)


def main():
    for world in worlds():
        for order in react(world):
            print order
        print '.'
        sys.stdout.flush()


if __name__ == '__main__':
    main()
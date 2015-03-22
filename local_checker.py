#!/usr/bin/env python

import os
import sys
import math
import heapq
import time
import random
import itertools
import argparse
import subprocess
import collections
import re

NEUTURAL_ID = 0
CUSTOM_BOT_ID = random.randint(2, 100)
IDLE_BOT_ID = 1

def first(iterable, default=None):
    if iterable:
        for item in iterable:
            return item
    return default

def distance(a, b):
    dx = a.x - b.x
    dy = a.y - b.y
    d = math.sqrt(math.pow(dx,2) + math.pow(dy, 2))
    return math.ceil(d)

def groupby(coll, f):
    res = collections.defaultdict(list)
    for k in coll: res[f(k)].append(k)
    return res.items()

Fleet = collections.namedtuple('Fleet', 'player source planet count launch_turn arrival_turn')

class Planet(object):

    def __init__(self, id, owner, population, production, x, y):
        self.id = id
        self.owner = owner
        self.population = population
        self.production = production
        self.x = x
        self.y = y

    def __repr__(self):
        return "Planet(id=%d, owner=%d, population=%d, production=%d, x=%d, y=%d)" % (self.id, self.owner, self.population, self.production, self.x, self.y)

def gen_planets(map_size):
    generators =  {'small': [  Planet(1, CUSTOM_BOT_ID, 4, 2, 0, 0),
                               Planet(2, IDLE_BOT_ID,   4, 2, 4, 4),
                               Planet(3, NEUTURAL_ID,   4, 2, 0, 4),
                               Planet(4, NEUTURAL_ID,   4, 2, 4, 0)],

                   'medium': [ Planet(1, CUSTOM_BOT_ID, 4, 2, 0, 0),
                               Planet(2, IDLE_BOT_ID,   4, 2, 3, 3),
                               Planet(3, NEUTURAL_ID,   4, 4, 1, 1),
                               Planet(4, NEUTURAL_ID,   4, 4, 2, 2),
                               Planet(5, NEUTURAL_ID,   4, 2, 2, 0),
                               Planet(6, NEUTURAL_ID,   4, 2, 3, 1),
                               Planet(7, NEUTURAL_ID,   4, 2, 1, 3),
                               Planet(8, NEUTURAL_ID,   4, 2, 0, 2) ]}

    if map_size not in generators:
        raise Exception("Unknown map size %s" % (map_size))

    return generators[map_size]

class World(object):

    def __init__(self, planets):
        self.planets = { x.id: x for x in planets }
        self.fleets = []
        self.num_turn = 0

    def as_str(self):
        sb = []
        for planet in self.planets.values():
            sb.append("P %d %d %d %d %d %d" % (planet.id, planet.x, planet.y, planet.production, planet.owner, planet.population))
        return "\n".join(sb)

    def find_planet(self, planet_id):
        if planet_id not in self.planets:
            raise Exception("Unknown planet with id %d" % (planet_id))
        return self.planets[planet_id]

    def do_battle(self):
        active = []

        while (0 < len(self.fleets)) and (self.fleets[0][0] <= self.num_turn):
            (turn, cmd) = heapq.heappop(self.fleets)
            assert turn == self.num_turn, 'battle turn is less then world turn'
            list.append(active, cmd)

        for planet, incoming_fleets in groupby(active, lambda x: x.planet):
            groupby_player = groupby(incoming_fleets, lambda x: x.player)

            competitors = []
            for player, fleets in groupby_player:
                num_fleets = sum([x.count for x in fleets])
                if (player == planet.owner):
                    planet.population = planet.population + num_fleets
                else:
                    list.append(competitors, (num_fleets, player))

            if 0 == len(competitors): return

            competitors = sorted(competitors, reverse=True)

            count_a, player_a = competitors[0]
            count_b, player_b = (planet.population, planet.owner) if 1 == len(competitors) else competitors[1]

            if (count_a == count_b):
                planet.population -= min(count_a, planet.population)
            else:
                if count_a <= planet.population:
                    planet.population -= count_a
                else:
                    opponent_count = max(count_b, planet.population)
                    planet.population = count_a - opponent_count
                    planet.owner = player_a

    def increase_population(self):
        for planet in self.planets.values():
            if planet.owner != NEUTURAL_ID:
                planet.population += planet.production
                if planet.population < 0:
                    planet.population = 0

    def start_new_turn(self):
        self.num_turn = self.num_turn + 1

    def total_fleet(self, player_id):
        defence = sum([p.population for p in self.planets.values() if p.owner == player_id])
        assault =  sum([cmd.count for (_, cmd) in self.fleets if cmd.player == player_id])
        return defence + assault

    def is_finished(self):
        return (200 < self.num_turn or 0 == self.total_fleet(IDLE_BOT_ID) or 0 == self.total_fleet(CUSTOM_BOT_ID))

    def send_fleet(self, player_id, cmd):
        (start_id, end_id, count) = map(int, cmd.split()[1:])
        start = self.find_planet(start_id)

        if player_id != start.owner:
            raise Exception("You don't own planet %d to send fleet from it" % (start_id))

        end = self.find_planet(end_id)

        if start.population < count:
            raise Exception("Planet %d doesn't have %d spacecrafts" % (start.id, count))

        start.population -= count
        dist = distance(start, end)

        assert 0 != dist, "Distance between planets can't be zero"

        fleet = (self.num_turn + dist,
            Fleet(player_id, start, end, count, self.num_turn, self.num_turn + dist))
        heapq.heappush(self.fleets, fleet)

    def increase_production(self, player_id, cmd):
        (planet_id,) = map(int, cmd.split()[1:])
        planet = self.find_planet(planet_id)

        if player_id != planet.owner:
            raise Exception("You don't own planet %d to boost production on it" % (planet_id))

        price = math.pow(2, abs(planet.production))

        if planet.population < price:
            raise Exception("Planet %d requires %d population to boost production" % (planet_id, price))

        planet.population = planet.population - price
        planet.production = planet.production + 1

    def execute_cmd(self, player_id, s):
        if s.strip():
            cmd_name = s.split()[0].upper()
            if   ("F" == cmd_name):
                self.send_fleet(player_id, s)
            elif ("B" == cmd_name):
                self.increase_production(player_id, s)
            else:
                raise Exception("Unknown command: %s", s)


class Replay(object):
    def __init__(self):
        self.states = []
        self.result = None
    def add_state(self, world):
        self.states.append(
            {'turn': world.num_turn,
             'planets': [dump_planet(p) for _, p in world.planets.iteritems()],
             'fleets': [dump_fleet(f) for _, f in world.fleets]})
    def set_result(self, result):
        self.result = result
    def save(self):
        import json
        replay_json = json.dumps({'states': self.states, 'result': self.result}, indent=2)
        replay_html = "\n".join((
            '''
            <!DOCTYPE html>
            <html>
              <head>
                <script language="javascript" src="all.js"></script>
              </head>
              <body>
                <pre id="json" style="display:none">''',
            replay_json,
            '''
                </pre>
              </body>
              <script language="javascript" defer>h$main(h$mainZCMainzimain);</script>
            </html>
            '''))
        replay_html = re.sub(r'\r', '', replay_html)
        with open('index.html', 'w') as f:
            f.write(replay_html)


def dump_planet(p):
    return {'id': p.id,
        'x': p.x,
        'y': p.y,
        'production': p.production,
        'population': p.population,
        'owner': p.owner}


def dump_fleet(f):
    return {'owner': f.player,
        'size': f.count,
        'destination': f.planet.id,
        'source': f.source.id,
        'launchTurn': f.launch_turn,
        'arrivalTurn': f.arrival_turn}

class Player(object):

    def __init__(self, sh_cmd, player_id):
        sh_cmd = sh_cmd[0].split() if (1 == len(sh_cmd)) else sh_cmd
        self.process = subprocess.Popen(sh_cmd, stdin = subprocess.PIPE, stdout = subprocess.PIPE, stderr = sys.stderr, bufsize = 1)
        self.stdout = self.process.stdin
        self.stdin = self.process.stdout
        self.player_id = player_id

    def send_world(self, world):
        self.stdout.write("%s\nY %d\n.\n" % (world.as_str(), self.player_id))
        self.stdout.flush()

    def turn(self, world):
        while True:
            cmd = self.stdin.readline()
            cmd = cmd.strip()
            if not cmd: continue
            if "." == cmd: break
            world.execute_cmd(self.player_id, cmd)

    def close(self):
        if self.process.poll() is None:
            self.process.terminate()

class MyParser(argparse.ArgumentParser):

    def error(self, message):
        sys.stderr.write('error: %s\n' % message)
        self.print_help()
        sys.exit(2)

def print_game_result(world, player):
    sys.stdout.write("World\n")
    sys.stdout.write(world.as_str() + "\n\n")

    result = {'turn': world.num_turn,
        'planets': [dump_planet(p) for _, p in world.planets.iteritems()]}

    player_fleet   = world.total_fleet(player.player_id)
    idle_bot_fleet = world.total_fleet(IDLE_BOT_ID)

    if (player_fleet == idle_bot_fleet):
        sys.stdout.write("Draw\n")
    elif (player_fleet < idle_bot_fleet):
        result['winner'] = 'Brave idle bot'
        sys.stdout.write("You lose!\n")
    else:
        result['winner'] = 'You'
        sys.stdout.write("You win!\n")

    return result


def game(args):
    player = Player(args.bot, CUSTOM_BOT_ID)
    planets = gen_planets(args.map_size)
    world = World(planets)
    replay = Replay()
    replay.add_state(world)

    try:
        while not world.is_finished():
            world.start_new_turn()
            player.send_world(world)

            start = time.time()
            player.turn(world)
            diff = time.time() - start
            if (2 < diff) and (1 < world.num_turn):
                sys.stdout.write("Warning: Your bot spent more then %f second to take turn #%d\n" % (diff, world.num_turn))

            world.increase_population()
            world.do_battle()
            replay.add_state(world)
        result = print_game_result(world, player)
        replay.set_result(result)
        replay.save()
    except:
        sys.stderr.write("\nException at turn %d\n" % (world.num_turn))
        sys.stderr.write("\nWorld:\n" + world.as_str() + "\n\n")
        raise
    finally:
        player.close()

def main():
    usage ='%(prog)s [arguments] <bot>'
    description = "Command-line tool to test AI challenge bots"
    parser = MyParser(prog = 'ai-challenge', usage = usage, description = description, formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("-m", "--map-size", default = "small", choices = ["small", "medium"], help = "Map size")
    parser.add_argument("bot", help = "Bot executable command", nargs='+')
    args = parser.parse_args()
    game(args)

if "__main__" == __name__:
    main()

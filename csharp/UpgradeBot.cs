using System;
using System.Collections.Generic;
using System.Linq;

namespace UpgradeBot
{
    class Planet
    {
        public int Id { get; set; }
        public int Owner { get; set; }
        public int GrowthRate { get; set; }
        public int NumShips { get; set; }
        public int X { get; set; }
        public int Y { get; set; }

    }

    internal class World
    {
        public int PlayerId { get; set; }
        public IList<Planet> Planets { get; set; }

        public World()
        {
            Planets = new List<Planet>();
        }

        public IEnumerable<Planet> PlanetsByOwner(int playerId)
        {
            return from planet in Planets
                   where planet.Owner == playerId
                   select planet;
        }
    }

    internal class UpgradeBot
    {
        private static IEnumerable<String> TakeTurns(World world)
        {
            var turns = new List<string>();

            var boost = (from p in world.PlanetsByOwner(world.PlayerId)
                         where (p.GrowthRate <= 6) && ((1 << p.GrowthRate) <= p.NumShips)
                         select p).FirstOrDefault();


            if (null != boost)
            {
                turns.Add(String.Format("B {0}", boost.Id));
                boost.NumShips -= (1 << boost.GrowthRate);
            }

            var force = (from source in world.PlanetsByOwner(world.PlayerId)
                         from target in world.PlanetsByOwner(0).OrderBy(x => x.NumShips)
                         where target.NumShips < source.NumShips
                         select new { source, target }).FirstOrDefault();

            if (null != force)
            {
                turns.Add(String.Format("F {0} {1} {2}", force.source.Id, force.target.Id, force.target.NumShips + 1));
                force.source.NumShips -= force.target.NumShips + 1;
            }
            return turns;
        }

        private static World ReadWorld()
        {
            var world = new World();
            var doRead = true;
            while (doRead)
            {
                var line = Console.In.ReadLine();
                if (null == line) break;
                var tokens = line.Trim().Split();
                switch (tokens.First())
                {
                    case "P":
                        {
                            var parameters = tokens.Skip(1).Select(int.Parse).ToArray();
                            var planet = new Planet
                                             {
                                                 Id = parameters[0],
                                                 X = parameters[1],
                                                 Y = parameters[2],
                                                 GrowthRate = parameters[3],
                                                 Owner = parameters[4],
                                                 NumShips = parameters[5]
                                             };
                            world.Planets.Add(planet);
                            break;
                        }

                    case "Y":
                        {
                            var parameters = tokens.Skip(1).Select(int.Parse).ToArray();
                            world.PlayerId = parameters[0];
                            break;
                        }

                    case ".":
                        doRead = false;
                        break;

                }
            }
            return world;
        }

        private static void WriteTurns(IEnumerable<String> turns)
        {
            foreach (var turn in turns)
            {
                Console.Out.WriteLine(turn);
            }
            Console.Out.WriteLine(".");
            Console.Out.Flush();
        }


        static void Main(string[] args)
        {
            while (true)
            {
                var world = ReadWorld();
                var turns = TakeTurns(world);
                WriteTurns(turns);
            }
        }
    }
}

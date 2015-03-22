import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class UpgradeBot {

    public List<String> takeTurns(World world) {
        List<String> turns = new ArrayList<String>();
        List<Planet> myPlanets = world.getPlanetsByOwner(world.getPlayerId());

        for (Planet planet : myPlanets) {
            if ((planet.getGrowthRate() <= 6) && ((1 << planet.getGrowthRate()) <= planet.getNumShips())) {
                planet.removeShips(1 << planet.getGrowthRate());
                turns.add(World.boost(planet.getPlanetId()));
                break;
            }
        }

        List<Planet> neutralPlanets = world.getPlanetsByOwner(World.NEUTRAL_ID);
        if (!neutralPlanets.isEmpty()) {

            Planet target = Collections.min(neutralPlanets, new Comparator<Planet>() {
                public int compare(Planet planetA, Planet planetB) {
                    return Integer.compare(planetA.getNumShips(), planetB.getNumShips());
                }
            });

            for (Planet assault : myPlanets) {
                if (target.getNumShips() < assault.getNumShips()) {
                    assault.removeShips(target.getNumShips() + 1);
                    turns.add(World.sendFleet(assault.getPlanetId(), target.getPlanetId(), target.getNumShips() + 1));
                    break;
                }
            }
        }
        return turns;
    }

    static World readWorld(BufferedReader reader) throws Exception {
        List<String> container = new ArrayList<String>();

        World world = new World();

        while (true) {
            String line = reader.readLine().trim();
            if (line.isEmpty()) continue;
            if (".".equals(line)) break;

            String[] tokens = line.split("\\s+");
            String command = tokens[0];

            if ("P".equals(command)) {
                int planetId = Integer.parseInt(tokens[1]);
                int x = Integer.parseInt(tokens[2]);
                int y = Integer.parseInt(tokens[3]);
                int growthRate = Integer.parseInt(tokens[4]);
                int playerId = Integer.parseInt(tokens[5]);
                int numShips = Integer.parseInt(tokens[6]);

                Planet planet = new Planet(planetId, playerId, numShips, growthRate, x, y);
                world.addPlanet(planet);
            } else if ("Y".equals(command)) {
                int playerId = Integer.parseInt(tokens[1]);
                world.setPlayerId(playerId);
            }
        }
        return world;
    }

    static void writeTurns(PrintWriter writer, List<String> commands) {
        for (String command : commands) {
            writer.println(command);
        }
        writer.println(".");
    }

    public static void game(BufferedReader reader, PrintWriter writer) throws Exception {
        UpgradeBot bot = new UpgradeBot();
        while (true) {
            World world = readWorld(reader);
            List<String> turns = bot.takeTurns(world);
            writeTurns(writer, turns);
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        PrintWriter writer = new PrintWriter(System.out, true);
        try {
            game(reader, writer);
        } finally {
            reader.close();
            writer.close();
        }
    }
}

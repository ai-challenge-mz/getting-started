import java.util.ArrayList;
import java.util.List;

public class World {
    public static final int NEUTRAL_ID = 0;
    private List<Planet> planets;
    private int playerId;


    World() throws Exception {
        this.playerId = -1;
        this.planets = new ArrayList<Planet>();
    }

    public int getPlayerId() {
        return this.playerId;
    }

    public void setPlayerId(int playerId) {
        this.playerId = playerId;
    }

    public void addPlanet(Planet planet) {
        this.planets.add(planet);
    }

    public List<Planet> getPlanets() {
        return this.planets;
    }

    public List<Planet> getPlanetsByOwner(int playerId) {
        List<Planet> ret = new ArrayList<Planet>();
        for (Planet planet : getPlanets()) {
            if (planet.getOwner() == playerId) {
                ret.add(planet);
            }
        }
        return ret;
    }

    public static String sendFleet(int srcPlanetId, int dstPlanetId, int count) {
        return String.format("F %d %d %d", srcPlanetId, dstPlanetId, count);
    }

    public static String boost(int planetId) {
        return String.format("B %d", planetId);
    }
}

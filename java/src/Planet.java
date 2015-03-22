public class Planet {
    public int getPlanetId() {
        return planetId;
    }

    public int getOwner() {
        return owner;
    }

    public void addShips(int amount) {
        numShips += amount;
    }

    public void removeShips(int amount) {
        numShips -= amount;
    }

    public int getGrowthRate() {
        return growthRate;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public int getNumShips() {
        return numShips;
    }

    private int planetId;
    private int owner;
    private int numShips;
    private int growthRate;
    private int x, y;

    public Planet(int planetId,
                  int owner,
                  int numShips,
                  int growthRate,
                  int x,
                  int y) {
        this.planetId = planetId;
        this.owner = owner;
        this.numShips = numShips;
        this.growthRate = growthRate;
        this.x = x;
        this.y = y;
    }
}

#!/usr/bin/env ruby

class Planet
  attr_reader :id, :growth_rate, :x, :y, :owner
  attr_accessor :num_ships

  def initialize(id, x, y, growth_rate, owner, num_ships)
    @id = id
    @x, @y = x, y
    @growth_rate = growth_rate
    @owner = owner
    @num_ships = num_ships
  end

  def remove_ships(n)
    @num_ships -= n
  end
end

class World
  attr_reader :planets
  attr_accessor :player_id

  def initialize
    @planets = []
  end

  def add_planet(planet)
    @planets << planet
  end

  def planets_by_owner(player_id)
    planets.select {|planet| planet.owner == player_id}
  end
end

def read_world()
  world = World.new
  while true
    case $stdin.readline.strip
    when "."
      break
    when /^P/
      planet_id, x, y, growth_rate, owner, num_ships = $_.split.drop(1).map(&:to_i)
      planet = Planet.new(planet_id, x, y, growth_rate, owner, num_ships)
      world.add_planet(planet)
    when /^Y/
      player_id, =  $_.split.drop(1).map(&:to_i)
      world.player_id = player_id
    end
  end
  world
end

def take_turns(world)
  turns = []

  boost_targets = world.planets_by_owner(world.player_id).select do |planet|
    (1 << planet.growth_rate) <= planet.num_ships && planet.growth_rate <= 6
  end

  if !boost_targets.empty?
    planet = boost_targets.first
    turns << "B #{planet.id}"
    planet.remove_ships(1 << planet.growth_rate)
  end

  victim = world.planets_by_owner(0).sort_by(&:num_ships).first
  if victim
    assault = world.planets_by_owner(world.player_id).select{|p| victim.num_ships < p.num_ships}.first
    if assault
      turns << "F #{assault.id} #{victim.id} #{victim.num_ships + 1}"
      assault.remove_ships(victim.num_ships + 1)
    end
  end

  turns
end

def write_turns(turns)
  turns.each { |t| $stdout.puts(t) }
  $stdout.puts "."
  $stdout.flush
end

begin
  while true
    world = read_world
    turns = take_turns(world)
    write_turns(turns)
  end
end

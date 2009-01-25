module Skirmish

  #
  # Main class for a Skirmish Game instance
  #
  class GameWorld

    # The top-left-most valid coordinate in this game instance
    attr_reader :upper_left_corner
    # The bottom-right-most valid coordinate in this game instance
    attr_reader :lower_right_corner
    # Units in the game (currently just one supported)
    attr_reader :units

    #
    # Initializes a game instance
    #
    # +world_rectangle+ is an array like +[x1, y1, x2, y2]+ where
    # point 1 is the top-left-most point in the game world and point 2
    # is the bottom-right-most
    #
    # +units+ is an array of arrays like +[uid, x, y]+ of unit
    # descriptions
    #
    def initialize(world_rectangle, units)
      @upper_left_corner = world_rectangle[0..1]
      @lower_right_corner = world_rectangle[2..3]
      @units = units
    end

  end

end

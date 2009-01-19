module Skirmish

  class GameWorld

    def initialize rect
      @rect = rect
    end

    def upper_left_corner
      @rect[0..1]
    end

    def lower_right_corner
      @rect[2..3]
    end

  end

end

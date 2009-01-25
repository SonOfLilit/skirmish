require 'helpers/connection'


module RequestGameTestHelper

  include ConnectionTestHelper

  def request_game options={}
    options[:request_game] = true
    ul_corner = options[:corner] || [0, 0]
    size = options[:size] || [3000, 3000]
    unit_id = options[:unit_id] || 0
    unit = options[:unit] || (0..1).map{|i| ul_corner[i] + size[i]/2 }
    unless options.has_key? :request_game_response
      options[:request_game_response] =
        "world-corner #{ul_corner.join(",")}\n" \
        "world-size #{size.join(",")}\n" \
        "unit #{unit_id} #{unit.join(",")}\n" \
        "\n"
    end
    game = connect options
    lr_corner = (0..1).map{|i| ul_corner[i] + size[i] - 1}
    assert_equal ul_corner, game.upper_left_corner, "wrong upper left corner"
    assert_equal lr_corner, game.lower_right_corner, "wrong lower right corner"
    assert_equal unit_id, game.units[0][0], "wrong unit id"
    assert_equal unit, game.units[0][1..2], "wrong unit position"
    game
  end

end

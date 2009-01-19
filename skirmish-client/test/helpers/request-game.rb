require 'helpers/connection'


module RequestGameTestHelper

  include ConnectionTestHelper

  def request_game options={}
    options[:request_game] = true
    unless options.has_key? :request_game_response
      options[:request_game_response] =
        "world-corner 0,0\n" \
        "world-size 3000,3000\n" \
        "\n"
    end
    game = connect options
    assert_equal [0, 0], game.upper_left_corner
    assert_equal [3000, 3000], game.lower_right_corner
    game
  end

end

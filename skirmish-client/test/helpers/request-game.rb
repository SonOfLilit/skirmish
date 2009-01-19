require 'helpers/connection'


module RequestGameTestHelper

  include ConnectionTestHelper

  def request_game options={}
    options[:request_game] = true
    options[:corner] ||= [0, 0]
    options[:size] ||= [3000, 3000]
    ul_corner = options[:corner]
    size = options[:size]
    unless options.has_key? :request_game_response
      options[:request_game_response] =
        "world-corner #{ul_corner[0]},#{ul_corner[1]}\n" \
        "world-size #{size[0]},#{size[1]}\n" \
        "\n"
    end
    game = connect options
    lr_corner = (0..1).map{|i| ul_corner[i] + size[i] - 1}
    assert_equal ul_corner, game.upper_left_corner, "wrong upper left corner"
    assert_equal lr_corner, game.lower_right_corner, "wrong lower right corner"
    game
  end

end

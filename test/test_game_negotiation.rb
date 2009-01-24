require 'automation'

# Story 1: Trivial game negotiation
class GameNegotiationAcceptance < Skirmish::SystemTest

  def setup
    start_server
  end

  def teardown
    stop_server
  end

  def test_simplest_case
    start_client
    start_game
    assert_equal [0, 0, 2999, 2999], client_world_rectangle
  ensure
    stop_client
  end
  def test_non_square
    configure_server_start_game :corner => [10000, 10000], :size => [3000, 4000]
  end
  def test_deep_in_space
    configure_server_start_game :corner => [10000, 10000], :size => [3000, 3000]
  end
  def test_tiny
    configure_server_start_game :corner => [100, 1000], :size => [200, 200]
  end
  def test_huge
    configure_server_start_game :corner => [100, 1000], :size => [30000, 30000]
  end
  def test_far_border
    border = 2 ** 32 # long int coordinates
    configure_server_start_game(:corner => [border - 1000, border - 1000],
                                :size => [1000, 1000])
  end

  def configure_server_start_game options
    server_configure_game options
    start_client # has to come AFTER configure_game
    start_game
    corner_x, corner_y = options[:corner]
    width, height = options[:size]
    world = client_world_rectangle
    assert_equal [corner_x, corner_y, corner_x+width-1, corner_y+height- 1], world
  ensure
    stop_client
  end

end

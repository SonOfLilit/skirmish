require 'automation'

# Story : One Unit
class OneUnitAcceptance < Skirmish::SystemTest

  def setup
    start_server
    start_client
  end

  def teardown
    stop_client
    stop_server
  end

  def test_movement
    start_game
    first_position = units[0]
    x, y = first_position
    assert_kind_of Fixnum, x
    assert_kind_of Fixnum, y
    sleep 0.1
    assert_not_equal first_position, units[0]
  end

end

require 'test/unit'

require 'helpers/request-game'


class TestNewGame < Test::Unit::TestCase

  include RequestGameTestHelper

  def test_simple_case
    request_game
  end

end

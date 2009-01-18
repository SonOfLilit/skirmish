require 'test/unit'

require 'helpers/connection'

Case = Test::Unit::TestCase

class TestNewGame < Case

  include ConnectionTestHelper

  def test_simple_case
    request_game
  end

end

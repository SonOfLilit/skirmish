require 'test/unit'

require 'helpers/request-game'


class TestNewGame < Test::Unit::TestCase

  include RequestGameTestHelper

  def test_simplest_case
    request_game
  end

  def test_simple_cases
    request_game :corner => [1000, 1000]
    request_game :corner => [0, 1000]
    request_game :corner => [1000, 0]
    request_game :corner => [1000000, 1000000]
    request_game :size => [100, 100]
    request_game :size => [1000, 1000]
    request_game :size => [10000, 100]
    request_game :size => [100, 10000]
    request_game :size => [10000, 10000]
    request_game :corner => [1000000, 1000000], :size => [10000, 10000]
  end
end

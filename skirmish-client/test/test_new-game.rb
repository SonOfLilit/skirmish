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

  def test_parse_errors
    ["", # nothing
     "asdf", # garbage
     "\n\n\n", # newlines
     "world-corner ,\n" \
     "world-size ,\n" \
     "\n", # no numbers
     "world-corner 0,0\n" \
     "world-size 100,-1\n" \
     "\n", # negative numbers
     "world-corner 0,0\n" \
     "world-size 3000,3000\n" # missing last newline
    ].each do |response|
      assert_raise NetworkProtocolError do
        request_game :request_game_response => response
      end
    end
  end
end

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
    request_game :corner => [2 ** 32 - 100] * 2, :size => [100, 100]
  end

  def test_fatal
    [# nothing
     "",
     # short
     "asdf"
    ].each do |fatal|
      ensure_server_fatal fatal, :request_game => true
    end
  end

  def response(x, y, w, h, uid, ux, uy)
    "world-corner #{x},#{y}\n" \
    "world-size #{w},#{h}\n" \
    "unit #{uid} #{ux},#{uy}\n\n"
  end

  def test_parse_errors
    [# nothing
     "",
     # garbage
     "asdf",
     # newlines
     "\n\n\n",
     # no numbers
     response("", "", "", "", "", "", ""),
     response("", 0, 3000, 3000, 0, 1500, 1500),
     response(0, "", 3000, 3000, 0, 1500, 1500),
     response(0, 0, "", 3000, 0, 1500, 1500),
     response(0, 0, 3000, "", 0, 1500, 1500),
     response(0, 0, 3000, 3000, "", 1500, 1500),
     response(0, 0, 3000, 3000, 0, "", 1500),
     response(0, 0, 3000, 3000, 0, 1500, ""),
     # negative numbers
     response(-1, 0, 3000, 3000, 0, 1500, 1500),
     response(0, 0, -3000, 3000, 0, 1500, 1500),
     response(0, 0, 3000, 3000, 0, -1, 1500),
     # missing last newline
     response(0, 0, 3000, 3000, 0, 1500, 1500)[0..-2],
     # number too big
     response(2 ** 32, 0, 3000, 3000, 0, 1500, 1500),
     response(0, 0, 2 ** 32, 3000, 0, 1500, 1500),
     response(0, 0, 3000, 3000, 2 ** 32, 1500, 1500),
     response(0, 0, 3000, 3000, 0, 1500, 2 ** 32),
     # sum too big
     response(2 ** 32 - 1000, 0, 3000, 3000, 0, 2 ** 32 - 100, 1500),
     response(0, 2 ** 32 - 1000, 3000, 3000, 0, 1500, 2 ** 32 - 100)
    ].each do |response|
      msg = "Invalid server response does not trigger NetworkProtocolError:\n" +
        response.inspect
      assert_raise NetworkProtocolError, msg do
        request_game :request_game_response => response
      end
    end
  end

end

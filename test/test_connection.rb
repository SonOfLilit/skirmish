require 'automation'

# Tests for story 0: Connection
class ConnectionTests < Skirmish::SystemTest

  def setup
    start_server
  end

  def teardown
    stop_client
    stop_server
  end

  def test_happy_path
    start_client "asd", ""
  end

  def test_boundaries
    start_client "1234567890.a`AbBcC", "A very, very, long and contrived passphrase."
  end

  def test_too_long
    assert_raise ServerFatal do
      start_client "a" * 17, ""
    end
  end

  def test_wrong_protocol_version
    assert_raise ServerFatal do
      start_client "asd", "" do
        monkey_patch_client_protocol_version_to client_protocol_version - 1
      end
    end
  end
end

require 'automation'

# Tests for story 0: Connection
class ConnectionTests < Skirmish::SystemTest

  def setup
    assert_nothing_raised "error in setup, might be because of previous test and might affect later tests" do
      start_server
    end
  end

  def teardown
    assert_nothing_raised "error in teardown, might affect later tests" do
      stop_client if client_active?
      stop_server
    end
  end

  def test_happy_path
    assert_nothing_raised do
      start_client "asd", ""
    end
  end

  def test_boundaries
    assert_nothing_raised do
      start_client "1234567890.aAbB.", "A very, very, long, contrived passphrase."
    end
  end

  def test_id_too_long
    assert_raised_message_matches(/id too long/) do
      start_client "a" * 17, ""
    end
  end

  def test_wrong_protocol_version
    assert_raised_message_matches(/different version/) do
      start_client "asd", "" do
        monkey_patch_client_protocol_version_to client_protocol_version - 1
      end
    end
  end

  def test_unreachable_host
    assert_raise HostUnreachable do
      start_client "asd", "", "10.255.249.127"
    end
  end
  def test_wrong_port
    assert_raise HostUnreachable do
      start_client "asd", "", "localhost", 9999
    end
  end
end

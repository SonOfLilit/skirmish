require 'test/unit/testcase'

require 'helpers/connection'


#
# Used to have a test
#
#  def test_server_does_not_respond
#    ex = assert_raise NetworkTimeout do
#      connect :server_response => false
#    end
#    assert_match(/not a skirmish server/i, ex.message)
#  end
#
# but apparently network behaviour in this case is exactly like in
# test_no_server, so it seems impossible.
#

class TestConnectErrors < Test::Unit::TestCase

  include ConnectionTestHelper

  def test_server_unreachable
    assert_raise ServerNotFound do
      connect :host => "10.255.249.127", :fake_server => false
    end
  end
  def test_no_server
    assert_raise ServerNotFound do
      connect :fake_server => false
    end
  end
  def test_different_port
    connect :server_port => 77788, :port => 77788
  end
  def test_wrong_port
    assert_raise ServerNotFound do
      connect :port => 88899
    end
  end

  def test_wrong_protocol_version
    ensure_server_fatal "Wrong protocol version\n"
  end
  def test_wrong_protocol_version_long_message
    ensure_server_fatal random_string(1024)
  end
  # This is not a legal case, so if this fails it is alright, as long
  # as nothing horrible happens. It was easier to write the test this
  # way - over-requiring - than just testing that nothing major breaks
  def test_wrong_protocol_version_too_long_message
    ensure_server_fatal random_string(4096)
  end

  def test_server_full
    ensure_server_fatal "Server full."
  end

  def test_garbage_server_response
    garbage_server_response "garbage"
    garbage_server_response "fatale"
    garbage_server_response "ok\n"
    garbage_server_response "\nok\n\n"
    garbage_server_response "ok\n\n\n"
  end

end

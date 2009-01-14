require 'test/unit'
require 'thread'
require 'fakeserver'
require File.expand_path(File.join(File.dirname(__FILE__), '../src/connection'))

class TestConnect < Test::Unit::TestCase
  include Skirmish

  def random_string n
    Array.new(n) { rand(256) }.pack("C*")
  end

  def try_connect id, secret, &block
    @server = FakeServer.new Connection::DEFAULT_PORT, &block

    Thread.abort_on_exception = true # otherwise t.raise doesn't work TODO ask why
    ct = Thread.new Thread.current do |t|
      begin
        Connection.new 'localhost', Connection::DEFAULT_PORT, id, secret
      rescue Object => e
        t.raise e
      end
    end

    yield

    ct.join # wait for thread to finish

  ensure
    begin
      @server.close
    rescue IOError # socket is closed
    end
  end

  def can_connect_with id, secret
    try_connect id, secret do
      @server.expect "version #{Connection::PROTOCOL_VERSION}\n"
      @server.send "ok\n"
      @server.expect "id #{id}\n"
      @server.expect "secret #{secret}\n"
      @server.send "ok\n"
    end
  end
  def test_simplest_valid_case
    can_connect_with 'abc', ''
  end
  def test_average_valid_case
    can_connect_with 'abcdefg', 'hijklmnop'
  end
  def test_long_id_and_secret
    can_connect_with 'a' * 16, 'a' * 228 # there was a bug at this length exactly
  end
  def test_longest_allowed_id_and_secret
    can_connect_with 'a' * 16, 'a' * 255 # maximum lengths
  end
  def test_most_contrived_valid_case
    # \x00\01\02..\x08\x09\x0B\x0C..\xFF -- all but the newline
    array = ([*(0..(?\n-1))] + [*((?\n+1)..255)])
    secret = array.pack('C*')
    assert_equal 255, secret.length
    can_connect_with 'aA1.aA1.aA1.aA1.', secret
    can_connect_with 'aA1', secret.reverse!
  end

  def local_error_on_connect_with id, secret
    assert_raise ArgumentError do
      Connection.new 'localhost', Connection::DEFAULT_PORT, id, secret
    end
  end
  def test_too_short_id
    local_error_on_connect_with 'aa', 'bcdefg'
  end
  def test_empty_id
    local_error_on_connect_with '', 'bcdefg'
  end
  def test_too_long_id
    local_error_on_connect_with 'a' * 17, 'bcdefg'
  end
  def test_very_long_id
    local_error_on_connect_with 'a' * 1024, 'bcdefg'
  end
  def test_too_long_secret
    local_error_on_connect_with 'aaa', 'a' * 256
  end
  def test_very_long_secret
    local_error_on_connect_with 'aaa', random_string(1024)
  end

  def test_server_unreachable # TODO
  end
  def test_wrong_port # TODO
  end
  def test_server_suddenly_dies # TODO
  end

  def ensure_server_fatal message, &block
    assert_raise ServerFatal do
      begin
        try_connect 'abcd', 'efgh' do
          block.call 'abcd', 'efgh'
        end
      rescue ServerFatal => e
        assert_match message, e.to_s
        raise e
      end
    end
  end
  def wrong_protocol_version message
    ensure_server_fatal message do |id, secret|
      @server.expect "version #{Connection::PROTOCOL_VERSION}\n"
      @server.send "fatal " + message
      @server.close
    end
  end
  def test_wrong_protocol_version
    wrong_protocol_version "Wrong protocol version\n"
  end
  def test_wrong_protocol_version_long_message
    wrong_protocol_version random_string(1024)
  end
  # This is not a legal case, so if this fails it is alright, as long
  # as nothing horrible happens. It was easier to write the test this
  # way - over-requiring - than just testing that nothing major breaks
  def test_wrong_protocol_version_too_long_message
    wrong_protocol_version random_string(4096)
  end
  def server_does_not_like_us message
    ensure_server_fatal message do |id, secret|
      @server.expect "version #{Connection::PROTOCOL_VERSION}\n"
      @server.send "ok\n"
      @server.expect "id #{id}\n"
      @server.expect "secret #{secret}\n"
      @server.send "fatal " + message
      @server.close
    end
  end
  def server_full
    server_does_not_like_us "Server full."
  end
end

require 'test/unit'
require 'thread'
require 'fakeserver'
require File.expand_path(File.join(File.dirname(__FILE__), '../src/connection'))

class TestConnect < Test::Unit::TestCase
  include Skirmish

  def try_connect id, secret, &block
    @server = FakeServer.new Connection::DEFAULT_PORT, &block

    Thread.abort_on_exception = true
    ct = Thread.new Thread.current do |t|
      begin
        Connection.new 'localhost', Connection::DEFAULT_PORT, id, secret
      rescue Object => e
        t.raise e
      end
    end

    yield

    ct.join

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

  def wrong_protocol_version message
    assert_raise ServerFatal do
      try_connect 'abcd', 'efgh' do
        @server.expect "version #{Connection::PROTOCOL_VERSION}\n"
        @server.send "fatal " + message
        @server.close
      end
    end
  end
  def test_wrong_protocol_version
    wrong_protocol_version "Wrong protocol version\n"
  end
  def test_wrong_protocol_version_long_message
    wrong_protocol_version Array.new(1024) { rand(256) }.to_s
  end
end

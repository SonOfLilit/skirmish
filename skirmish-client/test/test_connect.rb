require 'test/unit'
require 'thread'
require 'fake-server'
require File.expand_path(File.join(File.dirname(__FILE__), '../src/connection'))

class TestConnect < Test::Unit::TestCase
  include Skirmish

  def random_string n
    Array.new(n) { rand(256) }.pack("C*")
  end

  def local_error_on_connect_with id, secret
    assert_raise ArgumentError, "Connect did not fail on illegal identifiers " \
    "#{id.inspect}, #{secret.inspect}" do
      Connection.new "localhost", Connection::DEFAULT_PORT, id, secret
    end
  end
  def test_too_short_id
    local_error_on_connect_with "aa", "bcdefg"
  end
  def test_empty_id
    local_error_on_connect_with "", "bcdefg"
  end
  def test_garbage_id
    local_error_on_connect_with "ab,", "cdefg"
    local_error_on_connect_with "!ab", "cdefg"
    local_error_on_connect_with "--asdf--", "cdefg"
    local_error_on_connect_with "\n", "cdefg"
    local_error_on_connect_with "-=[Ment0r]=-,", "cdefg"
  end
  def test_newline_in_secret
    local_error_on_connect_with "abc", "\n"
    local_error_on_connect_with "abc", "\n\n asdf \n\n"
    local_error_on_connect_with '!@#$', "\n"
  end
  def test_too_long_id
    local_error_on_connect_with "a" * 17, "bcdefg"
  end
  def test_very_long_id
    local_error_on_connect_with "a" * 1024, "bcdefg"
  end
  def test_too_long_secret
    local_error_on_connect_with "aaa", "a" * 256
  end
  def test_very_long_secret
    local_error_on_connect_with "aaa", random_string(1024)
  end

  def call_connect options
    Connection.new options[:host], options[:port], options[:id], options[:secret]
  end

  def connect_in_other_thread options
    Thread.abort_on_exception = true # otherwise t.raise doesn't work TODO ask why
    parent = Thread.current
    thread = Thread.new do
      begin
        call_connect options
      rescue Object => e
        parent.raise e
      end
    end
    return thread
  end

  def connect_with_fake_server options
    @server = FakeServer.new options[:server_port]

    connection_thread = connect_in_other_thread options

    @server.expect "version #{Connection::PROTOCOL_VERSION}\n" \
                   "id #{options[:id]}\n" \
                   "secret #{options[:secret]}\n" \
                   "\n"
    @server.send options[:server_response]

    connection_thread.join # wait for thread to finish
  ensure
    @server.close unless @server.nil? or @server.closed?
  end

  def connect options
    default_port = Connection::DEFAULT_PORT
    options[:id] ||= "abc"
    options[:secret] ||= "defg"
    options[:host] ||= "localhost"
    options[:port] ||= default_port
    options[:server_port] ||= default_port
    options[:server_response] ||= "ok\n\n"

    if options[:fake_server] != false
      connect_with_fake_server options
    else
      call_connect options
    end
  end

  def connect_with id, secret, options={}
    options[:id] = id
    options[:secret] = secret
    connect options
  end

  def test_simplest_valid_case
    connect_with "abc", ""
  end
  def test_average_valid_case
    connect_with "abcdefg", "hijklmnop"
  end
  def test_long_id_and_secret
    connect_with "a" * 16, "a" * 228 # there was a bug at this length exactly
  end
  def test_longest_allowed_id_and_secret
    connect_with "a" * 16, "a" * 255 # maximum lengths
  end
  def test_most_contrived_valid_case
    # \x00\01\02..\x08\x09\x0B\x0C..\xFF -- all but the newline
    array = ([*(0..(?\n-1))] + [*((?\n+1)..255)])
    secret = array.pack("C*")
    assert_equal 255, secret.length
    connect_with "aA1.aA1.aA1.aA1.", secret
  end

  def test_server_unreachable
    assert_raise ServerNotFound do
      connect :host => "10.255.249.127", :fake_server => false
    end
  end
  def test_not_server
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

  def ensure_server_fatal message, &block
    assert_raise ServerFatal do
      begin
        connect :server_response => "fatal #{message}"
      rescue ServerFatal => e
        assert_match message, e.to_s
        raise e
      end
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
end

require 'test/unit/assertions'
require 'thread'
here = File.expand_path(File.dirname(__FILE__))
require File.join(here, 'fake-server')
require File.join(here, '../../src/connection')


module ConnectionTestHelper

  include Skirmish

  def random_string n
    Array.new(n) { rand(256) }.pack("C*")
  end

  def local_error_on_connect_with id, secret
    assert_raise ArgumentError,
        "Connect did not fail on illegal identifiers " \
        "#{id.inspect}, #{secret.inspect}" do
      connection_new(apply_defaults :id => id, :secret => secret)
    end
  end

  def connection_new options
    Connection.new options[:host], options[:port], options[:id], options[:secret]
  end

  def do_request_game connection, options
    connection.request_game
  end

  def connect options
    apply_defaults options
    if options[:fake_server]
      connect_with_fake_server options
    else
      connection_new options
    end
  end

  def request_game options={}
    options[:request_game] = true
    unless options.has_key? :request_game_response
      options[:request_game_response] =
        "world-corner 0,0\n" \
        "world-size 3000,3000\n" \
        "\n"
    end
    connect options
  end

  def apply_defaults options
    options[:id] ||= "abc"
    options[:secret] ||= "defg"
    options[:host] ||= "localhost"
    default_port = Connection::DEFAULT_PORT
    options[:port] ||= default_port
    options[:server_port] ||= default_port
    options[:fake_server] = true unless options.has_key? :fake_server
    options[:server_response] = "ok\n\n" unless options.has_key? :server_response
    options[:request_game] ||= false
    options
  end

  def connect_with id, secret, options={}
    options[:id] = id
    options[:secret] = secret
    connect options
  end

  def connect_in_other_thread options
    Thread.abort_on_exception = true # otherwise t.raise doesn't work TODO ask why
    parent = Thread.current
    thread = Thread.new do
      begin
        conn = connection_new options
        do_request_game conn, options if options[:request_game]
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
    @server.send options[:server_response] if options[:server_response]
    if options[:request_game]
      @server.expect "game\n" \
                     "\n"
      @server.send options[:request_game_response]
    end

    connection_thread.join # wait for thread to finish
  ensure
    @server.close unless @server.nil? or @server.closed? or options[:keep_server]
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

  def garbage_server_response response
    assert_raise NetworkProtocolError do
      connect :server_response => response
    end
  end

end

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
      connection_new(apply_defaults(:id => id, :secret => secret))
    end
  end

  def connection_new options
    Connection.new options[:host], options[:port], options[:id], options[:secret],
                   options[:timeout]
  end

  def do_request_game connection, options
    game = connection.request_game
    return game
  end

  def connect options
    apply_defaults options
    if options[:fake_server]
      connect_with_fake_server options
    else
      connection_new options
    end
  end

  def apply_defaults options
    options[:id] ||= "abc"
    options[:secret] ||= "defg"
    options[:host] ||= "localhost"
    default_port = Connection::DEFAULT_PORT
    options[:port] ||= default_port
    options[:timeout] ||= 2
    options[:server_port] ||= default_port
    options[:fake_server] = true unless options.has_key? :fake_server
    options[:server_timeout] ||= 3
    options[:ignore_server_errors] ||= false
    options[:server_response] = "ok\n\n" unless options.has_key? :server_response
    options[:request_game] ||= false
    options
  end

  def connect_with id, secret, options={}
    options[:id] = id
    options[:secret] = secret
    connect options
  end

  def start_server_thread options
    @server = FakeServer.new options[:server_port], options[:server_timeout]

    parent = Thread.current
    Thread.abort_on_exception = true # otherwise t.raise doesn't work TODO ask why
    server_thread = Thread.new do
      begin
        @server.expect "version #{Connection::PROTOCOL_VERSION}\n" \
                       "id #{options[:id]}\n" \
                       "secret #{options[:secret]}\n" \
                       "\n"
        @server.send options[:server_response] if options[:server_response]
        if options[:request_game]
          @server.expect "game\n\n"
          @server.send options[:request_game_response]
          @server.expect "ok\n\n"
        end
      rescue Exception => e
        parent.raise e unless options[:ignore_server_errors]
      end
    end
    return server_thread
  end

  def connect_with_fake_server options
    server_thread = start_server_thread options
    conn = connection_new options
    ret = conn
    if options[:request_game]
      ret = do_request_game conn, options
    end
    server_thread.join
    return ret
  ensure
    unless options[:keep_server]
      @server.close unless @server.nil? or @server.closed?
    end
  end

  def ensure_server_fatal message, options={}, &block
    assert_raise ServerFatal do
      begin
        key = if options[:request_game]
                :request_game_response
              else
                :server_response
              end
          options[key] = "fatal #{message}"
        connect options
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

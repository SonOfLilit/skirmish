require 'test/unit/assertions'
require 'thread'
require 'fake-server'
here = File.dirname(__FILE__)
require File.expand_path(File.join(here, '../../src/connection'))


module ConnectionTestHelper

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

  def connection_new options
    Connection.new options[:host], options[:port], options[:id], options[:secret]
  end

  def connect options
    default_port = Connection::DEFAULT_PORT
    options[:id] ||= "abc"
    options[:secret] ||= "defg"
    options[:host] ||= "localhost"
    options[:port] ||= default_port
    options[:server_port] ||= default_port
    options[:fake_server] = true unless options.has_key? :fake_server
    options[:server_response] = "ok\n\n" unless options.has_key? :server_response

    if options[:fake_server]
      connect_with_fake_server options
    else
      connection_new options
    end
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
        connection_new options
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

    connection_thread.join # wait for thread to finish
  ensure
    @server.close unless @server.nil? or @server.closed?
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

require 'socket'
require 'timeout'

require 'game-world'


module Skirmish

  # Thrown when server takes too long to respond
  class NetworkTimeout < ScriptError; end # TODO: #to_s
  # Thrown on an error parsing server response
  class NetworkProtocolError < ScriptError; end # TODO: #to_s
  # Thrown when the given host/port cannot be reached
  class ServerNotFound < ScriptError; end # TODO: #to_s
  # Thrown when the server reports a fatal error
  class ServerFatal < ScriptError
    def initialize message
      @message = message
    end

    def to_s
      "Server fatal error: #{@message}"
    end
  end

  #
  # Manages a connection to the server
  #
  # === Example
  #
  #     require 'connection.rb'
  #
  #     conn = Skirmish::Connection.new(server_host,
  #                                     Skirmish::Connection::DEFAULT_PORT,
  #                                     "my.id", "my secret code!")
  #     game = conn.request_game()
  #
  class Connection

    PROTOCOL_VERSION = 0 # TODO: DRY here and in server
    # in honor of my friends, FIRST Robotics Competition team #1657 HAMOSSAD
    DEFAULT_PORT = 1657

    def initialize(host, port, id, secret, timeout=5)
      @timeout = timeout
      @buffer = ''

      validate_id id
      validate_secret secret

      begin
        bind(host, port)
        send_header(id, secret)
        read_ok_or_fatal()
      rescue NetworkTimeout, Errno::ECONNREFUSED, Errno::EHOSTUNREACH => ex
        raise ServerNotFound, ex
      end
    end

    REQUEST_RESPONSE_RE = /^world-corner (\d+),(\d+)\nworld-size (\d+),(\d+)\n\n$/
    MAX_LONG = 2 ** 32
    def request_game()
      send("game\n\n")
      message = read_message_blocking()
      match = message.match(REQUEST_RESPONSE_RE)
      raise NetworkProtocolError unless match and match.captures.length == 4
      captures = match.captures.map do|s|
        i = s.to_i
        raise NetworkProtocolError unless i < MAX_LONG
        i
      end
      corner = captures[0..1]
      size = captures[2..3]
      world_rect = corner + (0..1).map{|i| corner[i] + size[i] - 1}
      return GameWorld.new(world_rect)
    end

    def validate_id(id)
      raise ArgumentError, "id too short" unless id.length >= 3
      raise ArgumentError, "id too long" unless id.length <= 16
      unless id =~ /^[0-9a-zA-Z.]*$/
        raise ArgumentError, "id may only contain letters, numbers and dots"
      end
    end

    def validate_secret(secret)
      raise ArgumentError, "Secret too long - keep at 255 characters" if secret.length > 255
      raise ArgumentError, "Secret may not contain newlines" if secret.index("\n")
    end

    # currently does almost nothing, all the work is done in send_header
    def bind(host, port)
      @socket = UDPSocket.new()
      @socket.bind("", 0)
      @host = host
      @port = port
    end

    def send_header(id, secret)
      send("version #{PROTOCOL_VERSION}\nid #{id}\nsecret #{secret}\n\n")
    end

    def send(data)
      @socket.send(data, 0, @host, @port)
    end

    def read_message_blocking()
      begin
        Timeout::timeout(@timeout) do
          pair = nil
          begin
            # TODO: fix BUG: the kind of socket used here throws away
            # anything not received on the first recvfrom_nonblock
            # call. For now I've given it a very large buffer, but
            # eventually it should be googled, perhaps usenet'd, and a
            # way to recv without throwing out the rest should be
            # figured out
            pair = @socket.recvfrom_nonblock(32767)
          rescue Errno::EAGAIN
            IO.select([@socket])
            retry
          end
          return pair[0]
        end
      rescue Timeout::Error
        raise NetworkTimeout
      end
    end

    def read_ok_or_fatal()
      message = read_message_blocking
      return if message.eql? "ok\n\n"

      @socket.close
      if message[0, 6].eql? "fatal "
        raise ServerFatal, message[6..-1]
      else
        raise NetworkProtocolError, "unexpected server response" + message.inspect
      end
    end

  end

end

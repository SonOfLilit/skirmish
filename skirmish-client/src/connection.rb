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

    #
    # Connects to a Skirmish Server at +host+:+port, identifies with
    # +id+, +secret+ and returns a Connection object.
    #
    # May throw ServerNotFound, NetwprkProtocolError, ServerFatal.
    def initialize(host, port, id, secret, timeout=5)
      @timeout = timeout
      @buffer = ''

      validate_id(id)
      validate_secret(secret)

      begin
        bind(host, port)
        send("version #{PROTOCOL_VERSION}\nid #{id}\nsecret #{secret}\n\n")
        read_and_parse_message(:ok)
      rescue NetworkTimeout, Errno::ECONNREFUSED, Errno::EHOSTUNREACH => ex
        raise ServerNotFound, ex
      end
    end

    #
    # Request a new game from the server. Returns a GameWorld object.
    #
    # Allowed only immediately after connecting.
    #
    def request_game()
      send("game\n\n")
      x, y, w, h = read_and_parse_message(:request_game_response)
      ul_corner = [x, y]
      size = [w, h]
      lr_corner = (0..1).map{|i| ul_corner[i] + size[i] - 1}
      world_rect = ul_corner + lr_corner
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

    def bind(host, port)
      @socket = UDPSocket.new()
      @socket.bind("", 0)
      @host = host
      @port = port
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


    MESSAGES = {
      :ok => [/^ok\n\n$/, 0],
      :request_game_response =>
      [/^world-corner (\d+),(\d+)\nworld-size (\d+),(\d+)\n\n$/, 4]
    }

    #
    # Reads one message from the server and tries to parse it as a
    # message of the given kind.
    #
    # +MESSAGES+ is a dictionary of kinds of messages. Its values are
    # arrays with cell 0 containing a regular expression matched
    # against the message and cell 1 the number of LONG parameters to
    # extract from it.
    #
    #--
    # TODO find a better way to match against the whole string and
    # not just one line
    #++
    MAX_LONG = 2 ** 32
    def read_and_parse_message(type)
      message = read_message_blocking()
      match = MESSAGES[type][0].match(message)
      if match and
          match.pre_match.length == 0 and
          match.post_match.length == 0 and
          match.captures and
          match.captures.length == MESSAGES[type][1]
        return match.captures.map do |s|
          i = s.to_i
          raise NetworkProtocolError unless i < MAX_LONG
          i
        end
      else
        @socket.close
        if /^fatal /.match(message)
          raise ServerFatal, message[6..-1]
        else
          msg = "Unexpected server response" + message.inspect
          raise NetworkProtocolError, msg
        end
      end
    end

    def recv_ok()
      read_and_parse_message(:ok)
    end

    def recv_request_game_response()
      read_and_parse_message(:request_game_response)
    end

  end

end

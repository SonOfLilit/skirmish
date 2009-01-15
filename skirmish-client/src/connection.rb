require 'socket'
require 'timeout'

module Skirmish

  class NetworkTimeout < Exception; end # TODO: #to_s
  class NetworkProtocolError < Exception; end # TODO: #to_s
  class ServerNotFound < Exception; end # TODO: #to_s
  class ServerFatal < Exception; end # TODO: #to_s

  class Connection

    PROTOCOL_VERSION = 0 # TODO: DRY here and in server
    # in honor of my friends, FIRST Robotics Competition team #1657 HAMOSSAD
    DEFAULT_PORT = 1657

    TIMEOUT=5

    def initialize host, port, id, secret
      @buffer = ''

      validate_id id
      validate_secret secret

      begin
        connect host, port
        send "version #{PROTOCOL_VERSION}\nid #{id}\nsecret #{secret}\n\n"
        read_ok_or_fatal
      rescue NetworkTimeout, Errno::ECONNREFUSED, Errno::EHOSTUNREACH
        raise ServerNotFound
      end
    end

    def validate_id id
      raise ArgumentError, "id too short" unless id.length >= 3
      raise ArgumentError, "id too long" unless id.length <= 16
      unless id =~ /^[0-9a-zA-Z.]*$/
        raise ArgumentError, "id may only contain letters, numbers and dots"
      end
    end

    def validate_secret secret
      raise ArgumentError, "Secret too long - keep at 255 characters" if secret.length > 255
      raise ArgumentError, "Secret may not contain newlines" if secret.index("\n")
    end

    def connect host, port
      @socket = UDPSocket.new
      @socket.connect host, port
    end

    def send string
      @socket.write string
    end

    def read_message_blocking timeout=TIMEOUT
      begin
        Timeout::timeout(timeout) do
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

    def read_ok_or_fatal
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

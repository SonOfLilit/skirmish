require 'socket'
require 'timeout'

module Skirmish

  class NetworkProtocolError < Exception; end # TODO: #to_s
  class ServerFatal < Exception; end # TODO: #to_s

  class Connection

    PROTOCOL_VERSION = 0 # TODO: DRY here and in server
    # in honor of my friends, FIRST Robotics Competition team #1657 HAMOSSAD
    DEFAULT_PORT = 1657

    TIMEOUT=20

    def initialize host, port, id, secret
      @buffer = ''

      validate_id id
      validate_secret secret

      connect host, port
      send "version #{PROTOCOL_VERSION}\n"
      wait_for_ok_or_fatal
      send "id #{id}\nsecret #{secret}\n"
      wait_for_ok_or_fatal
    end

    def validate_id id
      raise ArgumentError, "id too short" unless id.length >= 3
      raise ArgumentError, "id too long" unless id.length <= 16
      unless id =~ /^[0-9a-zA-Z.]*$/
        raise ArgumentError, "id may only contain letters, numbers and dots"
      end
    end

    def validate_secret secret
      unless secret.length <= 255 and secret["\n"].nil?
        raise ArgumentError, "Invalid secret"
      end
    end

    def connect host, port
      @socket = UDPSocket.new
      @socket.connect host, port
    end

    def send string
      @socket.write string
    end

    def read seconds
      Timeout::timeout(seconds) do
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
        @buffer << pair[0]
      end
    end

    def wait_for_ok_or_fatal
      if @buffer.length < 3
          read TIMEOUT
      end
      if @buffer[0, 3].eql? "ok\n"
        @buffer[0, 3] = ''
      elsif @buffer[0, 5].eql? "fatal"
        @socket.close
        raise ServerFatal, @buffer[6..-1]
      else
        @socket.close
        raise NetworkProtocolError
      end
    end
  end

end

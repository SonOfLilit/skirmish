require 'socket'
require 'timeout'

module Skirmish

  class NetworkProtocolError < Exception; end

  class Connection

    # TODO: DRY here and in server
    PROTOCOL_VERSION = 0
    # in honor of my friends, FIRST Robotics Competition team #1657 HAMOSSAD
    DEFAULT_PORT = 1657

    def initialize host, port, id, secret
      @buffer = ''

      connect host, port
      send "version #{PROTOCOL_VERSION}\n"
      wait_for_ok
      send "id #{id}\nsecret #{secret}\n"
      wait_for_ok
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
          # TODO: fix BUG

          # the kind of socket used here throws away anything not
          # received on the first recvfrom_nonblock call. For now I've
          # given it a very large buffer, but eventually it should be
          # googled, perhaps usenet'd, and a way to recv without
          # throwing out the rest should be figured out
          pair = @socket.recvfrom_nonblock(32767)
        rescue Errno::EAGAIN
          IO.select([@socket])
          retry
        end
        @buffer << pair[0]
      end
    end

    def wait_for_ok
      Timeout::timeout(5) do
        while @buffer.length < 3
          read 5
        end
      end
      if @buffer[0, 3].eql? "ok\n"
        @buffer[0, 3] = ''
      else
        raise NetworkProtocolError
      end
    end
  end

end

require 'socket'
require 'timeout'
require 'test/unit/assertions'

class FakeServer
  include Test::Unit::Assertions

  DEFAULT_TIMEOUT = 5
  def initialize port, waittime=DEFAULT_TIMEOUT
    @timeout = waittime
    @buffer = ''
    @socket = nil
    @addr = nil
    @sockaddr = nil
    @listener = UDPSocket.new
    @listener.bind("", port)
  end

  def read
    socket = @socket || @listener

    pair = nil

    begin
      pair = socket.recvfrom_nonblock(32767)
    rescue Errno::EAGAIN
      IO.select([socket])
      retry
    end

    # if this is first read, store remote address and start dedicated
    # socket
    unless @socket
      @addr = pair[1]
      af, port, host, addr = @addr
      @sockaddr = Socket.sockaddr_in(port, addr)

      @socket = UDPSocket.new
      @socket.bind("", 0)
    end

    assert_equal @addr, pair[1], "only one client supported by fake-server"

    @buffer << pair[0]
  end

  def expect string
    len = string.length
    Timeout::timeout(@timeout) do
      begin
        while @buffer.length < len
          read
        end
      rescue Timeout::Error
        flunk "Timeout awaiting message #{string.inspect} (buffer: '#{@buffer}')"
      end
    end

    assert_equal string, @buffer[0, len], "Client sent unexpected message"
    @buffer[0, len] = ''
  end

  def send string
    @socket.send string, 0, @sockaddr
  end

  def close
    @listener.close
    @socket.close if @socket
  end

  def closed?
    @listener.closed?
  end

end

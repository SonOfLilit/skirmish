require 'socket'
require 'timeout'
require 'test/unit/assertions'

class FakeServer
  include Test::Unit::Assertions

  DEFAULT_TIMEOUT = 5
  def initialize port, waittime=DEFAULT_TIMEOUT
    @timeout = waittime
    @socket = nil
    @addr = nil
    @sockaddr = nil
    @listener = UDPSocket.new
    @listener.bind("", port)
  end

  def init_connection addr_a
    @addr = addr_a
    af, port, host, addr = @addr
    @sockaddr = Socket.sockaddr_in(port, addr)
    @socket = UDPSocket.new
    @socket.bind("", 0)
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
    init_connection pair[1] unless @socket
    assert_equal @addr, pair[1], "only one client supported by fake-server"
    pair[0]
  end

  def expect string
    Timeout::timeout(@timeout) do
      begin
        r = read()
        assert_equal string, r, "Client sent unexpected message"
      rescue Timeout::Error
        flunk "Timeout awaiting message #{string.inspect}"
      end
    end
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

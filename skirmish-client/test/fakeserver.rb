require 'socket'
require 'timeout'
require 'test/unit/assertions'

class FakeServer
  include Test::Unit::Assertions

  DEFAULT_TIMEOUT = 5
  def initialize port, waittime=DEFAULT_TIMEOUT, &block
    @timeout = waittime
    @buffer = ''
    @addr = nil
    @sockaddr = nil
    @listener = UDPSocket.new
    @listener.bind('localhost', port)
  end

  def read
    pair = nil

    begin
      pair = @listener.recvfrom_nonblock(32767)
    rescue Errno::EAGAIN
      IO.select([@listener])
      retry
    end

    # if this is first read, store remote address
    unless @addr
      @addr = pair[1]
      af, port, host, addr = @addr
      @sockaddr = Socket.sockaddr_in(port, addr)
    end

    assert_equal @addr, pair[1], "only one client supported"

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
        flunk "Timeout waiting for message '#{string}' (buffer: '#{@buffer}')"
      end
    end

    assert_equal string, @buffer[0, len], "Client sent unexpected message"
    @buffer[0, len] = ''
  end

  def send string
    @listener.send string, 0, @sockaddr
  end

  def close
    @listener.close
  end

end

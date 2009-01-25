require 'clirunner'
require 'log'
require 'helper'

module Skirmish; end

module Skirmish::Automation

  #
  # Methods to drive a Skirmish Client.
  #
  module Client

    # Skirmish server default port
    SERVER_PORT = 1657

    # Thrown when client reports fatal error from server
    class ServerFatal < ScriptError; end
    # Thrown when client reports unreachable server
    class HostUnreachable < ScriptError; end
    # Thrown when client reports network error
    class NetworkError < ScriptError; end

    #
    # Skirmish Client base directory
    #---
    # This is a method because BASE_DIR is not in
    # Skirmish::Automation::Client so it cannot be evaluated at
    # class-evaluation-time
    #+++
    #
    def client_dir
      File.join(BASE_DIR, 'skirmish-client')
    end

    #
    # Start a skirmish client
    #
    # Yields to optional +block+ between starting the client and
    # connecting to the server.
    #
    # Do not use if you require more than one client alive
    #
    def start_client(id="test.id", secret="dirty secret",
                     host="localhost", port=SERVER_PORT,
                     &block)
      client_cmd = File.join(client_dir, 'bin/skirmish-client') + ' --console'
      @automation_client = CliRunner.open 'skirmish-client', client_cmd

      yield if block

      begin
        @automation_client << "connect #{host.inspect}, #{port.inspect}, " \
        "#{id.inspect}, #{secret.inspect}" >> /connected/i
      rescue MatchError => m
        case m.message
        when /server error/i: raise ServerFatal
        when /servernotfound/i: raise HostUnreachable
        when /network error/i: raise NetworkError
        end
        flunk m
      end

      log.info("Automation#start_client") { "client started" }
    end

    #
    # Stop a skirmish client started with #start_client
    #
    def stop_client
      if @automation_client
        # in case of error, does not affect next tests
        client = @automation_client
        @automation_client = nil

        client.close "exit"
      else
        trace = Helper.get_backtrace
        log.warn("Automation#stop_client") { "Client was not started\n" \
          "in #{trace[0, 5].join("\n")}" }
      end
      log.info("Automation#stop_client") { "client stopped" }
    end

    #
    # True if a client was started with +#start_client+
    #
    def client_active?
      @automation_client
    end


    VERSION_CONSTANT_NAME = "Skirmish::Connection::PROTOCOL_VERSION" # :nodoc:
    #
    # Query the client console for the network protocol version
    #
    def client_protocol_version
      match = @automation_client << VERSION_CONSTANT_NAME >> /\d*/
      version =  match[0].to_i
      return version
    end

    #
    # Make the client claim to the server to be using a fake protocol
    # version +version+
    #
    def monkey_patch_client_protocol_version_to version
      @automation_client << "#{VERSION_CONSTANT_NAME} = #{version}" >> /warning/
    end

    #
    # Request a new game from the server
    #
    def start_game
      m = nil
      assert_nothing_raised do
        m = @automation_client << "new_game" >>
          /world from \((\d+),(\d+)\) to \((\d+),(\d+)\)/
      end
      @automation_client_world_rect = m.captures.map {|s| s.to_i }
    end

    #
    # Returns [top, left, right, bottom] cooerdinates of the game world
    #
    def client_world_rectangle
      @automation_client_world_rect
    end

    def units
      m = @automation_client << "game.units" >> /\[.*\]/
      return eval(m.to_s)
    end

  end

end

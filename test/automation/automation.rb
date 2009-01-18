$LOAD_PATH.unshift(File.dirname(__FILE__))
require 'clirunner'
require 'systemtest'

module Skirmish

  # Automation of Skirmish client and server
  #
  # === Example
  #
  #     require 'automation.rb'
  #
  #     class SimpleSkirmishBot
  #       include Skirmish::Automation
  #       def initialize
  #         start_server
  #       end
  #     end
  #
  # Usually used through +SystemTest+ that includes it.
  module Automation

    # Base skirmish project directory
    BASE_DIR = '.'
    # Skirmish server default port
    SERVER_PORT = 1657
    SERVER_DIR = File.join(BASE_DIR, 'skirmish_server') # :nodoc:
    CLIENT_DIR = File.join(BASE_DIR, 'skirmish-client') # :nodoc:

    # Thrown when client reports fatal error from server
    class ServerFatal < Exception; end
    # Thrown when client reports unreachable server
    class HostUnreachable < Exception; end
    # Thrown when client reports network error
    class NetworkError < Exception; end

    # Start a skirmish server
    #
    # Do not use if you require more than one server alive
    def start_server
      erl = CliRunner.open 'skirmish_server', 'erl', /\d>/, /Eshell/
      erl << "code:add_path(\"#{SERVER_DIR}/ebin\")." >> /true/
      erl << "application:start(skirmish_server)." >> /ok/
      @automation_server = erl
      log.info("Automation#start_server") { "server started" }
    end

    # stop a skirmish server started with #start_server
    def stop_server
      if @automation_server
        @automation_server.close "q()."
        @automation_server = nil
      else
        trace = get_backtrace
        log.warn("Automation#stop_client") { "Client was not started\n" \
          "in #{trace[0, 5].join("\n")}" }
      end
      log.info("Automation#stop_server") { "server stopped" }
    end

    # Start a skirmish client
    #
    # Do not use if you require more than one client alive
    def start_client(id="test_id", secret="dirty secret",
                     host="localhost", port=SERVER_PORT,
                     &block)
      client_cmd = File.join(CLIENT_DIR, 'bin/skirmish-client') + ' --console'
      @automation_client = CliRunner.open 'skirmish-client', client_cmd

      yield if block

      begin
        @automation_client << "connect #{host.inspect}, #{port.inspect}, " \
        "#{id.inspect}, #{secret.inspect}\n" >> /connected/i
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
    # True if a client was started with +#start_client+
    #
    def client_active?
      @automation_client
    end

    # Stop a skirmish client started with #start_client
    def stop_client
      if @automation_client
        @automation_client.close "exit"
        @automation_client = nil
      else
        trace = get_backtrace
        log.warn("Automation#stop_client") { "Client was not started\n" \
          "in #{trace[0, 5].join("\n")}" }
      end
      log.info("Automation#stop_client") { "client stopped" }
    end

    VERSION_CONSTANT_NAME = "Skirmish::Connection::PROTOCOL_VERSION" # :nodoc:
    #
    # Queries the client console for the network protocol version
    #
    def client_protocol_version
      match = @automation_client << VERSION_CONSTANT_NAME >> /\d*/
      version =  match[0].to_i
      return version
    end
    #
    # Makes the client claim to be using a fake protocol version
    # +version+ to the server
    #
    def monkey_patch_client_protocol_version_to version
      @automation_client << "#{VERSION_CONSTANT_NAME} = #{version}" >> /warning/
    end

    # Internal methods

    # TODO: google idiom for getting backtrace
    # this is so ugly
    def get_backtrace # :nodoc:
      trace = nil
      begin raise Exception
      rescue Exception => ex
        trace = ex.backtrace
        trace.pop
      end
      trace
    end

  end

end

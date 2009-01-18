require 'clirunner'
require 'log'
require 'helper'

module Skirmish; end

module Skirmish::Automation

  module Server

    #
    # Skirmish Server base directory
    #--
    # This is a method because BASE_DIR is not in
    # Skirmish::Automation::Server so it cannot be evaluated at
    # class-evaluation-time
    #++
    #
    def server_dir
      File.join(BASE_DIR, 'skirmish_server') # :nodoc:
    end

    # Start a skirmish server
    #
    # Do not use if you require more than one server alive
    def start_server
      erl = CliRunner.open 'skirmish_server', 'erl', /\d>/, /Eshell/
      erl << "code:add_path(\"#{server_dir}/ebin\")." >> /true/
        erl << "application:start(skirmish_server)." >> /ok/
      @automation_server = erl
      log.info("Automation#start_server") { "server started" }
    end

    # Stop a skirmish server started with #start_server
    def stop_server
      if @automation_server
        # in case of error, does not affect next tests
        server = @automation_server
        @automation_server = nil

        server.close "q()."
      else
        trace = Helper.get_backtrace
        log.warn("Automation#stop_client") { "Client was not started\n" \
          "in #{trace[0, 5].join("\n")}" }
      end
      log.info("Automation#stop_server") { "server stopped" }
    end

  end

end
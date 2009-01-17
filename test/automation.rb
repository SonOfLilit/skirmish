require 'test/unit'
require 'logger'
require 'timeout'

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
  # Usually used through SystemTest that includes it.
  module Automation

    BASE_DIR = '.'
    SERVER_PORT = 1657
    SERVER_DIR = File.join(BASE_DIR, 'skirmish_server')
    CLIENT_DIR = File.join(BASE_DIR, 'skirmish-client')

    # Thrown when client reports fatal error from server
    class ServerFatal < Exception; end
    # Thrown when client reports unreachable server
    class HostUnreachable < Exception; end
    # Thrown when client reports network error
    class NetworkError < Exception; end

    # start a skirmish server
    #
    # do not use if you require more than one server alive
    def start_server
      erl = open_cli 'skirmish_server', 'erl', /\d>/, /Eshell/
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

    # start a skirmish client
    #
    # do not use if you require more than one client alive
    def start_client id, secret, host="localhost", port=SERVER_PORT, &block
      client_cmd = File.join(CLIENT_DIR, 'bin/skirmish-client') + ' --console'
      @automation_client = open_cli 'skirmish-client', client_cmd

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

    def client_active?
      @automation_client
    end

    # stop a skirmish client started with #start_client
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

    VERSION_CONSTANT_NAME = "Skirmish::Connection::PROTOCOL_VERSION"
    def client_protocol_version
      match = @automation_client << VERSION_CONSTANT_NAME >> /\d*/
      version =  match[0].to_i
      return version
    end
    def monkey_patch_client_protocol_version_to version
      @automation_client << "#{VERSION_CONSTANT_NAME} = #{version}" >> /warning/
    end

    # internal methods

    def get_backtrace # TODO: google idiom for getting backtrace
      trace = nil
      begin raise Exception
      rescue Exception => ex
        trace = ex.backtrace
        trace.pop
      end
      trace
    end

    def open_cli name, command, prompt = />/, expect_pattern = //
      pipe = IO.popen(command + " 2>&1", "w+")
      pipe.extend(Cli)
      pipe.cli_options :name => name, :prompt => prompt, :default_timeout => 30
      pipe >> expect_pattern
      return pipe
    end

    class MatchError < Exception; end
    class TimeOutError < Exception; end
    module Cli

      include Test::Unit::Assertions

      def << command, pattern=//
        log.info(cli_option(:name)) { "<< #{command}" }
        self.write(command + "\n")
        @cli_last_command = command
        self
      end
      def >> pattern=//, timeout = cli_option(:default_timeout)
        match = nil
        resp = nil
        begin Timeout::timeout(timeout) do
            resp = self.expect_prompt timeout
            raise MatchError unless match = resp =~ pattern
          end
        rescue MatchError, Timeout::Error, EOFError => e
          name = cli_option(:name)
          command = @cli_last_command || "initialization"
          raise e,
          "in #{name}, after '#{command}':\n" \
          "#{e.class}: #{e.to_s}\n" \
          "#{pattern} not in output:\n#{resp}"
        end
        match
      end
      # for internal use only - does not perform error handling
      def expect_prompt timeout
        resp = ""
        until resp =~ cli_option(:prompt)
          resp += self.read_once
        end
        resp
      end

      def read_once
        begin
          r = self.read_nonblock(256)
          r.split.each { |l| log.info(cli_option(:name)) { ">> #{l}" } }
        rescue Errno::EAGAIN
          IO.select([self])
          retry
        end
        r
      end

      def close command, timeout=cli_option(:default_timeout)
        assert_raise EOFError, "#{cli_option(:name)} failed to close properly" do
          self << command
          Timeout::timeout(timeout) do
            while true
              self.read_once
            end
          end
        end
      end

      def cli_options(new_options = {})
        @cli_options ||= {}
        return @cli_options.merge!(new_options)
      end
      def cli_option(key)
        return @cli_options[key]
      end
    end

  end


  # Skirmish tests above unit-level should derive from this class
  #
  # The power of Test::Unit::TestCase and Skirmish::Automation combined!
  #
  # TODO: prevent Test::Unit invocation whenever Automation is used
  class SystemTest < Test::Unit::TestCase
    include Skirmish::Automation

    # monkey patch to prevent test/unit from thinking this is a test suite
    def self.suite # :nodoc:
      undef_method :default_test if self.name == 'Skirmish::SystemTest'
      super
    end

    def assert_raised_message_matches pattern, message="", &block
      full_message = build_message(message,
                                   "exception expected but none was thrown.")
      ex = nil
      assert_block(full_message) do
        begin
          yield
        rescue Exception => ex
          break
        end
        false
      end
      full_message = build_message(message,
                                   "exception <?> message did not match pattern <?>\n",
                                   ex, pattern)
      assert_block(full_message) do
        ex.message =~ pattern
      end
      ex
    end

  end

end


module Kernel
    def log
      unless $automation_logger
        $automation_logger = Logger.new(STDOUT)
        $automation_logger.level = Logger::INFO
      end
      $automation_logger
    end
end

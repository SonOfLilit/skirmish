require 'test/unit'

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
    SERVER_DIR = File.join(BASE_DIR, 'server')
    CLIENT_DIR = File.join(BASE_DIR, 'client')

    # Thrown when client reports fatal error from server
    class ServerFatal < Exception; end

    # start a skirmish server
    #
    # do not use if you require more than one server alive
    def start_server
      @automation_server = 'Server'
      puts 'Started server'
    end

    # stop a skirmish server started with #start_server
    def stop_server
      if @automation_server
        @automation_server = nil
        puts 'Stopped server'
      else
        puts 'Server was not started'
      end
    end

    # start a skirmish client
    #
    # do not use if you require more than one client alive
    def start_client id, secret, &block
      client_cmd = File.join(CLIENT_DIR, 'bin/skirmish-client') + ' --console'
      @automation_client = IO.popen(client_cmd, 'w+')
      client = @automation_client

      prompt = client.read(1)
      assert_equal '>', prompt

      yield if block

      client.write("connect 'localhost', #{SERVER_PORT}, #{id.inspect}, #{secret.inspect}\n");
      response = client.readline
      raise ServerFatal if response =~ /server fatal/i

      assert_match(/connected/i, response, "failed to start client")
    end

    # stop a skirmish client started with #start_client
    def stop_client
      if @automation_client
        @automation_client = nil
      else
        # TODO: google idiom for this
        trace = nil
        begin
          raise Exception
        rescue Exception => ex
          trace = ex.backtrace
        end
        puts "Warning: Client was not started"
        puts trace.join('\n')
      end
    end

    def client_protocol_version
      return 9
    end
    def monkey_patch_client_protocol_version_to version
      puts 'Monkey patched client protocol version'
    end

  end

  # Skirmish tests above unit-level should derive from this class
  #
  # The power of Test::Unit::TestCase and Skirmish::Automation combined!
  #
  # TODO: optionally evaluate this class to enable using
  # Skirmish::Automation for other things
  class SystemTest < Test::Unit::TestCase
    include Skirmish::Automation

    # monkey patch to prevent test/unit from thinking this is a test suite
    def self.suite # :nodoc:
      undef_method :default_test if self.name == 'Skirmish::SystemTest'
      super
    end
  end

end

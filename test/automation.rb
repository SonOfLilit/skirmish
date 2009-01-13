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

    # Thrown when client reports fatal error from server
    class ServerFatal < Exception; end

    # start a skirmish server
    #
    # do not use if you require more than one server alive
    def start_server
      @auto_server = 'Server'
      puts 'Started server'
    end

    # stop a skirmish server started with #start_server
    def stop_server
      if @auto_server
        @auto_server = nil
        puts 'Stopped server'
      else
        puts 'Server was not started'
      end
    end

    # start a skirmish client
    #
    # do not use if you require more than one client alive
    def start_client id, secret, &block
      @auto_client = nil
      yield if block
      assert @auto_client, "failed to start client"
      puts 'Started client'
    end

    # stop a skirmish client started with #start_client
    def stop_client
      if @auto_client
        @auto_client = nil
        puts 'Stopped client'
      else
        puts 'Client was not started'
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
  class SystemTest < Test::Unit::TestCase
    include Skirmish::Automation

    # monkey patch to prevent test/unit from thinking this is a test suite
    def self.suite # :nodoc:
      undef_method :default_test if self.name == 'Skirmish::SystemTest'
      super
    end
  end

end

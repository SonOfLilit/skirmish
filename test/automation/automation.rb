$LOAD_PATH.unshift(File.dirname(__FILE__))

require 'server'
require 'client'

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

    include Server
    include Client

  end

end

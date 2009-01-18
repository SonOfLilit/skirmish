#!/usr/bin/env ruby
$LOAD_PATH.unshift(File.expand_path(File.join(File.dirname(__FILE__), '../src')))
require 'connection'

module Skirmish

  #
  # Placeholder for documentation purposes.
  #
  class Main
  end

  class ConsoleCommands
    def connect(host, port, id, secret)
      @connection = Connection.new(host, port, id, secret)
      "connected"
    end
  end

  module ConsoleOut
    def < (message)
      self.write(message + "\n> ")
      self.flush()
    end
  end

  if ARGV[0] == '--console'
    console = ConsoleCommands.new()
    out = STDOUT
    out.extend(ConsoleOut)
    out < 'Skirmish Client'
    while true
      line = STDIN.gets()
      if line
        begin
          out < console.instance_eval(line).inspect
        rescue SystemExit
          exit
        rescue Exception => e
          out < "#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}"
        end
      else
        exit
      end
    end
  end

end

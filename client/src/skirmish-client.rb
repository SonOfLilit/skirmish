#!/usr/bin/env ruby
$LOAD_PATH.unshift(File.expand_path(File.join(File.dirname(__FILE__), '../src')))
require 'connection'

module Skirmish

  class ConsoleCommands
    def connect host, port, id, secret
      @connection = Connection.new host, port, id, secret
    end
  end

  if ARGV[0] == '--console'
    console = ConsoleCommands.new
    while true
      print '>'
      STDOUT.flush
      line = STDIN.gets
      if line
        puts console.instance_eval(line)
      else
        exit
      end
    end
  end

end

#!/usr/bin/env ruby
$LOAD_PATH.unshift(File.expand_path(File.join(File.dirname(__FILE__), '../src')))
require 'connection'

module Skirmish

  #
  # Commands available when invoking Skirmish Client with the
  # '--console' argument
  #
  # In addition to these, of course, the entire Ruby programming
  # language is at your fingertips, as the console is a Ruby
  # read-eval-print loop.
  #
  class ConsoleCommands

    #
    # Connect to a server at +host+:+port+ with identity +id+,+secret+
    #
    def connect(host, port, id, secret)
      @connection = Connection.new(host, port, id, secret)
      "connected"
    end

    #
    # Request a new game from server
    #
    def new_game()
      @game_world = @connection.request_game
      x, y = @game_world.upper_left_corner
      x2, y2 =  @game_world.lower_right_corner
      "world from (%d,%d) to (%d,%d)" % [x, y, x2, y2]
    end

  end

  #
  # Extends a stream with +#<+
  #
  module ConsoleOut

    #
    # Prints +message+ and a prompt; flushes stream
    #
    def < (message)
      self.write(message + "\n> ")
      self.flush()
    end

  end

  #
  # Ruby client for the Skirmish tactical and programming game of
  # medieval war
  #
  # Invoke as +skirmish-client/bin/skirmish-client --console+
  #
  # Then you may interact with the client using the commands in
  # ConsoleCommands and/or the Ruby programming languages
  #
  # === Example:
  #  skirmish# skirmish-client/bin/skirmish-client -console
  #  Skirmish Client
  #  > connect "localhost", 1657, "my.id", "my password"
  #   => "connected"
  #  > new_game
  #   => world from (0,0) to (2999,2999)
  #  > exit
  #
  class Client

    def initialize()
      if ARGV[0] == '--console'
        console = ConsoleCommands.new()
        out = STDOUT
        out.extend(ConsoleOut)
        repl(STDIN, out)
      end
    end

    #
    # A Read-Eval-Print Loop using streams +in+, +out+
    #
    def repl(in, out)
        out < 'Skirmish Client'
        while true
          line = in.gets()
          if line
            begin
              out < (" => " + console.instance_eval(line).inspect)
            rescue StandardError, ScriptError => e
              out < "#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}"
            end
          else
            exit
          end
        end
    end

  end

  Client.new

end

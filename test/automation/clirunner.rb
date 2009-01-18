require 'automation'
require 'log'

require 'timeout'
require 'test/unit/assertions'

module Skirmish

  module Automation

    # Thrown when a CLI does not produce the expected result
    class MatchError < Exception; end

    #
    # Extends a pipe with methods to script Command Line Interfaces
    #
    # Use the _open_ module method to create a CliRunner-extended pipe;
    # call +#<<+ to write to it and +#>>+ to read until the next prompt and
    # match a regular expression or string against the output; close the
    # invoked program with +#close+; if you need to examine or change the
    # settings, use +#cli_options+.
    #
    # === Example
    #
    #     server = CliRunner.open("server", "erl", /\d>/, /Eshell/)
    #     server << "code:add_path(#{path_to_server})." >> /true/
    #     server << "application:start(skirmish_server)." >> /ok/
    #     server.close "q()."
    #

    module CliRunner

      include Test::Unit::Assertions

      #
      # Opens a CliRunner-extended pipe to a given shell command
      #
      # +name+ is the name to give this cli in the logs, +command+ is
      # the command to start the cli, +prompt+#=~ is invoked when
      # looking for a prompt, optional +expect_pattern+ is a pattern
      # that signifies successful initialization of the cli
      #
      def CliRunner.open name, command, prompt = />/, expect_pattern = //
        pipe = IO.popen(command + " 2>&1", "w+")
        pipe.extend(CliRunner)
        pipe.cli_options :name => name, :prompt => prompt, :default_timeout => 30
        pipe >> expect_pattern
        return pipe
      end

      #
      # Sends a command to the pipe
      #
      # Writes +command+ followed by a newline to the pipe.
      #
      # Plays well with +#>>+. e.g.
      #
      # bash << "echo 'Hello'" >> /Hello/
      #
      def << command
        log.info(cli_option(:name)) { "<< #{command}" }
        self.write(command + "\n")
        @cli_last_command = command
        self
      end

      #
      # Reads from the pipe, looking for a match of the prompt regular
      # expression (settable in _open_ or through _cli_options_), and
      # then matches the string read against +pattern+ (pass +//+ if you
      # don't need to validate the output).
      #
      # Returns the match object, in case
      #
      # +timeout+ is the time, in seconds, to wait for output.
      #
      # Throws +MatchError+, +Timeout::Error+ or +EOFError+ on failure
      #
      def >> pattern, timeout = cli_option(:default_timeout)
        match = nil
        resp = ""
        begin Timeout::timeout(timeout) do
            until resp =~ cli_option(:prompt)
              resp += self.read_once
            end
            raise MatchError unless match = resp =~ pattern
          end
        rescue MatchError, Timeout::Error, EOFError => e
          name = cli_option(:name)
          command = @cli_last_command || "initialization"
          raise(e,
                "in #{name}, after '#{command}':\n" \
                "#{e.class}: #{e.to_s}\n" \
                "#{pattern} not in output:\n" \
                "#{resp}")
        end
        return match
      end

      #
      # Invokes +command+ and asserts that program is closed, then
      # closes the pipe
      #
      def close command, timeout=cli_option(:default_timeout)
        assert_raise EOFError, "#{cli_option(:name)} failed to close properly" do
          self << command
          Timeout::timeout(timeout) do
            while true
              self.read_once
            end
          end
        end
        super()
      end

      #
      # Modifies and CliRunner options if given and returns hash of
      # current options
      #
      def cli_options(new_options = {})
        @cli_options ||= {}
        return @cli_options.merge!(new_options)
      end
      #
      # Returns a CliRunner option. +key+ is a symbol, the name of the option.
      #
      def cli_option(key)
        return @cli_options[key]
      end


      # Waits until there is something to read and reads it, without
      # blocking other threads.
      #
      # May throw +EOFError+
      def read_once # :nodoc:
        begin
          response = self.read_nonblock(8196)
          response.split.each { |l| log.info(cli_option(:name)) { ">> #{l}" } }
        rescue Errno::EAGAIN
          IO.select([self])
          retry
        end
        return response
      end

    end

  end

end

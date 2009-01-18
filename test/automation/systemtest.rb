require 'test/unit'
require 'automation'

module Skirmish

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
      m = build_message(message,
                        "exception expected but none was thrown.")
      ex = nil
      assert_block(m) do
        begin
          yield
        rescue Exception => ex
          break
        end
        false
      end
      m = build_message(message,
                        "exception <?> message did not match pattern <?>\n",
                        ex, pattern)
      assert_block(m) do
        ex.message =~ pattern
      end
      ex
    end

  end

end

module Skirmish; end

module Skirmish::Automation

  #
  # Internal helper methods
  #
  module Helper # :nodoc:

    # TODO: google idiom for getting backtrace
    # this is so ugly
    def Helper.get_backtrace
      trace = nil
      begin raise Exception
      rescue Exception => ex
        trace = ex.backtrace
        trace.pop
      end
      trace
    end

  end

end

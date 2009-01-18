require 'logger'

module Kernel

  #
  # Creates or returns a +Logger+ object that logs to +STDOUT+
  #
  def log
    unless $automation_logger
      $automation_logger = Logger.new(STDOUT)
      $automation_logger.level = Logger::ERROR
    end
    $automation_logger
  end
end

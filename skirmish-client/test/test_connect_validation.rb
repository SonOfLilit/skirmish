require 'test/unit'

require 'helpers/connection'


class TestConnectValidation < Test::Unit::TestCase

  include ConnectionTestHelper

  def test_too_short_id
    local_error_on_connect_with "aa", "bcdefg"
  end
  def test_empty_id
    local_error_on_connect_with "", "bcdefg"
  end
  def test_garbage_id
    local_error_on_connect_with "ab,", "cdefg"
    local_error_on_connect_with "!ab", "cdefg"
    local_error_on_connect_with "--asdf--", "cdefg"
    local_error_on_connect_with "\n", "cdefg"
    local_error_on_connect_with "-=[Ment0r]=-,", "cdefg"
  end
  def test_newline_in_secret
    local_error_on_connect_with "abc", "\n"
    local_error_on_connect_with "abc", "\n\n asdf \n\n"
    local_error_on_connect_with '!@#$', "\n"
  end
  def test_too_long_id
    local_error_on_connect_with "a" * 17, "bcdefg"
  end
  def test_very_long_id
    local_error_on_connect_with "a" * 1024, "bcdefg"
  end
  def test_too_long_secret
    local_error_on_connect_with "aaa", "a" * 256
  end
  def test_very_long_secret
    local_error_on_connect_with "aaa", random_string(1024)
  end

end


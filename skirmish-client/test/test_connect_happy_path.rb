require 'test/unit'

require 'helpers/connection'


class TestConnectHappyPath < Test::Unit::TestCase

  include ConnectionTestHelper

  def test_simplest_valid_case
    connect_with "abc", ""
  end
  def test_average_valid_case
    connect_with "abcdefg", "hijklmnop"
  end
  def test_long_id_and_secret
    connect_with "a" * 16, "a" * 228 # there was a bug at this length exactly
  end
  def test_longest_allowed_id_and_secret
    connect_with "a" * 16, "a" * 255 # maximum lengths
  end
  def test_most_contrived_valid_case
    # \x00\01\02..\x08\x09\x0B\x0C..\xFF -- all but the newline
    array = ([*(0..(?\n-1))] + [*((?\n+1)..255)])
    secret = array.pack("C*")
    assert_equal 255, secret.length
    connect_with "aA1.aA1.aA1.aA1.", secret
  end

end

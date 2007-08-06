require 'test/unit'

class SampleTest < Test::Unit::TestCase
  def test_might_error
    junk.foo
  end
  
  def test_i_should_pass
    assert true
  end
  
  def test_should_pass_longer
    foo = 2 + 0
    assert_equal 2, foo
    assert_match /foo(.*)/, "foobarbunkle"
  end
  
  def test_will_fail
    a = "hello world"
    a.split(' ')
    assert_equal 88, a.length, "bad length"
  end

  def test_will_pass
    ruby = :awesome
    testing = :essential
    assert true
  end
end

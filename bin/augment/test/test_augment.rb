$LOAD_PATH << File.dirname(__FILE__) + '/../lib'
require 'test/unit'
require 'augment'

class MockAugmentor < Augmentor
  class << self
    def run(target)
      Layer.new(target, (0 .. 1), 'red', 'you screwed up.')
      Layer.new(target, (1 .. 2), 'green', 'you rule.')
      Layer.new(target, (2 .. 3), 'blue', 'you are blue.')
    end
  end
  Augmentor::AUGMENTORS << self
end

Options[:output] = 'ansi'

class TestAugmentor < Test::Unit::TestCase
  def test_gather_data
    MockAugmentor.run(fixture('colors.txt'))
    output = Augmentor.augment(fixture('colors.txt'))

    assert_match "red".colorize('red'), output
  end

  private
  
  def fixture(name)
    File.dirname(__FILE__) + '/fixtures/' + name
  end
end

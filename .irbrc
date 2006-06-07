def ri(obj)
  puts `ri #{obj}`
end

require 'irb/completion'


# rails-specific

if false # how to determine if it's rails if rails hasn't been initialized yet? hmmm.
class ActiveRecord::Base
  # Write a fixture file for testing
  def self.to_fixture
    write_file(File.expand_path("test/fixtures/#{table_name}.yml", RAILS_ROOT),
               self.find(:all).inject({}) { |hsh, record|
                 hsh.merge(record.id => record.attributes)
               }.to_yaml)
  end

  def self.write_file(path, content)
    f = File.new(path, "w+")
    f.puts content
    f.close
  end
end

def all_to_fixtures
  Dir.glob('app/models/*.rb').map{|f| Object.const_get(Inflector.camelize(f.split('/').last.split('.')[0..-2])) }.each do |m|
    m.to_fixture
  end
end

end
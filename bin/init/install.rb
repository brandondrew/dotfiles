require 'yaml'

debs = YAML.load_file(File.dirname(__FILE__) + '/debs.yml') +
  YAML.load_file(File.dirname(__FILE__) + '/client-debs.yml')

gems = YAML.load_file(File.dirname(__FILE__) + '/gems.yml')

system "sudo apt-get install #{debs.join(' ')}"
system "sudo gem install #{gems.join(' ')}"

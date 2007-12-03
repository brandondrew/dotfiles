#!/usr/bin/env ruby

require 'yaml'

if `whoami`.chomp != "root"
  puts "You must be root."
  exit 1
end

debs = YAML.load(File.read('debs.yml'))
gems = YAML.load(File.read('gems.yml'))

system "apt-get install #{debs.join(' ')}"

# install rubygems
if !File.exist?('/usr/bin/gem')
  system "wget http://rubyforge.org/frs/download.php/20989/rubygems-0.9.4.tgz"
  system "tar xzf rubygems-0.9.4.tgz"
  system "cd rubygems-0.9.4"
  system "ruby setup.rb"
  system "gem update --system"
end

system "gem install rake" # sometimes we get a false start
system "gem install -y #{gems.join(' ')}"

%w(irb ri rdoc ruby).each { |p| system "update-alternatives --install /usr/bin/#{p} #{p} /usr/bin/#{p}1.8 10" }

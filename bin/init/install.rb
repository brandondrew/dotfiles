#!/usr/bin/env ruby

require 'yaml'

if `whoami`.chomp != "root"
  puts "You must be root."
  exit 1
end

debs = YAML.load(File.read('debs.yml'))
gems = YAML.load(File.read('gems.yml')) # TODO: parsetree is called something else; as is redcloth

if !system "apt-get install #{debs.join(' ')}"
  raise "Couldn't install packages"
end

# install rubygems
if !File.exist?('/usr/bin/gem')
  system "wget http://rubyforge.org/frs/download.php/29548/rubygems-1.0.1.tgz"
  system "tar xzf rubygems-1.0.1.tgz"
  system "cd rubygems-1.0.1; ruby setup.rb"
  system "gem update --system"
end

%w(irb ri rdoc ruby gem).each { |p| system "update-alternatives --install /usr/bin/#{p} #{p} /usr/bin/#{p}1.8 10" }

system "gem install rake" # sometimes we get a false start
system "gem install #{gems.join(' ')}"

sed -i s/DisallowTCP=true/DisallowTCP=false/ /etc/gdm/gdm.conf # gotta have my remote X!

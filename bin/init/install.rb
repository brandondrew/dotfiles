#!/usr/bin/env ruby

require 'yaml'

if `whoami`.chomp != "root"
  puts "You must be root."
  exit 1
end

debs = YAML.load(File.read('debs.yml'))
gems = YAML.load(File.read('gems.yml')) # TODO: parsetree is called something else; as is redcloth
roastbeefs = YAML.load(File.read('roastbeefs.yml'))

if !system "apt-get install #{debs.join(' ')}"
  raise "Couldn't install packages"
end

system "apt-get remove app-install-data-commercial"

# install rubygems
if !File.exist?('/usr/bin/gem')
  system "wget http://rubyforge.org/frs/download.php/29548/rubygems-1.0.1.tgz"
  system "tar xzf rubygems-1.0.1.tgz"
  system "cd rubygems-1.0.1; ruby1.8 setup.rb"
  system "gem update --system"
end

%w(irb ri rdoc ruby gem rake).each { |p| system "update-alternatives --install /usr/bin/#{p} #{p} /usr/bin/#{p}1.8 10" }

system "gem install rake" # sometimes we get a false start
system "gem install #{gems.join(' ')}"

system "sed -i s/DisallowTCP=true/DisallowTCP=false/ /etc/gdm/gdm.conf" # gotta have my remote X!

# MPD setup
mpd_conf = File.read('/etc/mpd.conf')
File.open('/etc/mpd.conf', 'w') { |fp| fp.puts mpd_conf.gsub("/var/lib/mpd/music", '/home/phil/music') }
system "/etc/init.d/mpd restart; mpc update"

roastbeefs.each { |beef| system "sudo -u $USER roastbeef install #{beef}" }

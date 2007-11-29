system "ln -s .sshconfig .ssh/config"
system "ln -s /usr/lib/ruby/gems/1.8/gems ~/gems"

%w(Music Pictures Public Templates Video).each { |d| system "rm -rf ~/#{d}" }

sources = YAML.load(File.read('sources.yml'))

FileUtils.cd(File.expand_path("~/src"))

sources.each do |type, location|
  "#{type} #{location}"
end

load "~/bin/gconf.rb"

"cd ~/src/emacs
./configure
make bootstrap
make
sudo make install

cd ~/src/screen/src
./configure
make
sudo make install

cd ~/src/ruby-dbus
ruby setup.rb config
ruby setup.rb setup
sudo ruby setup.rb install

firefox firebug1.0-current.xpi
firefox mozlab-current.xpi".split("\n").each { |cmd| system cmd }

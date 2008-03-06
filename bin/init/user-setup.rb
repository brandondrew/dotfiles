require 'fileutils'
FileUtils.cd File.expand_path("~")
system "mv dotfiles/* ~"
system "mv dotfiles/.* ~"

system "cp .sshconfig .ssh/config"
system "ln -s /usr/lib/ruby/gems/1.8/gems ~/gems"

%w(Music Pictures Public Templates Video).each { |d| system "rm -rf ~/#{d}" }

system "ruby ~/bin/gconf.rb"

system "mv ~/dotfiles/* ~"
system "mv ~/dotfiles/.* ~"

system "cp ~/.sshconfig ~/.ssh/config"
system "ln -s /usr/lib/ruby/gems/1.8/gems ~/gems"

system "ruby ~/bin/gconf.rb"

%w(Music Examples Desktop Pictures Public Templates Videos Documents).each { |d| system "rm -rf ~/#{d}" }

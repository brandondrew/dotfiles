["mv ~/dotfiles/* ~",
 "mv ~/dotfiles/.* ~",
 "mkdir ~/.ssh",
 "cp ~/.sshconfig ~/.ssh/config",
 "ln -s /usr/lib/ruby/gems/1.8/gems ~/gems",
 "ruby ~/bin/gconf.rb"].map{ |c| system c rescue nil }

# TODO: set up proper gconf panel and keyboard options

%w(Music Examples Desktop Pictures Public Templates Videos Documents).each { |d| system "rm -rf ~/#{d}" }

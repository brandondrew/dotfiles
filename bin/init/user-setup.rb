["mv ~/dotfiles/* ~",
 "mv ~/dotfiles/.* ~",
 "cp ~/.sshconfig ~/.ssh/config",
 "ln -s /usr/local/lib/ruby/gems/1.8/gems ~/gems",
].map{ |c| system c rescue puts "#{c} failed." }

%w(Music Examples Desktop Pictures Public Templates Videos Documents).each { |d| system "rm -rf ~/#{d}" }

require 'roastbeef'
%w(ruby emacs emacs-w3m conkeror).each { |c| RoastBeef.install(c) }

# TODO: how to set gconf keys without ruby-gnome2 installed?
# http://ruby-gnome2.sourceforge.jp/data/ruby-gnome2-api.tar.gz

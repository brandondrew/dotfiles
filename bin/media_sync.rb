#!/usr/bin/env ruby

OPTIONS = '-r -u --ignore-existing'
CMD = if ARGV.first == 'from-disk'
        "rsync #{OPTIONS} /media/disk/ ~/%s"
      else
        "rsync #{OPTIONS} ~/%s /media/disk/"
      end

SETS = { :default => ['Photos', 'documents', 'music'],
  'dynabook' => ['.gnome2/f-spot/photos.db', 'Mail'] }

(SETS[:default] + SETS[`hostname`.chomp]).each do |to_sync|
  system(CMD % [to_sync, to_sync])
end

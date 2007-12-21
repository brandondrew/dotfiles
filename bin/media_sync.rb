#!/usr/bin/env ruby

PEER = ARGV.first

DIRS = ['documents', 'music', 'Photos']
FILES = ['~/.gnome2/f-spot/photos.db']

def copy_file(to, from)
  system("scp \"#{to}\" \"#{from}\"")
end

def copy_file_to_peer(file)
  copy_file file, "#{PEER}:#{file}"
end

def copy_file_from_peer(file)
  copy_file "#{PEER}:#{file}", file
end

# for directories, we assume if a file exists at the same path in both systems, it's identical.
DIRS.each do |dir|
  local_files = system("find #{dir} -type f").split("\n")
  remote_files = system("ssh #{PEER} find #{dir} -type f").split("\n")

  local_only = local_files - remote_files
  remote_only = remote_files - local_files

  local_only.each { |f| copy_file_to_peer f }
  remote_only.each { |f| copy_file_from_peer f }
end

FILES.each do |file|
  system("scp #{PEER}:#{file} /tmp/sync-file")
  # if modified more recently, move it to file
end

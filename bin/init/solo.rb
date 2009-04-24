#
# Chef Solo Config File
#
log_level          :info
log_location       STDOUT
file_cache_path    "/tmp/chef-solo"
cookbook_path      "#{ENV['HOME']}/bin/init/"
Chef::Log::Formatter.show_time = false

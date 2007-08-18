#!/usr/bin/env ruby
# ruby really *is* the new perl!

def current_ip
  router_password = File.read(File.expand_path('~/.router_password'))
  `curl -# -u  admin:#{router_password} http://192.168.0.1/cgi-bin/webcm?getpage=..%2Fhtml%2Findex_real.html\\&var%3Aconname=connection0\\&var%3Acontype=pppoa 2>&1 /dev/null`.match(/(\d\d?\d?\.\d\d?\d?\.\d\d?\d?\.\d\d?\d?)/).to_s
end

`touch ~/.ip`

if File.read(File.expand_path('~/.ip')) != current_ip
  # IP has changed!
  File.open(File.expand_path('~/.new_ip')){ |f| f.puts current_ip }
  `scp ~/.new_ip philhag@hagelb.org`
  `mv ~/.new_ip ~/.ip`
end

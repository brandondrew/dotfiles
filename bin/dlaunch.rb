#!/usr/bin/env ruby

# Get the standard list of executable files in your $PATH
# TODO: save history, sort by usages
@program_list = {}
path_glob = ENV["PATH"].gsub(/,/, '\\,').tr(":",",")

Dir.glob("{#{path_glob}}/*").select do |fname|
  File.file?(fname) && File.executable?(fname)
end.map{|fname| File.basename(fname)}.uniq.each{|v| @program_list[v] = v}

system `echo "#{@program_list.keys.join("\n")}" | dmenu`

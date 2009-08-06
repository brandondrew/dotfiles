#!/usr/bin/env ruby

books = Dir.glob(File.expand_path("~/documents/books/*pdf")).map{|b| File.basename(b)}
chosen = `echo "#{books.join("\n")}" | dmenu`
system "evince \"documents/books/#{chosen}\"" unless chosen.empty?

# {{{ ======== ruby-wmii CONFIGURATION BEGINS HERE ==============

# programs to run when wmiirc starts
# one per line, they're run sequentially right before the main loop begins
START_PROGS = <<EOF
xsetroot -solid '#333333'
EOF

# {{{ WM CONFIGURATION
WMII::Configuration.define do
  border      1
  font        "fixed"
  selcolors   '#FFFFFF #248047 #147027'
  normcolors  '#4D4E4F #DDDDAA #FFFFCC'
  colmode     'default'
  colwidth    0
  grabmod     'Control-t'
  rules <<EOF
/Kdict.*/ -> dict
/XMMS.*/ -> ~
/Gaim.*/ -> ~
/Gimp.*/ -> ~
/MPlayer.*/ -> ~
/XForm.*/ -> ~
/XSane.*/ -> ~
/fontforge.*/ -> ~
/.*/ -> !
/.*/ -> 1
EOF

  # Translate the following names in the on_key and use_binding definitions.
  key_subs  :MODKEY  => "Control-t,",
            :MODKEY2 => :Mod4,
            :LEFT    => :b,
            :RIGHT   => :f,
            :UP      => :p,
            :DOWN    => :n


  # Constant used by the intellisort tag selection mechanism
  # set it to   0.0 <= value <= 1.0
  # Lower values make recent choices more likely (modified first order
  # markovian process with exponential decay):
  # 0.0 means that only the last transition counts (all others forgotten)
  # 1.0 means that the probabilities aren't biased to make recent choices more
  #     likely
  view_history_decay 0.8

  # Favor the view we came from in intellisort.
  # 1.0: that view is the first choice
  # 0.0: that view comes after all views with non-zero transition probability,
  #      but before all views we haven't yet jumped to from the current one
  view_history_prev_bias 0.4

# {{{ Plugin config

  plugin_config["standard"]["x-terminal-emulator"] = "x-terminal-emulator"

  plugin_config["standard:actions"]["history_size"] = 3  # set to 0 to disable
  plugin_config["standard:programs"]["history_size"] = 5 # set to 0 to disable

  plugin_config["standard:volume"]["mixer"] = "Master"
  
  plugin_config["standard:mode"]["mode_toggle_keys"] = ["MODKEY2-space"]

  plugin_config["standard:status"]["refresh_time"] = 1
  currload = nil
  Thread.new{ loop { currload = `uptime`.chomp.sub(/.*: /,"").gsub(/,/,""); sleep 10 } }
  plugin_config["standard:status"]["text_proc"] = lambda do
    "#{Time.new.strftime("%d/%m/%Y %X %Z")} #{currload}"
  end

  plugin_config["standard:battery-monitor"]["statefile"] = 
      '/proc/acpi/battery/BAT0/state'
  plugin_config["standard:battery-monitor"]["infofile"] =
      '/proc/acpi/battery/BAT0/info'

  plugin_config["standard:actions"]["internal"] = {
    "browser" => lambda do |wmii, *selection|
      selection = selection[0]
      selection ||= `wmiipsel`.strip
      case browser = ENV["BROWSER"]
      when nil: system "wmiisetsid /etc/alternatives/x-www-browser '#{selection}' &"
      else system "wmiisetsid #{browser} '#{selection}' &"
      end
    end,
    "google" => lambda do |wmii, *selection|
      require 'cgi'
      if selection && !selection.empty?
        selection = CGI.escape(selection.join(" "))
      else
        selection = CGI.escape(%!#{`wmiipsel`.strip}!)
      end
      url = "http://www.google.com/search?q=#{selection}"
      case browser = ENV["BROWSER"]
      when nil: system "wmiisetsid /etc/alternatives/x-www-browser '#{url}' &"
      else system "wmiisetsid #{browser} '#{url}' &"
      end
    end,
    "screenshot" => lambda do |wmii, *base|
      fname = (base[0] || "screenshot") + "000"
      fname.succ! while File.exist?(File.join(ENV["HOME"], "tmp", "fname.png"))
      system("import -window root ~/tmp/#{fname}.png &")
    end,
    "rename-view" => lambda do |wmii, *args|
      unless /./ =~ (new_name = args[0].to_s)
        new_name = wmii.wmiimenu([]).value  # blocking, OK
      end
      old = wmii.curr_view
      wmii.read("/client").each do |line|
        cid = line.split(/\s+/).last
        wmii.write("/client/#{cid}/tags", 
              wmii.read("/client/#{cid}/tags").gsub(/\b#{Regexp.escape(old)}\b/, new_name))
      end
      wmii.view new_name
    end,
    "quit" => lambda do |wmii|
      wmii.write "/ctl", "quit"
    end,
    "config-help" => lambda do |wmii|
      IO.popen("xmessage -file -", "w"){|f| f.puts WMIIRC_HELP_MESSAGE; f.close_write }
    end
  }

#{{{ Import bindings and bar applets
  from "standard"  do
    use_bar_applet "volume", 999
    use_bar_applet "mode", 900
    use_bar_applet "status", 100
    #use_bar_applet "cpuinfo", 150
    #use_bar_applet "mpd", 110
    #use_bar_applet "battery-monitor

    use_binding "dict-lookup"
    use_binding "execute-program-with-tag"
    use_binding "execute-action"
    use_binding "execute-program"
    (0..9).each{|k| use_binding "numeric-jump-#{k}"  }
    use_binding "tag-jump"
    use_binding "retag"
    use_binding "retag-jump"
    use_binding "namespace-retag"
    use_binding "namespace-retag-jump"
    ('a'..'z').each{|k| use_binding "letter-jump-#{k}" }
    (0..9).each{|k| use_binding "numeric-retag-#{k}" }
    use_binding "move-prev"
    use_binding "move-next"
    use_binding "namespace-move-prev"
    use_binding "namespace-move-next"
  end

  # {{{ Click on view bars
  on_barclick(/./, MOUSE_BUTTON_LEFT){|name,| view name}
  on_barclick(/./, MOUSE_BUTTON_RIGHT){|name,| view name}

  # {{{ Tag all browser instances as 'web' in addition to the current tag
  browsers = %w[Firefox Konqueror]
  browser_re = /^#{browsers.join("|")}/
  on_createclient(condition{|c| browser_re =~ read("/client/#{c}/class")}) do |cid|
    write("/client/#{cid}/tags", normalize(read("/client/#{cid}/tags") + "+web"))
  end

#{{{ Simpler key bindings --- not defined in plugins
  on_key("MODKEY-LEFT"){ write "/view/ctl", "select prev" }
  on_key("MODKEY-RIGHT"){ write "/view/ctl", "select next" }
  on_key("MODKEY-DOWN"){ write "/view/sel/ctl", "select next" }
  on_key("MODKEY-UP"){ write "/view/sel/ctl", "select prev" }
  on_key("MODKEY-space"){ write "/view/ctl", "select toggle" }
  on_key("MODKEY-d"){ write "/view/sel/mode", "default" }
  on_key("MODKEY-s"){ write "/view/sel/mode", "stack" }
  on_key("MODKEY-m"){ write "/view/sel/mode", "max" }
  on_key("MODKEY-f"){ write "/view/0/sel/geom", "0 0 east south" }
  on_key("MODKEY-i"){ write "/view/sel/sel/geom", "+0 +0 +0 +48" }
  on_key("MODKEY-Shift-i"){ write "/view/sel/sel/geom", "+0 +0 +0 -48" }
  on_key("MODKEY-Return") do 
    term = plugin_config["standard"]["x-terminal-emulator"] || "xterm"
    system "wmiisetsid #{term} &"
  end
  on_key("MODKEY-Shift-LEFT"){ write "/view/sel/sel/ctl", "sendto prev" }
  on_key("MODKEY-Shift-RIGHT"){ write "/view/sel/sel/ctl", "sendto next" }
  on_key("MODKEY-Shift-DOWN"){ write "/view/sel/sel/ctl", "swap down" }
  on_key("MODKEY-Shift-UP"){ write "/view/sel/sel/ctl", "swap up" }
  on_key("MODKEY-Shift-space"){ write "/view/sel/sel/ctl", "sendto toggle" }
  on_key("MODKEY-Shift-c"){ write "/view/sel/sel/ctl", "kill" }
  on_key("MODKEY-r"){ view prev_view }
  on_key("MODKEY-Control-LEFT") { write "/view/sel/sel/ctl", "swap prev" }
  on_key("MODKEY-Control-RIGHT"){ write "/view/sel/sel/ctl", "swap next" }

  
# {{{ ======== CONFIGURATION ENDS HERE ==============
end

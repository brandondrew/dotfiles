# Copyright (c) 2006 Mauricio Fernandez <mfp@acm.org> 
#      http://eigenclass.org/hiki.rb?wmii+ruby
# Licensed under the same terms as Ruby (see LICENSE).
#
# ===========================================================================
#
#{{{ Core bindings and applets defined as the "standard" plugin
# 
# This file must be placed in $HOME/.wmii-3/plugins.
# 
# It will be overwritten when upgrading ruby-wmii, so modifications will be
# lost!
#
# If you want to change a standard binding/applet, either:
# * send a patch to <mfp@acm.org> (put wmii in the subject)
# * copy the relevant code to another file under $HOME/.wmii-3/plugins
#   and edit as needed. Don't forget to change the namespace, e.g.
#     Plugin.define "my-address@isp.com" do
#        .... paste the code here & change it ...
#     end
#   you'll also have to change wmiirc-config.rb to import the new
#   bindings/applets. For example, if you want to use new definitions of
#   the "retag" binding and the "volume" applet, comment 
#       use_binding "retag"
#       use_bar_applet "volume"
#   in the   from "standard" do .... end   area, and add
#     from "my-address@isp.com" do
#       use_binding "retag"
#       use_bar_applet "volume"
#     end
#
# Read the top of wmiirc or type Alt-a -> config-help for more information on
# plugins.
#
# The standard plugin is updated regularly with new functionality and
# *bugfixes*. You cannot benefit from them automatically on upgrade if you
# copy the code and modify the definitions under your own namespace, though.
# ===========================================================================

Plugin.define "standard"  do
  author '"Mauricio Fernandez" <mfp@acm.org>'

  #{{{ Volume control
  bar_applet("volume", 990) do |wmii, bar|
    mixer = wmii.plugin_config["standard:volume"]["mixer"] || "Master"
    update_volume = lambda do |increment|
      sign = increment < 0 ? "-" : "+"
      status = `amixer set #{mixer},0 #{increment.abs}#{sign}`
      volume = status [/\[(\d+%)\]/]
      volume = "OFF" if status[/\[off\]/]
      bar.data = "VOL #{volume}"
    end
    Thread.new{ loop { update_volume[0]; sleep 10 } }

    term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
    bar.on_click(MOUSE_SCROLL_UP){ update_volume[+1] }
    bar.on_click(MOUSE_SCROLL_DOWN){ update_volume[-1] }
    bar.on_click(MOUSE_BUTTON_LEFT) do
      handler = wmii.on_createclient do |cid|
        wmii.write("/view/sel/sel/ctl", "sendto 0")
        wmii.write("/view/sel/sel/geom", "0 100 east south-100")
        wmii.unregister handler
      end
      system "wmiisetsid #{term} -e alsamixer &"
    end
    bar.on_click(MOUSE_BUTTON_RIGHT) do
      case `amixer get #{mixer},0`
      when /\[off\]/: `amixer set #{mixer},0 unmute`
      when /\[on\]/ : `amixer set #{mixer},0 mute`
      end
      update_volume[0]
    end
  end

  #{{{ Modal keybindings: raw/normal modes.
  bar_applet("mode", 980) do |wmii, bar|
    raw_mode = false
    saved_keys = nil
    keys = wmii.plugin_config["standard:mode"]["mode_toggle_keys"] || ["MODKEY2-space"]
    h = wmii.on_key(*keys) do
      case raw_mode
      when true
        wmii.write("/def/keys", saved_keys)
        raw_mode = false
        bar.data = "-N-"
        LOGGER.info "Entering NORMAL mode."
      when false
        saved_keys = wmii.read("/def/keys")
        wmii.write("/def/keys", h.key)
        raw_mode = true
        bar.data = "-R-"
        LOGGER.info "Entering RAW mode."
      end
    end
    bar.data = "-N-"
  end

# {{{ Dictionary
  bar_applet("dict", 880, "DICT") do |wmii, bar|
    dict_ask_and_define = lambda do
      Thread.new do
        wmii.wmiimenu([]) do |phrase|
          system "dcop kdict KDictIface definePhrase '#{phrase}'"
        end.value  # block until we get the word
        wmii.set_curr_view "dict"
      end
    end
    wmii.on_key("MODKEY-Control-d"){ dict_ask_and_define.call }
    bar.on_click(MOUSE_BUTTON_LEFT){ dict_ask_and_define.call }
    bar.on_click(MOUSE_BUTTON_RIGHT){ wmii.set_curr_view "dict" }
  end

# {{{ Battery monitor
# Originally by Wael Nasreddine <wael@phoenixlinux.org>.
  bar_applet("battery-monitor", 950) do |wmii, bar|
    statefile = wmii.plugin_config["standard:battery-monitor"]["statefile"] ||
      '/proc/acpi/battery/BAT0/state'
    infofile = wmii.plugin_config["standard:battery-monitor"]["infofile"] ||
      '/proc/acpi/battery/BAT0/info'
    low = wmii.plugin_config["standard:battery-monitor"]["low"] || 5
    low_action = wmii.plugin_config["standard:battery-monitor"]["low-action"] ||
      'echo "Low battery" | xmessage -center -buttons quit:0 -default quit -file -'
    critical = wmii.plugin_config["standard:battery-monitor"]["critical"] || 1
    critical_action = wmii.plugin_config["standard:battery-monitor"]["critical-action"] ||
      'echo "Critical battery" | xmessage -center -buttons quit:0 -default quit -file -'
    warned_low = false
    warned_critical = false
    Thread.new do
      loop do
        batt = IO.readlines(statefile)
        battinfo = IO.readlines(infofile)
        batt_percent = ((batt[4].gsub(/.*:\s*/,'').chomp.chomp("mAh").to_f / battinfo[2].gsub(/.*:\s*/,'').chomp.chomp(" mAh").to_f ) * 100).to_i
        batt_state = batt[2].gsub(/.*:\s*/,'').chomp
        # Take action in case battery is low/critical
        if batt_state == "discharging" && batt_percent <= critical && warned_critical == false
          LOGGER.info "Warning about critical battery."
          system("wmiisetsid #{critical_action} &")
          warned_critical = true
        elsif batt_state == "discharging" && batt_percent <= low && warned_low == false
          LOGGER.info "Warning about low battery."
          system("wmiisetsid #{low_action} &")
          warned_low = true
        else
          warned_low = false
          warned_critical = false
        end
        # If percent is 100 and state is discharging then
        # the battery is full and not discharging.
        batt_state = "=" if batt_state == "charged" || ( batt_state == "discharging" && batt_percent >= 97 )
        batt_state = "^" if batt_state == "charging"
        batt_state = "v" if batt_state == "discharging"
        text = "#{batt_state} #{batt_percent} #{batt_state}"
        bar.data = text
        sleep 2
      end
    end
  end

  # {{{ MPD Bar
  # Originally  by Wael Nasreddine <wael@phoenixlinux.org>.
  bar_applet("mpd", 100) do |wmii, bar|
    require 'mpd'
    mpd_do_action = lambda do |action, *args|
      Thread.new do
        begin
          mpd = MPD.new
          r = mpd.__send__(action, *args)
          LOGGER.info "MPD #{action}"
          r
        ensure
          mpd.close
        end
      end
    end
    mpdserv = MPD.new
    update_bar = lambda do
      mpdserv_status = mpdserv.status["state"]
      case mpdserv_status
      when 'play' : text = ">>: "; show_info = true
      when 'pause': text = "||: "; show_info = true
      else show_info = false
      end
      if show_info
        title = mpdserv.strf("%t")[0..(wmii.plugin_config["standard:mpd"]["title_maxlen"] || -1)]
        author = mpdserv.strf("%a")[0..(wmii.plugin_config["standard:mpd"]["author_maxlen"] || -1)]
        bar.data = text + "#{author} - #{title} " + mpdserv.strf("(%e/%l)")
      else   # Player is stopped or connection not yet initialized...
        bar.data = "[]: NOT PLAYING"
      end
    end
    # Initialize MPD status
    Thread.new do
      loop{ begin; update_bar.call; rescue Exception; end; sleep 1 }
    end
    bar.on_click(MOUSE_SCROLL_UP)  { mpd_do_action[:previous] }
    bar.on_click(MOUSE_SCROLL_DOWN){ mpd_do_action[:next] }
    bar.on_click(MOUSE_BUTTON_LEFT) do
      Thread.new do
        begin
          mpd = MPD.new
          mpdserv_status = mpd.status
        ensure 
          mpd.close rescue nil
        end
        case mpdserv_status["state"]
        when "play":           mpd_do_action[:pause]
        when "pause", "stop" : mpd_do_action[:play]
        end
      end
    end
    bar.on_click(MOUSE_BUTTON_RIGHT) do
      mpd_handle = wmii.on_createclient do |cid|
        wmii.write("/view/sel/sel/ctl", "sendto 0")
        wmii.write("/view/sel/sel/geom", "400 0 center+200 south")
        wmii.unregister mpd_handle
      end
      wmii.write("/view/ctl", "select toggle")
      term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
      system "wmiisetsid #{term} -e ncmpc &"
    end
  end

# # {{{ CPU info
  bar_applet("cpuinfo", 800) do |wmii, bar|
    Thread.new do
      loop do
        cpuinfo = IO.readlines("/proc/cpuinfo")[6].split[-1].sub(/\..*$/,'')
        bar.data = cpuinfo.chomp + " Mhz"
        sleep 5
      end
    end
  end

# {{{ Status bar
  bar_applet("status", 0, "STATUS BAR --- init") do |wmii, bar|
    Thread.new do
      text_proc = wmii.plugin_config["standard:status"]["text_proc"]
      unless text_proc
        currload = nil
        Thread.new{ loop { currload = `uptime`.chomp.sub(/.*: /,"").gsub(/,/,""); sleep 10 } }
        text_proc = lambda { "#{Time.new.strftime("%d/%m/%Y %X %Z")} #{currload}" }
      end
      loop do
        bar.data = text_proc.call
        sleep(wmii.plugin_config["standard:status"]["refresh_time"] || 1)
      end
    end

    xmessagebox = "xmessage -center -buttons quit:0 -default quit -file -"
    term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
    fl = lambda{ wmii.write "/view/ctl", "select 0" }
    toggle_fl = lambda{ sleep 2; wmii.write "/view/ctl", "select toggle" }
    left_action = wmii.plugin_config["standard:status"]["left_click_action"] || 
                  lambda { fl[]; system "tzwatch | wmiisetsid #{xmessagebox} &"; toggle_fl[] }
    right_action = wmii.plugin_config["standard:status"]["right_click_action"] || 
                   lambda { fl[]; system "ncal -y | wmiisetsid #{xmessagebox} &"; toggle_fl[] }
    middle_action = wmii.plugin_config["standard:status"]["middle_click_action"] || 
                   lambda {  fl[]; system "wmiisetsid #{term} -e top &"; toggle_fl[] }
    bar.on_click do |name, button|
      current = wmii.curr_view_index
      case button.to_i
      when MOUSE_BUTTON_LEFT:   left_action.call if left_action
      when MOUSE_BUTTON_MIDDLE: middle_action.call if middle_action
      when MOUSE_BUTTON_RIGHT:  right_action.call if right_action
      when MOUSE_SCROLL_UP
        wmii.set_curr_view(wmii.views[wmii.curr_view_index-1] || wmii.views[-1])
      when MOUSE_SCROLL_DOWN
        wmii.set_curr_view(wmii.views[wmii.curr_view_index+1] || wmii.views[0])
      end
    end
  end

  binding("dict-lookup", "MODKEY-Control-d") do |wmii,|
    LOGGER.debug "dict-lookup called!!!"
    Thread.new do
      wmii.wmiimenu([]) do |phrase|
        system "dcop kdict KDictIface definePhrase '#{phrase}'"
      end.value  # block until we get the word
      wmii.set_curr_view "dict"
    end
  end

# {{{ Run program with given tag
  binding("execute-program-with-tag", "MODKEY-Control-y") do |wmii,|
    result = wmii.wmiimenu(wmii.views_intellisort) do |tag|
      if /^\s*$/ !~ tag
        result = wmii.wmiimenu(wmii.program_list) do |prog|
          if /^\s*$/ !~ prog
            system("wmiisetsid #{prog} &")
          else
            wmii.unregister handler
          end
        end
      end
    end
    # using result.value to perform the view switch in the current process so
    # that the transition table can be updated
    Thread.new do 
      tag = result.value
      if /^\s*$/ !~ tag
        handler = wmii.on_createclient do |cid|
          wmii.write("/client/#{cid}/tags", tag)
          wmii.unregister handler
          wmii.view tag
        end
      end
    end
  end

#{{{ actions (internal and WMIIRC_HOME/*) (w/ history)
  standard_internal_actions = {
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
  binding("execute-action", "MODKEY-a") do |wmii,|
    internal_actions = standard_internal_actions.merge(wmii.plugin_config["standard:actions"]["internal"] || {})
    history_size = (wmii.plugin_config["standard:actions"]["history_size"] ||= 5)
    remembered = (wmii.plugin_config["standard:actions"]["history"] ||= [])
    actions = (wmii.action_list + internal_actions.keys.map{|x| x.to_s}).sort.uniq
    internal_actions.each_pair{|name, action| actions.delete(name) unless action }
    list = remembered + actions
    result = wmii.wmiimenu(list) do |choice|
      choices = choice.split(/\s+/)
      cmd = choices.first
      if internal_actions.has_key? cmd
        internal_actions[cmd].call(wmii, *choices[1..-1]) if internal_actions[cmd]
      else
        system("wmiisetsid #{WMIIRC_HOME}/#{choice} &") if /^\s*$/ !~ choice
      end
    end
    # use result.value to record the choice in the current process
    Thread.new do
      if cmd = result.value.split(/\s+/).first
        remembered.delete cmd
        remembered.unshift cmd
        LOGGER.debug "plugin/actions: history #{remembered.inspect}"
        remembered.replace remembered[0, history_size]
      end
    end
  end

#{{{ programs (w/ history)
  binding("execute-program", "MODKEY-shift-a") do |wmii,|
    remembered = (wmii.plugin_config["standard:programs"]["history"] ||= [])
    history_size = (wmii.plugin_config["standard:programs"]["history_size"] ||= 5)
    list = remembered + wmii.program_list
    result = wmii.wmiimenu(list) do |prog|
      if /^\s*$/ !~ prog
        LOGGER.info "Executing #{prog}"
        system("wmiisetsid #{prog} &") 
      end
    end
    # use result.value to record the choice in the current process
    Thread.new do
      if cmd = result.value.split(/\s+/).first
        remembered.delete cmd
        remembered.unshift cmd
        LOGGER.debug "plugin/programs: history #{remembered.inspect}"
        remembered.replace remembered[0, history_size]
      end
    end
  end

#{{{ Either move to the given numeric tag, if it exists, or to the 
# (N-last_numeric_tag)th non-numeric tag.
# e.g.   views  1 3 4 mail web
#    ALT-1  => 1
#    ALT-3  => 3
#    ALT-5 => mail
#    ALT-6 => web
  (0..9).each do |key|
    binding("numeric-jump-#{key}", "MODKEY-#{key}") do |wmii,|
      all_views = wmii.views
      num_tags = all_views.grep(/^\d+$/)
      nkey = (key - 1) % 10
      if num_tags.include?(key.to_s)
        wmii.view(key)
      elsif nkey >= (prev_index = (num_tags.last || 0).to_i)
        non_num_tags = all_views - num_tags
        wmii.view non_num_tags[nkey - prev_index]
      end
    end
  end

#{{{ Move to given view, with intelligent history
  binding("tag-jump", "MODKEY-t") do |wmii,|
    Thread.new do
      # do it this way so the current process can update the transition table
      wmii.view wmii.wmiimenu(wmii.views_intellisort - [wmii.curr_view]).value
    end
  end

  binding("retag", "MODKEY-Shift-t") do |wmii,|
    wmii.wmiimenu(wmii.views_intellisort){|new_tag| wmii.retag_curr_client(new_tag) }
  end
  binding("retag-jump", "MODKEY-Shift-r") do |wmii,|
    rd, wr = IO.pipe
    wmii.wmiimenu(wmii.views_intellisort) do |new_tag|
      rd.close
      wmii.retag_curr_client(new_tag)
      wr.puts new_tag
      wr.close
    end
    wr.close
    Thread.new do
      new_tag = rd.gets
      rd.close
      wmii.view new_tag[/(?![+-]).*/]
    end
  end

  binding("namespace-retag", "MODKEY2-Shift-t") do |wmii,|
    wmii.wmiimenu(wmii.views){|new_tag| wmii.retag_curr_client_ns(new_tag) }
  end
  binding("namespace-retag-jump", "MODKEY2-Shift-r") do |wmii,|
    rd, wr = IO.pipe
    result = wmii.wmiimenu(views) do |new_tag|
      rd.close
      wmii.retag_curr_client_ns(new_tag)
      wr.puts new_tag
      wr.close
    end
    wr.close
    Thread.new do
      subtag = rd.gets
      rd.close
      wmii.view "#{wmii.curr_view[/[^:]+/]}:#{subtag[/(?![+-]).*/]}"
    end
  end

  ('a'..'z').each do |key|
    binding("letter-jump-#{key}", "MODKEY2-#{key}") do |wmii,|
      unless wmii.curr_view[0,1] == key
        wmii.view wmii.views_intellisort.find{|x| x[0,1] == key }
      end
    end
  end
# Retag as specified numeric tag if it exists, or 
# (N-last_numeric_tag)th non-numeric tag.
  (0..9).each do |key|
    binding("numeric-retag-#{key}", "MODKEY-Shift-#{key}") do |wmii,|
      all_views = wmii.views
      num_tags = all_views.grep(/^\d+$/)
      curr_tags = wmii.curr_client_tags
      nkey = (key - 1) % 10
      if num_tags.include? key.to_s or key > all_views.size
        new_tags =  curr_tags.reject{|x| /^\d+$/=~ x } + [key.to_s]
      elsif nkey >= (prev_index = (num_tags.last || 0).to_i)
        non_num_tags = all_views - num_tags
        new_tags = non_num_tags[nkey - prev_index]
      else
        break
      end
      LOGGER.info "Retagging #{curr_tags.inspect} => #{new_tags.inspect}"
      wmii.set_curr_client_tags(new_tags)
    end
  end


  binding("move-prev", "MODKEY-Control-UP", "MODKEY-comma") do |wmii,|
    wmii.view  wmii.views[wmii.curr_view_index-1] || wmii.views[-1]
  end
  binding("move-next", "MODKEY-Control-DOWN", "MODKEY-period") do |wmii,|
    wmii.view  wmii.views[wmii.curr_view_index+1] || wmii.views[0]
  end
  move_within_namespace = lambda do |wmii, offset|
    namespace = wmii.curr_view[/([^:]+)/]
    candidate_views = wmii.views.grep(/#{Regexp.escape(namespace)}\b/)
    dest = candidate_views[candidate_views.index(wmii.curr_view) + offset]
    dest ||= (offset > 0) ? candidate_views[0] : candidate_views[-1]
    wmii.view dest
  end
  binding("namespace-move-prev", "MODKEY2-Shift-UP", "MODKEY2-comma") do |wmii,|
    move_within_namespace.call(wmii, -1)
  end
  binding("namespace-move-next", "MODKEY2-Shift-DOWN", "MODKEY2-period") do |wmii,|
    move_within_namespace.call(wmii, +1)
  end

end

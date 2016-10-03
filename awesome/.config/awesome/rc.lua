-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.autofocus = require("awful.autofocus")
awful.rules = require("awful.rules")
awful.remote = require("awful.remote")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local drop    = require("scratchdrop")
local lain    = require("lain")
local tyrannical = require("tyrannical")

-- Vicious library
local vicious = require("vicious")
-- MPD library
--require("lib/mpd") ; mpc = mpd.new()
-- Keybind libraray by ierton
local keybind = require("lib/keybind")
-- Shifty library
--local shifty = require("shifty")
-- Calendar Module
local calendar2 = require("calendar2")
-- Tagger module
local sharedtags = require("sharedtags")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
--beautiful.init(os.getenv("HOME") .. "/.config/awesome/zenburn.lua")
beautiful.init(os.getenv("HOME") .. "/.config/awesome/solarized.lua")
-- beautiful.init(os.getenv("HOME") .. "/.config/awesome/themes/holo/theme.lua")

-- This is used later as the default terminal and editor to run.
-- home = "/home/alex"
home = os.getenv("HOME")
browser = "firefox"
--browser = "luakit"
--browser = "dwb"
--browser = "uzbl"
--browser_restore = "uzbl-restore.sh"
imclient = "pidgin"
-- newsclient = "rssowl"
filemanager = "dolphin"
--terminal = "terminator"
terminal = "konsole"
scratch = "konsole --profile Scratch"
--emailclient = terminal .. " -e mutt"
emailclient = "xfce4-terminal -e mutt"
editor = os.getenv("EDITOR") or "emacsclient -c"
editor_cmd = terminal .. " -e " .. editor

-- Function aliases
local exec = awful.util.spawn
local sexec = awful.util.spawn_with_shell

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,         -- 1
    awful.layout.suit.tile,             -- 2
    awful.layout.suit.tile.left,        -- 3
    awful.layout.suit.tile.bottom,      -- 4
    awful.layout.suit.tile.top,         -- 5
    awful.layout.suit.fair,             -- 6
    awful.layout.suit.max,              -- 7
    awful.layout.suit.max.fullscreen,   -- 8
    awful.layout.suit.magnifier         -- 9
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
--tags = {}
--for s = 1, screen.count() do
    ---- Each screen has its own tag table.
    --tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
--end
-- }}}

----{{{ SHIFTY: configured tags
--shifty.config.tags = {
    --["term"] =     { layout = layouts[7],          mwfact=0.60, exclusive = false, solitary = false, position = 1, init = true, screen = 1 } ,
    --["web"] =    { layout = layouts[7],  mwfact=0.65, exclusive = false , solitary = false , position = 2, spawn = browser } ,
    ---- ["news"] =  { layout = layouts[1],    exclusive = false, solitary = false, popup = false, position = 3, spawn = newsclient } ,
    --["dev"] =   { layout = layouts[7],   mwfact=0.55, exclusive = false, solitary = false, position = 9, persist = false, spawn = "emacsclient -c"   } ,
    --["media"] = { layout = layouts[1], nopopup = true, persist = false, leave_kills = true, position = 6, spawn = terminal .. " -title '::ncmpcpp::' -e ncmpcpp" },
    --["file"] =  { layout = layouts[7],        exclusive = false, solitary = false, popup = false, position = 4, spawn = filemanager } ,
    --["pics"] =  { layout = layouts[7],        exclusive = false, solitary = false, popup = true},
    --["pdf"] = { layout = layouts[7], popup = true, solitary = false, position = 5 },
    ---- ["media"] = { layout = layouts[1], position = 6} ,
    --["dict"] = { layout = layouts[7], position = 7, spawn = "stardict"} ,
    --["visit"] = { layout = layouts[1], position = 8, spawn = "visit", persist = true} ,
    --["email"] = { layout = layouts[7], position = 3, spawn = emailclient },
    --["gimp"] = { layout = layouts[1], icon_only=true, mwfact = 0.18, sweep_delay = 2, exclusive = true, icon="/usr/share/icons/hicolor/22x22/apps/gimp.png", screen = 1, },
    --["p2p"] = { layout = layouts[7], exclusive = true},
    --["office"] = { layout = layouts[7], exclusive = true},
    --["preview"] = { layout = layouts[7], exclusive = true},
    --["virtualbox"] = { layout = layouts[7], exclusive = true},
    --["wine"] = { layout = layouts[1], exclusive = true, persist = true},
    --["remote"] = { layout = layouts[1], exclusive = true}
--}
----}}}

--{{{ Tyrannical: configure tags
tyrannical.tags = {
    {
        name        = "term",
        init        = true,
        exclusive   = "true",
        layout      = awful.layout.suit.tile,
        class       = {
            "rxvt-unicode", "urxvt", "terminal", "URxvt", "konsole", "terminator", "finalterm", "Terminology",
            "gnome-terminal", "sakura"
        },
        position    = 1
    },
    {
        name        = "web",
        init        = false,
        exec_once   = browser,
        layout      = awful.layout.suit.max,
        volatile    = true,
        class       = {
            "Navigator", "Vimperator", "Firefox", "Chrome", "google-chrome", "chromium", "conkeror", "luakit"
        },
        position    = 2
    },
    {
        name        = "email",
        init        = false,
        exec_once   = emailclient,
        layout      = awful.layout.suit.max,
        volatile    = true,
        class       = {
            "Thunderbird", "Evolution", "mutt", "xfce4-terminal"
        },
        position    = 3
    },
    {
        name        = "file",
        init        = false,
        volatile    = true,
        position    = 4,
        exec_once   = filemanager,
        layout      = awful.layout.suit.max,
        class       = {
            "Dolphin", "pcmanfm", "Thunar", "Nautilus", "Rox-Filer"
        },
    },
    {
        name        = "pdf",
        init        = false,
        volatile    = true,
        position    = 5,
        layout      = awful.layout.suit.max,
        class       = {
            "Okular", "Evince", "xdvi"
        },
    },
    {
        name        = "media",
        init        = false,
        volatile    = true,
        position    = 6,
        layout      = awful.layout.suit.floating,
        --exec_once   = terminal .. " -title '::ncmpcpp::' -e ncmpcpp",
        class       = {
            "Mplayer", "Smplayer", "xine", "VLC", "easytag", "Aqualung", "xmms2", "wesnoth", "Osdlyrics", "Cantata"
        },
    },
    {
        name        = "emacs",
        init        = false,
        volatile    = true,
        position    = 7,
        layout      = awful.layout.suit.tile,
        exec_once   = "emacsclient -c",
        class       = {
            "Emacs", "emacsclient"
        },
    },
    {
        name        = "visit",
        init        = false,
        volatile    = true,
        position    = 8,
        layout      = awful.layout.suit.floating,
        --exec_once   = "visit",
        class       = {
            "VisIt", "GUI", "Viewer"
        },
    },
    {
        name        = "other",
        init        = false,
        volatile    = true,
        position    = 9,
        layout      = awful.layout.suit.floating,
        class       = {
            "Nsight", "XMathematica"
        },
        exclusive   = false,
    },
    {
        name        = "wine",
        init        = false,
        volatile    = false,
        layout      = awful.layout.suit.floating,
        class       = {
            "wine"
        },
    },
}
--}}}

-- {{{ Tyrannical Settings
-- Ignore the tag "exclusive" property for the following clients (matched by classes)
tyrannical.properties.intrusive = {
    "ksnapshot"     , "pinentry"       , "gtksu"     , "kcalc"        , "xcalc"               ,
    "feh"           , "Gradient editor", "About KDE" , "Paste Special", "Background color"    ,
    "kcolorchooser" , "plasmoidviewer" , "Xephyr"    , "kruler"       , "plasmaengineexplorer",
}

-- Ignore the tiled layout for the matching clients
tyrannical.properties.floating = {
    "MPlayer"      , "pinentry"        , "ksnapshot"  , "pinentry"     , "gtksu"          ,
    "xine"         , "feh"             , "kmix"       , "kcalc"        , "xcalc"          ,
    "yakuake"      , "Select Color$"   , "kruler"     , "kcolorchooser", "Paste Special"  ,
    "New Form"     , "Insert Picture"  , "kcharselect", "mythfrontend" , "plasmoidviewer" 
}

-- Make the matching clients (by classes) on top of the default layout
tyrannical.properties.ontop = {
    "Xephyr"       , "ksnapshot"       , "kruler"
}

-- Force the matching clients (by classes) to be centered on the screen on init
tyrannical.properties.centered = {
    "kcalc"
}

tyrannical.settings.block_children_focus_stealing = true --Block popups ()
tyrannical.settings.group_children = true --Force popups/dialogs to have the same tags as the parent client

-- }}}

----{{{ SHIFTY: application matching rules
---- order here matters, early rules will be applied first
--shifty.config.apps = {
         --{ match = { "emacs", "Nsight"  } , tag = "dev"    } ,
         --{ match = { "rxvt-.*","urxvt.*","terminal"  } , tag = "term"    } ,
         --{ match = { "kmess", "Kopete","Pidgin","emesene","amsn","Skype"      } , tag = "im", nopopup = true    } ,
         --{ match = { "Dolphin","pcmanfm","Rox-Filer","nautilus"         } , tag = "file"    } ,
         --{ match = { "Okular", "evince" } , tag = "pdf" } , 
         --{ match = { "eMule.exe"              } , tag = "p2p", nopopup = true    } ,
         --{ match = { "preview","xdvi"            } , tag = "preview"    } ,
         --{ match = { "VirtualBox"              } , tag = "virtualbox"    } ,
         --{ match = { "LibreOffice"              } , tag = "office"    } ,
         --{ match = { "VisIt", "GUI", "Viewer"              } , tag = "visit"    } ,
        --{ match = {"::ncmpcpp.*", }, tag = "media", },
        --{ match = {"::mutt.*", "Thunderbird", }, tag = "email", },
        --{ match = {"liferea", "RSSOwl", }, tag = "news", },
         ----{ match = { "Shredder.*","Thunderbird","mutt"                     } , tag = "mail"   } ,
         ----{ match = { "pcmanfm"                                             } , slave = true   } ,
         --{ match = { "Mplayer.*","Smplayer","xine","VLC.*","easytag","Aqualung", "grip", "xmms2", "wesnoth" } , tag = "media" } ,
         --{ match = { "MPlayer", "Smplayer", "Gimp", "Kopete", "mail_notification", "kmix","ossxmix", "gnome-volume-control"   } , float = true   } ,
         --{ match = { terminal                                              } , honorsizehints = false, slave = false   } ,
         --{ match = { "stardict", "EBView" }, tag = "dict" } ,
         --{ match = { "gwenview" }, tag = "pics" } ,
        --{ match = { "Yakuake", "yakuake" }, float = true },
        --{ match = { "Gimp","Ufraw" }, tag = "gimp", },
        --{ match = { "gimp.toolbox", }, slave = true , struts = { right=200 },
                                                        --geometry = {nil,35,200,733} },
        --{ match = { "gimp-image-window" }, slave = true },
        --{ match = { "Wine" }, tag = "wine" , slave = true },
         --{ match = { "Navigator","Vimperator","Firefox","Pentadactyl"              } , tag = "web"   },
         --{ match = { "uzbl" }, tag = "web", nopopup = true, slave = true},
         --{ match = { "luakit" }, tag = "web", nopopup = true, slave = true},
         --{ match = { "freerdp" } , tag = "remote" , slave = true }
--}
----}}}

----{{{ SHIFTY: default tag creation rules
---- parameter description
----  * floatBars : if floating clients should always have a titlebar
----  * guess_name : wether shifty should try and guess tag names when creating new (unconfigured) tags
----  * guess_position: as above, but for position parameter
----  * run : function to exec when shifty creates a new tag
----  * remember_index: ?
----  * all other parameters (e.g. layout, mwfact) follow awesome's tag API
--shifty.config.defaults = {
		--layout = awful.layout.suit.tile,
		--floatBars = true,
		--run = function(tag)
		--naughty.notify({ text = "Shifty Created "..
				 --(awful.tag.getproperty(tag,"position") or shifty.tag2index(mouse.screen,tag)).." : "..
				 --(tag.name or "foo")
				--})
		--end,
		--guess_name = true,
		--persist = false,
		--leave_kills = false,
		--exclusive = false,
		--guess_position = true,
		--remember_index = true,
		--ncol = 1,
		--floatBars=true,
		--mwfact = 0.5,
		--nopopup = true
		--}
----}}}

--{{{ Functions

---- {{{ Move tag to screen
function tag_to_screen(t, scr)
    local ts = t or awful.tag.selected()
    local screen_origin = ts.screen
    local screen_target = scr or awful.util.cycle(screen.count(), ts.screen + 1)

    awful.tag.history.restore(ts.screen,1)
    tag_move(ts, screen_target)

    -- never waste a screen
    if #(screen[screen_origin]:tags()) == 0 then
        for _, tag in pairs(screen[screen_target]:tags()) do
            if not tag.selected then
                tag_move(tag, screen_origin)
                tag.selected = true
                break
            end
        end
    end

    awful.tag.viewonly(ts)
    mouse.screen = ts.screen
    if #ts:clients() > 0 then
        local c = ts:clients()[1]
        client.focus = c
    end

end
----}}}

--[[
   [--{{{ Get mpd info
   [function get_mpd()
   [  local stats = mpc:send("status")
   [   if stats.errormsg then
   [    local mpd_text = "MPD not running? | "
   [   else
   [    if stats.state == "stop" then
   [  	 now_playing = " Stopped"
   [    else
   [      local zstats = mpc:send("playlistid " .. stats.songid)
   [  	  now_playing = ( zstats.artist or "NA" ) .. " - " .. (zstats.title or string.gsub(zstats.file, ".*/", "" ) )
   [    end
   [    -- Don't abuse the wibox, truncate
   [    if now_playing:len() > 50 then
   [       now_playing = now_playing:sub(1, 47) .. "..."
   [    end
   [
   [    if stats.state == "pause" then
   [     now_playing = "<span color='" .. beautiful.fg_mpd_paused .. "'>" .. awful.util.escape(now_playing) .. "</span>"
   [    else
   [     now_playing = awful.util.escape(now_playing)
   [    end
   [
   [    mpd_text = now_end
   [   playing
   [return mpd_text
   [end
   [--}}}
   [
   [--{{{ Get the album image
   [        function album_art()
   [                local stats = mpc:send("status")
   [                local zstats = mpc:send("playlistid " .. stats.songid)
   [                art = awful.util.pread("find '" .. musicdir .. awful.util.unescape(string.match(zstats.file, ".*/")) .. "' -regextype posix-egrep -iregex '.*(cover|front|albumart|outside|folder).*(png|jpg|gif)' | head -1")
   [
   [                return string.gsub(art,"\n","")
   [        end
   [--}}}
   [
   [--{{{ Get playlist
   [function get_playlist ()
   [        local stats = mpc:send("status")
   [        local cur = stats.song
   [        local list = ""
   [        if tonumber(stats.song) < 10 then
   [                min = tonumber(stats.song)
   [        else
   [                min = 10
   [        end
   [        for i = stats.song - min,stats.song + 6
   [                do
   [                zstats = mpc:send("playlistinfo " .. i)
   [                if zstats.pos == nil then
   [                        list = list .. "<big><b>::end::</b></big>"
   [                        break
   [                end
   [                if zstats.pos == stats.song then
   [                        list = list .. "<span color='" .. beautiful.fg_focus .. "'>" .. zstats.pos .. ". " .. awful.util.escape((zstats.artist or "NA") .. " - " .. (zstats.title or zstats.file)) .. "</span>\n"
   [                else
   [                        list = list .. zstats.pos .. ". " .. awful.util.escape((zstats.artist or "NA") .. " - " .. (zstats.title or zstats.file) ) .. "\n"
   [                end
   [        end
   [    return list
   [end
   [--}}}
   ]]

---- {{{ Get Netcfg info
--function get_netcfg ()
        --local cfg = io.popen("netcfg current"):read("*all")
        --stat = string.match(cfg, "(%w+)")
        --if stat == "ethernet" then
            --netcfgicon.image = image(beautiful.widget_wired)
        --elseif stat == "wireless" then
            --netcfgicon.image = image(beautiful.widget_wifi)
        --else
            --netcfgicon.image = image(theme.titlebar_close_button_normal)
        --end
--end
---- }}}

--{{{ run-once function

-- function run_once(prg)
--     if not prg then
--         return nil 
--     end
--     os.execute("x=" .. prg .. "; pgrep -u $USER -x " .. prg .. " || (" .. prg .. " &)")
-- end
function run_once(cmd)
  findme = cmd
  firstspace = cmd:find(" ")
  if firstspace then
     findme = cmd:sub(0, firstspace-1)
  end
  awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
end


--}}}

-- {{{ Org-Mode Agenda Functions
-- the current agenda popup
org_agenda_pupup = nil

-- do some highlighting and show the popup
function show_org_agenda ()
   local fd = io.open("/home/alex/.org/org-agenda.txt", "r")
   if not fd then
      return
   end
   local text = fd:read("*a")
   fd:close()
   -- get rid of unwanted characters
   text = text:gsub("&", "&amp;")
   text = text:gsub("'", "\'")
   -- highlight week agenda line
   text = text:gsub("(Week%-agenda[ ]+%(W%d%d[-W%d]*%):)", "<span color='lightblue'><b>%1</b></span>")
   -- highlight dates
   text = text:gsub("(%w+[ ]+%d%d? %w+ %d%d%d%d[^\n]*)", "<span color='cyan'>%1</span>")
   -- highlight times
   text = text:gsub("(%d%d?:%d%d)", "<span color='green'>%1</span>")
   -- highlight tags
   text = text:gsub("(:[^ ]+:)([ ]*\n)", "<span color='yellow'>%1%2</span>")
   -- highlight TODOs
   text = text:gsub("(TODO) ", "<b><span color='red'>%1</span></b> ")
   -- highlight STARTEDs
   text = text:gsub("(STARTED) ", "<b><span color='orange'>%1</span></b> ")
   -- highlight WAITINGs
   text = text:gsub("(WAITING) ", "<b><span color='yellow'>%1</span></b> ")
   -- highlight APPTs
   text = text:gsub("(APPT) ", "<b><span color='lightblue'>%1</span></b> ")
   -- highlight categories
   text = text:gsub("([ ]+%w+:) ", "<span color='lightgreen'>%1</span> ")
   org_agenda_pupup = naughty.notify(
      { text     = text,
        timeout  = 999999999,
        width    = 600,
        position = "bottom_right",
        screen   = mouse.screen })
end

-- dispose the popup
function dispose_org_agenda ()
   if org_agenda_pupup ~= nil then
      naughty.destroy(org_agenda_pupup)
      org_agenda_pupup = nil
   end
end

--}}}

--{{{ uzbl prompt
function uzbl_prompt(prompt, text, fifo, command)
  if command then
  -- if a command prefix is provided
      command = command .. ' '
  else
      command = ""
  end
  awful.prompt.run({prompt=prompt, text=text},
      mypromptbox[mouse.screen].widget,
      function(input)
          awful.util.spawn_with_shell(string.format("echo '%s%s' >%s", command, input, fifo))
      end,
      function(cmd, cur_pos, ncomp)
              -- get the url
              local urls = {}
              f = io.popen('awk \'{print $3}\' '.. os.getenv("HOME") ..  '/.local/share/uzbl/history | sort -u')
              for url in f:lines() do
                      table.insert(urls, url)
              end
              f:close()
              -- abort completion under certain circumstances
              if #cmd == 0 or (cur_pos ~= #cmd + 1 and cmd:sub(cur_pos, cur_pos) ~= " ") then
                      return cmd, cur_pos
              end
              -- match
              local matches = {}
              table.foreach(urls, function(x)
                      if urls[x]:find(cmd:sub(1,cur_pos)) then
                              table.insert(matches, urls[x])
                      end
              end)
              -- if there are no matches
              if #matches == 0 then
                      return
              end
              -- cycle
              while ncomp > #matches do
                      ncomp = ncomp - #matches
              end
              -- return match and position
              return matches[ncomp], cur_pos
      end) 
end
--}}}

--{{{ uzbl minimize cycle
function minimize_cycle(direction)
   local vistag = awful.tag.selected(mouse.screen)
   local clis = vistag.clients(vistag)
   local who = 0

   for i,cli in ipairs(clis) do
      if not cli.minimized then
         cli.minimized = true
         who=i
         break
      end
   end
   if direction==1 then
      clis[math.mod(who, #clis)+1].minimized=false
   elseif direction==-1 then
      clis[math.mod(who+#clis-2, #clis)+1].minimized=false
   end
end
--}}}

--{{{ Fetch ncmpcpp lyrics
function fetch_lyrics()
    f = io.popen('fetchlyrics.py'):read("*all")
    naughty.notify( { text = f, timeout = 10, position = "bottom_right" })
    --myinfo.text = f
end
--}}}

function clientfind (properties)
   local clients = client.get()
   local rv = nil
   for i, c in pairs(clients) do
      if match(properties, c) then
        rv = c
      end
   end
   return rv
end

-- Returns true if all pairs in table1 are present in table2
function match (table1, table2)
   for k, v in pairs(table1) do
      if table2[k] ~= v then
         return false
      end
   end
   return true
end

--{{{ Find tag with a certain position
function find_tyrannical_tag (pos, screen)
    local scr = screen or mouse.screen
    local tags = awful.tag.gettags(scr)
    local found = false
    for i, t in pairs(tags) do
        if awful.tag.getproperty(t, "position") == pos then
            found = true
            return t
        end
    end
   
    if not found then
        for name, t in pairs(tyrannical.tags_by_name) do
            if t.position == pos then
                awful.tag.add(t.name, t)
                awful.tag.move(pos, t)
                return t
            end
        end
    end
end
--}}}

--{{{ Move tag 1 index lower
function move_tag_prev ()
    --local idx = awful.tag.getidx(awful.tag.selected(mouse.screen))
    --local numtags = table.getn(awful.tag.gettags(mouse.screen))
    --if idx ~= 1 then
        --awful.tag.move(idx - 1)
    --else
        --awful.tag.move(numtags)
    --end
    awful.tag.move(awful.tag.getidx(awful.tag.selected(mouse.screen)) - 1)
end
--}}}

--{{{ Move tag 1 index higher
function move_tag_next ()
    --local idx = awful.tag.getidx(awful.tag.selected(mouse.screen))
    --local numtags = table.getn(awful.tag.gettags(mouse.screen))
    --if idx ~= numtags then
        --awful.tag.move(idx + 1)
    --else
        --awful.tag.move(1)
    --end
    awful.tag.move(awful.tag.getidx(awful.tag.selected(mouse.screen)) + 1)
end
--}}}

--}}}

-- {{{ Menu
require("freedesktop/freedesktop")
-- }}}

-- Old config
-- {{{ Wibox

--{{{ Wibox Configuration
-- {{{ Reusable separators
--spacer    = widget({ type = "textbox"  })
spacer    = wibox.widget.textbox()
--separator = widget({ type = "imagebox" })
separator = wibox.widget.imagebox()
spacer:set_text(" ")
separator:set_image(beautiful.widget_sep)
-- }}}

-- {{{ CPU usage
--cpuicon = widget({ type = "imagebox" })
cpuicon = wibox.widget.imagebox()
cpuicon:set_image(beautiful.widget_cpu)
-- Initialize widgets
cpugraph  = awful.widget.graph()
--cputhermal = wibox.widget.textbox()
--cputhermal2 = wibox.widget.textbox()
cputhermal = lain.widgets.temp({
    timeout = 13,
    settings = function()
        widget:set_text("" .. coretemp_now .. "°C")
    end
})
-- Graph properties
cpugraph:set_width(40)
cpugraph:set_height(17)
cpugraph:set_background_color(beautiful.fg_off_widget)
--cpugraph:set_color(beautiful.fg_end_widget)
--cpugraph:set_gradient_angle(0)
cpugraph:set_color({ type = "linear", from = { 0, 0 }, to = { 0, 17 }, stops = { { 0, beautiful.fg_end_widget }, { 0.5, beautiful.fg_center_widget }, { 1, beautiful.fg_widget } }})
--cpugraph:set_gradient_colors({ beautiful.fg_end_widget,
   --beautiful.fg_center_widget, beautiful.fg_widget
--}) 
-- Register widgets
vicious.register(cpugraph,  vicious.widgets.cpu,     "$1")
--vicious.register(cputhermal, vicious.widgets.thermal, "$1°C", 13, {"coretemp.0", "core"})
--vicious.register(cputhermal2, vicious.widgets.thermal, "$1°C", 13, {"coretemp.1", "core"})
-- }}}

-- {{{ Battery
--baticon = widget({ type = "imagebox" })
baticon = wibox.widget.imagebox()
baticon:set_image(beautiful.widget_bat)
batwidget = lain.widgets.bat({
    settings = function()
        if bat_now.perc == "N/A" then
            widget:set_markup("AC")
            baticon:set_image(beautiful.widget_ac)
            return
        elseif tonumber(bat_now.perc) <= 10 then
            --baticon:set_image(beautiful.widget_battery_empty)
            widget:set_markup("<span color='#ff4b4b'>" .. bat_now.perc .. "%</span>")
        elseif tonumber(bat_now.perc) <= 35 then
            --baticon:set_image(beautiful.widget_battery_low)
            widget:set_markup("<span color='#d79b1e'>" .. bat_now.perc .. "%</span>")
        else
            widget:set_markup("" .. bat_now.perc .. "%")
            baticon:set_image(beautiful.widget_bat)
        end
    end
})
-- Initialize widgets
--batwidget = widget({ type = "textbox" })
--batwidget = wibox.widget.textbox()
--battip = awful.tooltip({ objects = { batwidget }, })
--battip:set_text("text")
---- Register widget
--vicious.register(batwidget, vicious.widgets.bat, 
      --function (widget, args)
        --if args[2] == 100 and args[1] ~= "-" then
            --battip:set_text("Battery is full")
        --elseif args[1] == "+" then
            --local battime = awful.util.pread("acpi -b | awk '{print $5}'") 
            --battip:set_text("Battery Charging:\n " .. string.gsub(battime,"\n","") .. " to complete")
        --elseif args[1] == "-" then
            --battime = awful.util.pread("acpi -b | awk '{print $5}'") 
            --battip:set_text("Battery Discharging:\n " .. string.gsub(battime,"\n","") .. " remaining")
        --else
            --battip:set_text("Battery State is Unknown")
        --end
        --if  args[2] >= 75  then
            --return "<span color='#9acd32'>" .. args[2] .. "%</span>"
        --elseif args[2] >= 50 and args[2] < 75 then
            --return "<span color='#d79b1e'>" .. args[2] .. "%</span>"
        --elseif args[2] >= 20 and args[2] < 50 then
            --return "<span color='#ff4b4b'>" .. args[2] .. "%</span>"
        --elseif args[2] < 20 and args[1] == "-" then
            --naughty.notify({ title = "Battery Warning", text = "Battery low! "..args[2].."% left!\nBetter plug in the power cord.", timeout = 10, position = "top_right", fg = beautiful.fg_urgent, bg = beautiful.bg_urgent })
            --return "<span color='#ff4b4b'>" .. args[2] .. "%</span>"
        --elseif args[2] < 20 then
            --return "<span color='#ff4b4b'>" .. args[2] .. "%</span>"
        --else
            --return "<span color='#9acd32'>" .. args[2] .. "%</span>"
        --end
    --end, 31, "BAT0")
-- }}}

-- {{{ Memory usage
--memicon = widget({ type = "imagebox" })
memicon = wibox.widget.imagebox()
memicon:set_image(beautiful.widget_mem)
-- Initialize widget
membar = awful.widget.progressbar()
-- Pogressbar properties
membar:set_width(12)
membar:set_height(17)
membar:set_vertical(true)
membar:set_background_color(beautiful.fg_off_widget)
membar:set_border_color(beautiful.border_widget)
--membar:set_color(beautiful.fg_widget)
membar:set_color({ type = "linear", from = { 0, 0 }, to = { 0, 17 }, stops = { { 0, beautiful.fg_end_widget }, { 0.5, beautiful.fg_center_widget }, { 1, beautiful.fg_widget }} })
--membar:set_gradient_colors({ beautiful.fg_widget,
   --beautiful.fg_center_widget, beautiful.fg_end_widget
--}) 
-- Register widget
vicious.register(membar, vicious.widgets.mem, "$1", 13)
-- }}}

-- {{{ Network usage
--dnicon = widget({ type = "imagebox" })
dnicon = wibox.widget.imagebox()
--upicon = widget({ type = "imagebox" })
upicon = wibox.widget.imagebox()
dnicon:set_image(beautiful.widget_net)
upicon:set_image(beautiful.widget_netup)
-- Initialize widget
--netwidget = widget({ type = "textbox" })
netwidget = wibox.widget.textbox()
-- Register widget
vicious.register(netwidget, vicious.widgets.net, '<span color="'
--#3F3F3F">${wlan0 down_kb}</span> <span color="#7F9F7F">${wlan0 up_kb}</span>', 3)
  .. beautiful.fg_netdn_widget ..'">${wlp1s0 down_kb}</span> <span color="'
  .. beautiful.fg_netup_widget ..'">${wlp1s0 up_kb}</span>', 3)
-- }}}

---- {{{ Netcfg status
--netcfgicon = widget({ type = "imagebox" })
--netcfgicon.image = image(beautiful.widget_wifi)

--wifibar   = awful.widget.progressbar()
--wifibar:set_width(10)
--wifibar:set_height(20)
--wifibar:set_vertical(true)
--wifibar:set_max_value(0.7)
--wifibar:set_background_color(beautiful.fg_off_widget)
--wifibar:set_border_color(beautiful.border_widget)
--wifibar:set_color(beautiful.fg_widget)
--wifibar:set_gradient_colors({ beautiful.fg_widget,
   --beautiful.fg_widget, beautiful.fg_widget
--}) -- Enable caching
--vicious.enable_caching(vicious.widgets.wifi)

--vicious.register(wifibar, vicious.widgets.wifi, "${link}", 7, "wlan0")
---- }}}

---- {{{ Volume level
--volicon = widget({ type = "imagebox" })
--volicon.image = image(beautiful.widget_vol)
---- Initialize widgets
--volbar    = awful.widget.progressbar()
--volwidget = widget({ type = "textbox" })
---- Progressbar properties
--volbar:set_width(10)
--volbar:set_height(20)
--volbar:set_vertical(true)
--volbar:set_background_color(beautiful.fg_off_widget)
--volbar:set_border_color(beautiful.border_widget)
--volbar:set_color(beautiful.fg_widget)
--volbar:set_gradient_colors({ beautiful.fg_widget,
   --beautiful.fg_center_widget, beautiful.fg_end_widget
--}) -- Enable caching
--vicious.enable_caching(vicious.widgets.volume)
---- Register widgets
--vicious.register(volbar,    vicious.widgets.volume, "$1",  2, "PCM")
--vicious.register(volwidget, vicious.widgets.volume, "$1%", 2, "PCM")
---- Register buttons
--volbar.widget:buttons(awful.util.table.join(
   --awful.button({ }, 1, function () exec("kmix") end),
   --awful.button({ }, 3, function () exec("amixer -q sset Master toggle")   end),
   --awful.button({ }, 4, function () exec("amixer -q sset PCM 2dB+", false) end),
   --awful.button({ }, 5, function () exec("amixer -q sset PCM 2dB-", false) end)
--)) -- Register assigned buttons
--volwidget:buttons(volbar.widget:buttons())
---- }}}

-- {{{ Date and time
--dateicon = widget({ type = "imagebox" })
dateicon = wibox.widget.imagebox()
dateicon:set_image(beautiful.widget_date)
-- Initialize widget
--datewidget = widget({ type = "textbox" })
datewidget = wibox.widget.textbox()
-- Register widget
vicious.register(datewidget, vicious.widgets.date, "%b %d, %R", 61)
-- Register buttons
--datewidget:buttons(awful.util.table.join(
  --awful.button({ }, 1, function () exec("pylendar.py") end)
--))
calendar2.addCalendarToWidget(datewidget, "<span color='green'>%s</span>")
-- }}}

--{{{ MPD Widget

--mpdicon = widget({ type = "imagebox" })
mpdicon = wibox.widget.imagebox()
mpdicon:set_image(beautiful.widget_mpd)
---- Initialize widget
--mpdwidget = widget({ type = "textbox" })
---- Register widget
--vicious.register(mpdwidget, vicious.widgets.mpd,
    --function (widget, args)
        --if args[1] == 'Stopped' then return ''
        --else return 'MPD: ' .. args[1]
        --end
    --end, 5)

--}}}

-- {{{ MPD textbox
--mpdbox = widget({ type = "textbox", layout = awful.widget.layout.horizontal.leftright })
mpdbox = wibox.widget.textbox()
vicious.register(mpdbox, vicious.widgets.mpd,
    function (widget, args)
        if args["{state}"] == 'Stop' then return "<span color='"..beautiful.fg_mpd_paused.."'>Stopped</span>"
        else 
            now_playing = args["{Artist}"]..' - '..args["{Title}"]
            if now_playing:len() > 50 then
               now_playing = now_playing:sub(1, 47) .. "..."
            end
            if args["{state}"] == 'Pause' then return "<span color='"..beautiful.fg_mpd_paused.."'>" .. now_playing .. "</span>"
            else return now_playing
            end
        end
    end, 5)
--mpdbox:set_align("left")
-- }}}

-- {{{ Information widget
--myinfo = widget({ type = "textbox", layout = awful.widget.layout.horizontal.leftright })
myinfo = wibox.widget.textbox()
myinfo:set_align("left")
myinfo:set_text("")
-- }}}

--{{{ System Tray widget
-- Create a systray
--mysystray = widget({ type = "systray" })
mysystray = wibox.widget.systray()
--}}}

--{{{ Org-mode agenda
--orgicon = widget({ type = "imagebox" })
orgicon = wibox.widget.imagebox()
orgicon:set_image(beautiful.widget_org)
--Initialize widget
--orgwidget = widget({ type = "textbox" })
orgwidget = wibox.widget.textbox()
--Configure widget
local orgmode = {
  files = { home.."/.org/newgtd.org" 
  },
  color = {
    past   = '<span color="'..beautiful.fg_urgent..'">',
    today  = '<span color="'..beautiful.fg_normal..'">',
    soon   = '<span color="'..beautiful.fg_widget..'">',
    future = '<span color="'..beautiful.fg_netup_widget..'">'
}} --Register widget
vicious.register(orgwidget, vicious.widgets.org,
  orgmode.color.past..'$1</span>-'..orgmode.color.today .. '$2</span>-' ..
  orgmode.color.soon..'$3</span>-'..orgmode.color.future.. '$4</span>', 601,
  orgmode.files
) --Register buttons
orgwidget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec("emacsclient -c --eval '(org-agenda-list)'") end),
  awful.button({ }, 3, function () exec("emacsclient -c "..home.."/.org/newgtd.org " ..home.."/.org/someday.org" ) end)
))
--}}}

--}}}

--{{{ Wibox Initialization
-- Create a wibox for each screen and add it
mywibox = {}
mywibox2 = {}
mypromptbox = {}
--shiftyprompt = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewprev),
                    awful.button({ }, 5, awful.tag.viewnext)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    --mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    mypromptbox[s] = awful.widget.prompt()
    --shiftyprompt[s] = widget({ type = "textbox", layout = awful.widget.layout.horizontal.leftright })
    --shiftyprompt[s].text = ""
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    --mytasklist[s] = awful.widget.tasklist(function(c)
                                              --return awful.widget.tasklist.filter.currenttags(c, s)
                                          --end, mytasklist.buttons)
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s, height = 18 })
    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mytaglist[s])
    left_layout:add(mylayoutbox[s])
    left_layout:add(separator)
    left_layout:add(mypromptbox[s])
    left_layout:add(myinfo)

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(mpdicon)
    right_layout:add(mpdbox)
    right_layout:add(separator)
    right_layout:add(cpuicon)
    right_layout:add(cpugraph)
    --right_layout:add(separator)
    right_layout:add(cputhermal)
    --right_layout:add(separator)
    --right_layout:add(cputhermal2)
    right_layout:add(separator)
    right_layout:add(memicon)
    right_layout:add(membar)
    right_layout:add(separator)
    right_layout:add(baticon)
    right_layout:add(batwidget)
    right_layout:add(separator)
    right_layout:add(dnicon)
    right_layout:add(netwidget)
    right_layout:add(upicon)
    right_layout:add(separator)
    right_layout:add(orgicon)
    right_layout:add(orgwidget)
    right_layout:add(separator)
    right_layout:add(dateicon)
    right_layout:add(datewidget)
    right_layout:add(separator)
    if s == 1 then right_layout:add(mysystray) end
    --right_layout:add(membar.widget)
    --right_layout:add(cpugraph.widget)

    -- Now bring it all together 
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    --layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    -- Add widgets to the wibox
    mywibox[s]:set_widget(layout)

    mywibox2[s] = awful.wibox({ position = "bottom", screen = s, height = 18 })
    local left_layout2 = wibox.layout.fixed.horizontal()
    left_layout2:add(mylauncher)
    left_layout2:add(mytasklist[s])

    local layout2 = wibox.layout.align.horizontal()
    layout2:set_left(left_layout2)

    mywibox2[s]:set_widget(layout2)

    --mywibox2[s].widgets = {
        --mylauncher,
        --mytasklist[s],
        --layout = awful.widget.layout.horizontal.leftright
    --}

end
-- }}}

--}}}

--{{{ SHIFTY: initialize shifty
-- the assignment of shifty.taglist must always be after its actually initialized with awful.widget.taglist.new()
--shifty.taglist = mytaglist
--shifty.init()
--}}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))


-- Client bindings
clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}

-- {{{ Modal keys for mpd
mpd_mode = {
    h = function () os.execute("mpc prev") ; vicious.force({ mpdbox, }) end,
    x = function () os.execute("mpc toggle") ; vicious.force({ mpdbox, }) end,
    s = function () os.execute("mpc stop") ; vicious.force({ mpdbox, }) end,
    k = function () os.execute("mpc next") ; vicious.force({ mpdbox, }) end,
    l = function () os.execute("fetchlyrics.py") ; vicious.force({ mpdbox, }) end,
}
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    --awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    --awful.key({ modkey,           }, "Left",  awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  function() lain.util.tag_view_nonempty(1) end),
    awful.key({ modkey,           }, "Left",  function() lain.util.tag_view_nonempty(-1) end),
    --awful.key({ modkey, "Shift"   }, "Left",  shifty.shift_prev       ),
    awful.key({ modkey, "Shift"   }, "Left",    function() move_tag_prev() end),
    --awful.key({ modkey, "Shift"   }, "Left",  shifty:shift_prev(mouse.screen)       ),
    awful.key({ modkey, "Shift"   }, "Right",   function() move_tag_next() end),
    --awful.key({ modkey, "Control"   }, "Left",  shifty.send_prev       ),
    --awful.key({ modkey, "Control"   }, "Right",  shifty.send_next       ),
    awful.key({ modkey,           }, "`", awful.tag.history.restore),
    
    --awful.key({ modkey            }, "t",      function() shifty.add({ rel_index = 1, position = awful.tag.getproperty(awful.tag.selected(mouse.screen), "position")  }) end),
    --awful.key({ modkey, "Control" }, "t",      function() shifty.add({ rel_index = 1, nopopup = true, position = awful.tag.getproperty(awful.tag.selected(mouse.screen), "position") }) end),
    --awful.key({ modkey,           }, "n",      shifty.rename),
    -- awful.key({ modkey, "Shift"   }, "w",      shifty.del),
    awful.key({ modkey, "Control"   }, "c",      awful.tag.delete(awful.tag.selected(mouse.screen))),
    -- awful.key({ modkey, "Shift"   }, "o",      function() shifty.set(awful.tag.selected(mouse.screen), { screen = awful.util.cycle(screen.count() , mouse.screen + 1) }) end),
    awful.key({ modkey, "Shift"   }, "o",      function() tagger.movescreenleft(awful.tag.selected(mouse.screen)) end),
    
    
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "h",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    --awful.key({ modkey,           }, "w", function () mymainmenu:show(true)        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "h", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "h", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    -- awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey,           }, "F11", awesome.restart),
    awful.key({ modkey,           }, "F12", awesome.quit),
    awful.key({ modkey, "Shift"   }, "l",     function () exec("xscreensaver-command -lock")      end),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "j",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Control"   }, "j",     function () awful.client.incwfact( 0.05)      end),
    awful.key({ modkey, "Control"   }, "l",     function () awful.client.incwfact(-0.05)      end),
    --awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    --awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Brightness Control
    --awful.key({ }, "XF86MonBrightnessUp", function () exec("brightbar.sh up") end),
    --awful.key({ }, "XF86MonBrightnessDown", function () exec("brightbar.sh down") end),


    ---- MPD Bindings
    --awful.key({ modkey, }, "z", function () os.execute("mpc prev") ; vicious.force({ mpdbox, }) end),
    --awful.key({ modkey, }, "x", function () os.execute("mpc toggle") ; vicious.force({ mpdbox, }) end),
    --awful.key({ modkey, }, "c", function () os.execute("mpc stop") ; vicious.force({ mpdbox, }) end),
    --awful.key({ modkey, }, "v", function () os.execute("mpc next") ; vicious.force({ mpdbox, }) end),
    --awful.key({ modkey, }, "x", function () mpc:toggle_play() ; hook_mpd() end),
    --awful.key({ modkey, }, "c", function () mpc:stop() ; hook_mpd() end),
    --awful.key({ modkey, }, "v", function () mpc:next() ; hook_mpd() end),

    -- {{{ MPD modal bindings
     awful.key({ modkey }, "m", function(c)
         keygrabber.run(function(mod, key, event)
             if event == "release" then return true end
             keygrabber.stop()
             if mpd_mode[key] then mpd_mode[key]() end
             return true
         end)
     end),
    -- }}}

    -- Prompt
    --awful.key({ modkey }, "F2", function() awful.util.spawn("launch.sh") end),
    awful.key({ modkey }, "F2", function() awful.util.spawn("gmrun") end),

    awful.key({ modkey }, "F4",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),

    awful.key({ modkey, "Shift" }, "i", 
              function ()
                  local s = mouse.screen
                  if myinfo.text then
                    myinfo:set_text("")
                  elseif client.focus then
                      myinfo:set_text("")
                      if client.focus.class then
                        myinfo:set_text( "Class: " .. client.focus.class .. " ")
                      end
                      if client.focus.instance then
                        myinfo:set_text( myinfo.text .. "Instance: ".. client.focus.instance .. " ")
                      end
                      if client.focus.role then
                        myinfo:set_text( myinfo.text .. "Role: ".. client.focus.role )
                      end
                      if client.focus.type then
                        myinfo:set_text( myinfo.text .. "Type: ".. client.focus.type )
                      end
                  end
              end),
    
    awful.key({ modkey,          }, "z",      function () drop(scratch, "top", "center", 0.8, 0.3) end)

)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    -- awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey, "Shift"   }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({ modkey, "Shift"   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind keys 1234,qwe,asd to tags 1 to 9
keys = {}
keys[1] = "1"
keys[2] = "2"
keys[3] = "3"
keys[4] = "q"
keys[5] = "w"
keys[6] = "f"
keys[7] = "a"
keys[8] = "r"
keys[9] = "s"
-- Compute the maximum number of digit we need, limited to 9
for i=1,9 do
  
  globalkeys = awful.util.table.join(globalkeys, awful.key({ modkey }, keys[i],
  function ()
    --local t = awful.tag.viewonly(shifty.getpos(i))
    local t = find_tyrannical_tag(i)
    awful.tag.viewonly(find_tyrannical_tag(i))
  end))
  globalkeys = awful.util.table.join(globalkeys, awful.key({ modkey, "Control" }, keys[i],
  function ()
    --local t = shifty.getpos(i)
    local t = find_tyrannical_tag(i)
    t.selected = not t.selected
  end))
  globalkeys = awful.util.table.join(globalkeys, awful.key({ modkey, "Mod1" }, keys[i],
  function ()
    if client.focus then
      awful.client.toggletag(find_tyrannical_tag(i))
    end
  end))
  -- move clients to other tags
  globalkeys = awful.util.table.join(globalkeys, awful.key({ modkey, "Shift" }, keys[i],
    function ()
      if client.focus then
        local t = find_tyrannical_tag(i)
        awful.client.movetotag(t)
        awful.tag.viewonly(t)
      end
    end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
--shifty.config.globalkeys = globalkeys
--shifty.config.clientkeys = clientkeys

-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    { rule = { class = "yakuake" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

--netcfgtimer = timer { timeout = 7 }
--netcfgtimer:add_signal("timeout", function()
               --get_netcfg()
        --end)
--netcfgtimer:start()

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

--orgwidget:connect_signal("mouse::enter", show_org_agenda)
--orgwidget:connect_signal("mouse::leave", dispose_org_agenda)

--mpdbox.text = get_mpd()
-- }}}

-- {{{ Autostart applications
--
--run_once("xscreensaver")
--run_once("ck-launch-session nm_applet")
--run_once("gnome-power-manager")
os.execute(terminal .. " -e bash -c \"tmux -q has-session && exec tmux attach-session -d || exec tmux new-session -n$USER -s$USER@$HOSTNAME\" &")

-- }}}

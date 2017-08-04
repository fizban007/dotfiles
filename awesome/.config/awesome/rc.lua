
--[[
                                             
     Powerarrow Darker Awesome WM config 2.0 
     github.com/copycat-killer               
                                             
--]]

-- {{{ Required libraries
local gears         = require("gears")
local awful         = require("awful")
                      require("awful.autofocus")
                      require("awful.remote")
local wibox         = require("wibox")
local beautiful     = require("beautiful")
local naughty       = require("naughty")
local lain          = require("lain")
local menubar       = require("menubar")
local freedesktop   = require("freedesktop")
--local hotkeys_popup = require("awful.hotkeys_popup").widget
local tyrannical    = require("tyrannical")
local calendar2 = require("calendar2")
-- }}}

-- {{{ Error handling
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Autostart applications
local function run_once(cmd)
  findme = cmd
  firstspace = cmd:find(" ")
  if firstspace then
     findme = cmd:sub(0, firstspace-1)
  end
  awful.spawn.with_shell(string.format("pgrep -u $USER -x %s > /dev/null || (%s)", findme, cmd))
end

run_once("nm-applet --sm-disable")
run_once("xfce4-clipman")
run_once("kmix")
run_once("fcitx")
run_once("xfce4-power-manager")
--run_once("unclutter -root")
-- }}}

-- {{{ Variable definitions
-- beautiful init
beautiful.init(os.getenv("HOME") .. "/.config/awesome/themes/powerarrow-dark/theme.lua")

-- common
home       = os.getenv("HOME")
modkey     = "Mod4"
altkey     = "Mod1"
terminal   = "konsole" or "urxvtc"
editor     = os.getenv("EDITOR") or "emacsclient -t"

-- user defined
browser    = "firefox"
gui_editor = "emacsclient -c"
graphics   = "gimp"
filemanager = "dolphin"
--local tagnames   = { "1", "2", "3", "4", "5" }

-- Function aliases
local exec = awful.util.spawn
local sexec = awful.util.spawn_with_shell

-- table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
     awful.layout.suit.max,
     awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}

-- lain
lain.layout.termfair.nmaster        = 3
lain.layout.termfair.ncol           = 1
lain.layout.termfair.center.nmaster = 3
lain.layout.termfair.center.ncol    = 1
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

--{{{ Find tag with a certain position
function find_tyrannical_tag (pos, screen)
    local scr = screen or mouse.screen
    --local tags = awful.tag.gettags(scr)
    local tags = scr.tags
    local found = false
    for i, t in pairs(tags) do
        if t.position == pos then
            found = true
            return t
        end
    end
   
    if not found then
        for name, t in pairs(tyrannical.tags_by_name) do
            if t.position == pos then
                mytag = awful.tag.add(t.name, t)
                return mytag
            end
        end
    end
end
--}}}

-- {{{ Menu
myawesomemenu = {
    --{ "hotkeys", function() return false, hotkeys_popup.show_help end },
    { "manual", terminal .. " -e man awesome" },
    { "edit config", string.format("%s -e %s %s", terminal, editor, awesome.conffile) },
    { "restart", awesome.restart },
    { "quit", function() awesome.quit() end }
}
mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })
--mymainmenu = freedesktop.menu.build({
    --before = {
        --{ "Awesome", myawesomemenu, beautiful.awesome_icon },
        ---- other triads can be put here
    --},
    --after = {
        --{ "Open terminal", terminal },
        ---- other triads can be put here
    --}
--})
menubar.utils.terminal = terminal -- Set the Menubar terminal for applications that require it
-- }}}

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
            "navigator", "firefox", "Navigator", "Vimperator", "Firefox", "Chrome", "google-chrome",
            "chromium", "conkeror", "luakit"
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
        exec_once   = "visit",
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

-- {{{ Wibox
local markup = lain.util.markup
local separators = lain.util.separators

local clockicon = wibox.widget.imagebox(beautiful.widget_clock)
local mytextclock = wibox.widget.textclock(" %a %d %b  %H:%M")
--local clock = lain.widget.abase({
    --timeout  = 60,
    --cmd      = " date +'%a %d %b %R'",
    --settings = function()
        --widget:set_markup(" " .. output)
    --end
--})
calendar2.addCalendarToWidget(mytextclock, "<span color='green'>%s</span>")

-- calendar
--lain.widget.calendar.attach(mytextclock, {
    --notification_preset = {
        --font = "Consolas 10",
        --fg   = beautiful.fg_normal,
        --bg   = beautiful.bg_normal
    --} 
--})

-- Mail IMAP check
local mailicon = wibox.widget.imagebox(beautiful.widget_mail)
--[[ commented because it needs to be set before use
mailicon:buttons(awful.util.table.join(awful.button({ }, 1, function () awful.util.spawn(mail) end)))
local mail = lain.widget.imap({
    timeout  = 180,
    server   = "server",
    mail     = "mail",
    password = "keyring get mail",
    settings = function()
        if mailcount > 0 then
            widget:set_text(" " .. mailcount .. " ")
            mailicon:set_image(beautiful.widget_mail_on)
        else
            widget:set_text("")
            mailicon:set_image(beautiful.widget_mail)
        end
    end
})
--]]

-- MPD
local mpdicon = wibox.widget.imagebox(beautiful.widget_music)
--mpdicon:buttons(awful.util.table.join(awful.button({ }, 1, function () awful.util.spawn_with_shell(musicplr) end)))
local mpd = lain.widget.mpd({
    settings = function()
        if mpd_now.state == "play" then
            artist = " " .. mpd_now.artist .. " "
            title  = mpd_now.title  .. " "
            mpdicon:set_image(beautiful.widget_music_on)
        elseif mpd_now.state == "pause" then
            artist = " mpd "
            title  = "paused "
        else
            artist = ""
            title  = ""
            mpdicon:set_image(beautiful.widget_music)
        end

        widget:set_markup(markup("#EA6F81", artist) .. title)
    end
})

-- MEM
local memicon = wibox.widget.imagebox(beautiful.widget_mem)
local mem = lain.widget.mem({
    settings = function()
        widget:set_text(" " .. mem_now.used .. "MB ")
    end
})

-- CPU
local cpuicon = wibox.widget.imagebox(beautiful.widget_cpu)
local cpu = lain.widget.cpu({
    settings = function()
        widget:set_text(" " .. cpu_now.usage .. "% ")
    end
})

-- Coretemp
local tempicon = wibox.widget.imagebox(beautiful.widget_temp)
local temp = lain.widget.temp({
    tempfile = "/sys/class/thermal/thermal_zone8/temp",
    settings = function()
        widget:set_text(" " .. coretemp_now .. "°C ")
    end
})

-- / fs
--local fsicon = wibox.widget.imagebox(beautiful.widget_hdd)
--local fsroot = lain.widget.fs({
    --options  = "--exclude-type=tmpfs",
    --notification_preset = { fg = beautiful.fg_normal, bg = beautiful.bg_normal, font = "Terminus 10" },
    --settings = function()
        --widget:set_text(" " .. fs_now.used .. "% ")
    --end
--})

-- Battery
local baticon = wibox.widget.imagebox(beautiful.widget_battery)
local bat = lain.widget.bat({
    settings = function()
        if bat_now.status ~= "N/A" then
            if bat_now.ac_status == 1 then
                widget:set_markup(" AC ")
                baticon:set_image(beautiful.widget_ac)
                return
            elseif not bat_now.perc and tonumber(bat_now.perc) <= 5 then
                baticon:set_image(beautiful.widget_battery_empty)
            elseif not bat_now.perc and tonumber(bat_now.perc) <= 15 then
                baticon:set_image(beautiful.widget_battery_low)
            else
                baticon:set_image(beautiful.widget_battery)
            end
            widget:set_markup(" " .. bat_now.perc .. "% ")
        else
            baticon:set_image(beautiful.widget_ac)
        end
    end
})

-- ALSA volume
local volicon = wibox.widget.imagebox(beautiful.widget_vol)
local volume = lain.widget.alsa({
    cmd = "amixer -c 1",
    settings = function()
        if volume_now.status == "off" then
            volicon:set_image(beautiful.widget_vol_mute)
        elseif tonumber(volume_now.level) == 0 then
            volicon:set_image(beautiful.widget_vol_no)
        elseif tonumber(volume_now.level) <= 50 then
            volicon:set_image(beautiful.widget_vol_low)
        else
            volicon:set_image(beautiful.widget_vol)
        end

        widget:set_text(" " .. volume_now.level .. "% ")
    end
})
volume.widget:buttons(awful.util.table.join(
	awful.button({ }, 4, function()
          os.execute(string.format("amixer -c 1 set %s 1%%+", volume.channel))
          volume.update()
	end),
	awful.button({ }, 5, function()
          os.execute(string.format("amixer -c 1 set %s 1%%-", volume.channel))
          volume.update()
	end)
	))
volicon:buttons(awful.util.table.join(
        awful.button({ }, 1, function()
            os.execute(string.format("amixer -c 1 set %s toggle", volume.channel or volume.channel))
            volume.update()
        end)))


-- Net
local neticon = wibox.widget.imagebox(beautiful.widget_net)
neticon:buttons(awful.util.table.join(awful.button({ }, 1, function () awful.util.spawn_with_shell(iptraf-ng) end)))
local net = lain.widget.net({
    iface = "wlp2s0",
    notify = "off",
    settings = function()
        widget:set_markup(markup("#7AC82E", " " .. net_now.received)
                          .. " " ..
                          markup("#46A8C3", " " .. net_now.sent .. " "))
    end
})

-- Separators
local spr     = wibox.widget.textbox(' ')
local arrl_dl = separators.arrow_left(beautiful.bg_focus, "alpha")
local arrl_ld = separators.arrow_left("alpha", beautiful.bg_focus)

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))


-- Randomly set wallpaper
-- scan directory, and optionally filter outputs
function scandir(directory, filter)
    local i, t, popen = 0, {}, io.popen
    if not filter then
        filter = function(s) return true end
    end
    --print(filter)
    for filename in popen('ls -a "'..directory..'"'):lines() do
        if filter(filename) then
            i = i + 1
            t[i] = filename
        end
    end
    --print(#t)
    return t
end

-- configuration - edit to your liking
math.randomseed( os.time() )
math.random(); math.random(1, 20); math.random(1, 10)
wp_timeout  = 600
wp_path = home .. "/Downloads/Wallpapers/"
wp_filter = function(s) return string.match(s,"%.png$") or string.match(s,"%.jpg$") end
wp_files = scandir(wp_path, wp_filter)
wp_index = math.random(1, #wp_files)
print(wp_index)
 
local function set_wallpaper(s,wallpaper)
    -- Wallpaper
    if wallpaper then
        gears.wallpaper.fit(wallpaper, s)
    else
        wallpaper = wp_path .. wp_files[wp_index]
        gears.wallpaper.fit(wallpaper, s)
    end
    --if beautiful.wallpaper then
        -- If wallpaper is a function, call it with the screen
        --if type(wallpaper) == "function" then
            --wallpaper = wallpaper(s)
        --end
    --gears.wallpaper.maximized(wallpaper, s)
    --gears.wallpaper.centered(wallpaper, s, true)
    --end
end

local function randomize_wallpaper()
    wp_index = math.random(1, #wp_files)
    -- set wallpaper to current index for all screens
    for s = 1, screen.count() do
        --gears.wallpaper.maximized(wp_path .. wp_files[wp_index], s, true)
        set_wallpaper(s)
    end
end

-- setup the timer
--wp_timer = gears.timer { timeout = wp_timeout }
--wp_timer:connect_signal("timeout", function()
    --randomize_wallpaper()
 
    ---- stop the timer (we don't need multiple instances running at the same time)
    --wp_timer:stop()

    ----restart the timer
    --wp_timer.timeout = wp_timeout
    --wp_timer:start()
--end)
 
-- initial start when rc.lua is first run
--wp_timer:start()

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Quake application
    s.quake = lain.util.quake({ app = terminal })

    -- Wallpaper
    set_wallpaper(s, home .. "/wallpaper")

    -- Tags
    --awful.tag(tagnames, s, awful.layout.layouts)

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, height = 18 })
    s.mywibox2 = awful.wibar({ position = "bottom", screen = s, height = 18})

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            spr,
            s.mytaglist,
            s.mypromptbox,
            spr,
        },
        --s.mytasklist, -- Middle widget
        nil,
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            arrl_ld,
            wibox.container.background(mpdicon, beautiful.bg_focus),
            wibox.container.background(mpd.widget, beautiful.bg_focus),
            arrl_dl,
            volicon,
            volume.widget,
            arrl_ld,
            --wibox.container.background(mailicon, beautiful.bg_focus),
            --wibox.container.background(mail.widget, beautiful.bg_focus),
            --arrl_dl,
            wibox.container.background(memicon, beautiful.bg_focus),
            wibox.container.background(mem.widget, beautiful.bg_focus),
            --mem.widget,
            --arrl_ld,
            arrl_dl,
            --wibox.container.background(cpuicon, beautiful.bg_focus),
            --wibox.container.background(cpu.widget, beautiful.bg_focus),
            cpuicon,
            cpu.widget,
            arrl_ld,
            wibox.container.background(tempicon, beautiful.bg_focus),
            wibox.container.background(temp.widget, beautiful.bg_focus),
            --tempicon,
            --temp.widget,
            arrl_dl,
            --wibox.container.background(fsicon, beautiful.bg_focus),
            --wibox.container.background(fsroot.widget, beautiful.bg_focus),
            --arrl_dl,
            baticon,
            bat.widget,
            arrl_ld,
            wibox.container.background(neticon, beautiful.bg_focus),
            wibox.container.background(net.widget, beautiful.bg_focus),
            arrl_dl,
            --clock.widget,
	    mytextclock,
            spr,
            arrl_ld,
            wibox.container.background(s.mylayoutbox, beautiful.bg_focus),
	    spr,
            wibox.widget.systray(),
        },
    }

    s.mywibox2:setup {
        layout = wibox.layout.align.horizontal,
        nil,
        s.mytasklist,
        nil,
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Modal keys for mpd
mpd_mode = {
    h = function () os.execute("mpc prev") ; mpd.update() end,
    x = function () os.execute("mpc toggle") ; mpd.update() end,
    s = function () os.execute("mpc stop") ; mpd.update() end,
    k = function () os.execute("mpc next") ; mpd.update() end,
    l = function () os.execute("fetchlyrics.py") ; mpd.update() end,
}
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    -- Take a screenshot
    -- https://github.com/copycat-killer/dots/blob/master/bin/screenshot
    --awful.key({ altkey }, "p", function() os.execute("screenshot") end),

    -- Hotkeys
    --awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              --{description="show help", group="awesome"}),
    -- Tag browsing
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "`", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    -- Non-empty tag browsing
    -- awful.key({ altkey }, "Left", function () lain.util.tag_view_nonempty(-1) end,
              -- {description = "view  previous nonempty", group = "tag"}),
    -- awful.key({ altkey }, "Right", function () lain.util.tag_view_nonempty(1) end,
              -- {description = "view  previous nonempty", group = "tag"}),

    -- Default client focus
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "h",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),

    -- By direction client focus
    awful.key({ altkey }, "k",
        function()
            awful.client.focus.bydirection("down")
            if client.focus then client.focus:raise() end
        end),
    awful.key({ altkey }, "h",
        function()
            awful.client.focus.bydirection("up")
            if client.focus then client.focus:raise() end
        end),
    --awful.key({ modkey }, "h",
        --function()
            --awful.client.focus.bydirection("left")
            --if client.focus then client.focus:raise() end
        --end),
    --awful.key({ modkey }, "l",
        --function()
            --awful.client.focus.bydirection("right")
            --if client.focus then client.focus:raise() end
        --end),
    --awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              --{description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "h", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "h", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Show/Hide Wibox
    awful.key({ modkey }, "b", function ()
        for s in screen do
            s.mywibox.visible = not s.mywibox.visible
            s.mywibox2.visible = not s.mywibox2.visible
        end
    end),

    -- On the fly useless gaps change
    awful.key({ modkey, "Control" }, "+", function () lain.util.useless_gaps_resize(1) end),
    awful.key({ modkey, "Control" }, "-", function () lain.util.useless_gaps_resize(-1) end),

    -- Dynamic tagging
    --awful.key({ modkey, "Shift" }, "n", function () lain.util.add_tag() end),
    --awful.key({ modkey, "Shift" }, "r", function () lain.util.rename_tag() end),
    awful.key({ modkey, "Shift" }, "Left", function () lain.util.move_tag(-1) end),   -- move to next tag
    awful.key({ modkey, "Shift" }, "Right", function () lain.util.move_tag(1) end), -- move to previous tag
    awful.key({ modkey, "Shift" }, "d", function () lain.util.delete_tag() end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey,           }, "F9", function() set_wallpaper(s, home .. "/wallpaper") end,
              {description = "load default wallpaper", group = "awesome"}),
    awful.key({ modkey,           }, "F10", randomize_wallpaper,
              {description = "load a random wallpaper", group = "awesome"}),
    awful.key({ modkey,           }, "F11", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey,           }, "F12", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ altkey, "Shift"   }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ altkey, "Shift"   }, "j",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "j",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "j",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Dropdown application
    awful.key({ modkey, }, "z", function () awful.screen.focused().quake:toggle() end),

    -- Widgets popups
    awful.key({ altkey, }, "c", function () lain.widget.calendar.show(7) end),
    --awful.key({ altkey, }, "h", function () fsroot.show(7) end),

    -- ALSA volume control
    --awful.key({ altkey }, "Up",
        --function ()
            --os.execute(string.format("amixer set %s 1%%+", volume.channel))
            --volume.update()
        --end),
    --awful.key({ altkey }, "Down",
        --function ()
            --os.execute(string.format("amixer set %s 1%%-", volume.channel))
            --volume.update()
        --end),
    --awful.key({ altkey }, "m",
        --function ()
            --os.execute(string.format("amixer set %s toggle", volume.togglechannel or volume.channel))
            --volume.update()
        --end),
    --awful.key({ altkey, "Control" }, "m",
        --function ()
            --os.execute(string.format("amixer set %s 100%%", volume.channel))
            --volume.update()
        --end),

		--awful.key({ altkey, "Control" }, "0",
				--function ()
						--os.execute(string.format("amixer -q set %s 0%%", volume.channel))
						--volume.update()
				--end),

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

    -- Copy primary to clipboard
    --awful.key({ modkey }, "c", function () os.execute("xsel | xsel -b") end),

    -- User programs
    --awful.key({ modkey }, "q", function () awful.spawn(browser) end),
    --awful.key({ modkey }, "e", function () awful.spawn(gui_editor) end),
    --awful.key({ modkey }, "g", function () awful.spawn(graphics) end),

    -- Default
    -- Prompt
    awful.key({ modkey }, "F2", function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "F4",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"})
    -- Menubar
    --awful.key({ modkey }, "p", function() menubar.show() end,
              --{description = "show the menubar", group = "launcher"})
    --]]

    --[[ dmenu
    awful.key({ modkey }, "x", function ()
        awful.spawn(string.format("dmenu_run -i -fn 'Tamsyn' -nb '%s' -nf '%s' -sb '%s' -sf '%s'",
        beautiful.bg_normal, beautiful.fg_normal, beautiful.bg_focus, beautiful.fg_focus))
		end)
    --]]
)

clientkeys = awful.util.table.join(
    awful.key({ modkey, "Control"   }, "m",      lain.util.magnify_client                         ),
    awful.key({ modkey, "Shift"   }, "b",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey, "Control" }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    --awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              --{description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey, "Shift" }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
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
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, keys[i],
                  function ()
                        local screen = awful.screen.focused()
                        --local tag = screen.tags[i]
                        local tag = find_tyrannical_tag(i, screen)
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, keys[i],
                  function ()
                      local screen = awful.screen.focused()
                      --local tag = screen.tags[i]
                      local tag = find_tyrannical_tag(i, screen)
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, keys[i],
                  function ()
                      if client.focus then
                          --local tag = client.focus.screen.tags[i]
                          local tag = find_tyrannical_tag(i, client.focus.screen)
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" },  keys[i],
                  function ()
                      if client.focus then
                          --local tag = client.focus.screen.tags[i]
                          local tag = find_tyrannical_tag(i, client.focus.screen)
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                     size_hints_honor = false
     }
    },

    -- Titlebars
    { rule_any = { type = { "dialog", "normal" } },
      properties = { titlebars_enabled = false } },

    { rule = { class = "Gimp", role = "gimp-image-window" },
          properties = { maximized_horizontal = true,
                         maximized_vertical = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c, {size = 16}) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

-- No border for maximized clients
client.connect_signal("focus",
    function(c)
        if c.maximized then -- no borders if only 1 client visible
            c.border_width = 0
        elseif #awful.screen.focused().clients > 1 then
            c.border_width = beautiful.border_width
            c.border_color = beautiful.border_focus
        end
    end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

Pianobar.el [![MELPA](http://melpa.org/packages/pianobar-badge.svg)](http://melpa.org/#/pianobar)
===========

This is a thin emacs interface for Pianobar, a Pandora Radio
<http://pandora.com/> command-line client. Pianobar was written by
Lars-Dominik Braun, among others, and can be found at
<http://6xq.net/html/00/17.html>. `pianobar.el` itself can be found at
<http://github.com/agrif/pianobar.el/>.

It is licensed under the GNU GPL v3. See `LICENSE` for details.

There are brief installation instructions at the top of
`pianobar.el`. After installing `pianobar.el`, see below for
information about usage.

Please note that this is a *very* early development version, written
mostly for myself to use. Suggestions, improvements, and bug reports
are welcome.

Usage
=====

Pianobar is started with `M-x pianobar`, or you can bind a key to the
`pianobar` function manually. `pianobar.el` will put the current
playing song in the modeline. Once Pianobar is running, you can type
commands manually in the `*pianobar*` buffer (like using shell-mode),
or you can use the following, key-bindable commands:

M-x pianobar-love-current-song
	-- tell Pandora you love this song

M-x pianobar-ban-current-song
	-- tell Pandora to ban this song from this station

M-x pianobar-next-song
	-- skip to the next song (but don't ban/love)

M-x pianobar-play-or-pause
	-- toggle play/pause state

M-x pianobar-change-station
	-- bring up the `*pianobar*` buffer with a station-change menu

Customization
=============

`pianobar.el` requires Pianobar to be installed somewhere in your
path. If it's not, you can set the variable `pianobar-command` to be
the path to the Pianobar executable.

(As of right now, `pianobar.el` expects Pianobar to use the default
key bindings. It works even if you change them, but the control
commands won't work. You'll just have to switch to the `*pianobar*`
buffer and control it manually.)

You can set `pianobar-username`, `pianobar-password`, and
`pianobar-station` to automatically login and connect to a
station. Note that `pianobar-station` must be a *string* containing
the number used to select the station you want through Pianobar.

If you leave any of these unset, you will be prompted for the info
instead.

If your pianobar config contains your username and password, you can
set `(setq pianobar-config t)`.

If you set `pianobar-run-in-background` to `t`, `M-x pianobar` will
*not* show the `*pianobar*` buffer after running. This is useful if
you set the above auto-login variables.

`pianobar-global-modeline` will eventually be used to decide whether
to change the modeline on all buffers, or just the `*pianobar*`
buffer. Right now, it does nothing; it always changes the global
modeline.

There are many other, lesser-used variables available to customize
Pianobar. Use `C-h v` and take a look at all the variables prefixed
with `pianobar-`.

If you want to write your own Pianobar commands (especially for those
not yet bound to an interactive function), you can use
`pianobar-send-command`, which accepts a character to send to the
current Pianobar process. It also takes a second, optional argument:
whether to make the Pianobar buffer active. For example,
`M-x pianobar-next-song` runs `(pianobar-send-command ?n)`, while
`M-x pianobar-change-station` runs `(pianobar-send-command ?s t)`.

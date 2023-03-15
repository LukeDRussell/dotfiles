local wezterm = require 'wezterm';

return {
  color_scheme = "Solarized Dark - Patched",
  font = wezterm.font("Hack Nerd Font"),
  font_size = 13,
  default_prog = {"/usr/bin/tmux", "new", "-A"},
}

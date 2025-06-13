local wezterm = require("wezterm")

return {
	check_for_updates = false,
	color_scheme = "Gruvbox Material (Gogh)",
	font = wezterm.font("Mononoki", { weight = "Medium" }),
	font_size = 14,
    front_end = "WebGpu",
	enable_wayland = false,
	enable_tab_bar = false,
	enable_scroll_bar = false,
	window_decorations = "RESIZE",
	window_padding = {
		left = 0,
		right = 0,
		top = 0,
		bottom = 0,
	},
	send_composed_key_when_left_alt_is_pressed = false,
	send_composed_key_when_right_alt_is_pressed = true,
}

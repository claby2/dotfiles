DOTFILES=.
TARGETS= vimrc       \
		 bat         \
		 bspwm       \
		 kitty       \
		 polybar     \
		 ripgrep     \
		 rofi        \
		 spotify-tui \
		 sxhkd       \
		 zathura     \

# Replace original file $(1) with new file $(2) with prompt
define set_file
	if [ ! -f "$(1)" ]; then \
		echo "$(1) does not exist and will be created"; \
		mkdir -p "`dirname $(1)`" && touch "$(1)"; \
	fi \

	if cmp -s "$(1)" "$(2)"; then \
		echo "ALREADY EQUAL: $(1) is already equal to $(2)"; \
	else \
		diff -q "$(1)" "$(2)"; \
		read -p "Are you sure (y/n)? " REPLY; \
		if [ $$REPLY = Y ] || [ $$REPLY = y ]; then \
			cp -f "$(2)" "$(1)"; \
			echo "Copied!"; \
		else \
			echo "Cancelling..."; \
		fi \
	fi
endef

all: $(TARGETS)

vimrc:
	echo "vimrc:"
	$(call set_file,$(HOME)/.vimrc,$(DOTFILES)/vimrc)
	echo ""

bat:
	echo "bat:"
	$(call set_file,$(HOME)/.config/bat/bat.conf,$(DOTFILES)/config/bat/bat.conf)
	echo ""

bspwm:
	echo "bspwm:"
	$(call set_file,$(HOME)/.config/bspwm/bspwmrc,$(DOTFILES)/config/bspwm/bspwmrc)
	echo ""

kitty:
	echo "kitty:"
	$(call set_file,$(HOME)/.config/kitty/kitty.conf,$(DOTFILES)/config/kitty/kitty.conf)
	$(call set_file,$(HOME)/.config/kitty/theme.conf,$(DOTFILES)/config/kitty/theme.conf)
	echo ""

polybar:
	echo "polybar:"
	$(call set_file,$(HOME)/.config/polybar/config,$(DOTFILES)/config/polybar/config)
	$(call set_file,$(HOME)/.config/polybar/launch.sh,$(DOTFILES)/config/polybar/launch.sh)
	echo ""

ripgrep:
	echo "ripgrep:"
	$(call set_file,$(HOME)/.config/ripgrep/config,$(DOTFILES)/config/ripgrep/config)
	echo ""

rofi:
	echo "rofi:"
	$(call set_file,$(HOME)/.config/rofi/config,$(DOTFILES)/config/rofi/config)

spotify-tui:
	echo "spotify-tui:"
	$(call set_file,$(HOME)/.config/spotify-tui/config.yml,$(DOTFILES)/config/spotify-tui/config.yml)
	echo ""

sxhkd:
	echo "sxhkd:"
	$(call set_file,$(HOME)/.config/sxhkd/sxhkdrc,$(DOTFILES)/config/sxhkd/sxhkdrc)
	echo ""

zathura:
	echo "zathura:"
	$(call set_file,$(HOME)/.config/zathura/zathurarc,$(DOTFILES)/config/zathura/zathurarc)
	echo ""

.PHONY: $(TARGETS)
.SILENT: $(TARGETS)
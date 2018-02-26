A minor-mode menu for the mode line
===================================

This package implements a menu that lists enabled minor-modes, as
well as commonly but not currently enabled minor-modes.  It can be
used to toggle local and global minor-modes, to access mode-specific
menus, and to get help about modes.

This menu is intended as a replacement for the incomplete yet wide
list of enabled minor-modes that is displayed in the mode line by
default.  To use the menu like this, enable Minions mode.

Alternatively the menu can be bound globally, for example:
`(global-set-key [S-down-mouse-3] 'minions-minor-modes-menu)`.

![screenshot](https://emacsair.me/assets/readme/minions.png)

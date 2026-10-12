# Workflow
All tools, software, scripts, and configuration files I use on my computer.

## Scripts
Custom utility scripts live in [`bin/`](bin/) and are symlinked into `~/.bin` by the
install script. Notable entries:

- **`arch-backup`** – refreshes the pacman package list into `arch-packages/pkglist.txt`
- **`update-dotfiles`** – pulls the latest changes in this repo (run periodically via the `dotfiles.service`/`dotfiles.timer` systemd units)
- **`switcheroo`** – toggles swapping Caps Lock with left Ctrl (supports both Hyprland and X11/keyd)
- **`morbin`** – switches graphics to hybrid mode with optimus-manager
- **`getip4` / `getip6`** – print the public IPv4/IPv6 address
- **`volumeup-notify` / `volumedown-notify` / `toggle-mute-notify` / `toggle-mic-notify`** – volume and microphone controls with dunst notifications
- **`brightnessup-notify` / `brightnessdown-notify`** – brightness controls with dunst notifications
- **`change_screen`** – switches display output via xrandr
- **`emacscliententry` / `emacsclientfocus` / `emacsfocus`** – helpers for launching and focusing GNU Emacs
- **`clean_downloads`** – deletes files older than 30 days from `~/Downloads`
- **`interview_entry`** – Python CLI for interview notes
- **`create_season` / `org_season` / `remove_sdh` / `smash_mods`** – Org-mode/Ruby helpers for season workflows

Other script collections:

- [`systemd/`](systemd/) – user units: `emacs.service` (Emacs daemon) and `dotfiles.timer` (auto-update)
- [`canonical/`](canonical/) – setup script and `concierge.yaml` for the Canonical VM profile
- [`pokecat/`](pokecat/) – Pokémon-related helper script

## Dotfiles
Application configuration lives in [`dotfiles/`](dotfiles/) and is symlinked into
`$HOME`/`~/.config` by the install script:

| File / directory | Destination |
| --- | --- |
| `bashrc`, `blerc`, `blerc2` | `~/.bashrc`, `~/.blerc`, `~/.blerc2` |
| `zshrc` | `~/.zshrc` |
| `init.el` | `~/.emacs.d/init.el` |
| `kitty.conf` | `~/.config/kitty/kitty.conf` |
| `hyprland/` | `~/.config/hypr/` |
| `waybar/` | `~/.config/waybar/` |
| `kanshi/` | `~/.config/kanshi/` |
| `keyd/` | keyd key remapping config |
| `polybar/` | `~/.config/polybar/` (status bar, launch/power-menu scripts) |
| `dunstrc` | `~/.config/dunst/dunstrc` |
| `xprofile` | `~/.xprofile` |
| `Xresources` | `~/.Xresources` |
| `userChrome.css` | Firefox `chrome/userChrome.css` |
| `opencode/` | `~/.config/opencode/` |
| `pi/` | `~/.pi/agent/` |

Additional top-level configuration:

- [`config/`](config/) – X11 touchpad config and Firefox preferences
- [`arch-packages/`](arch-packages/) – pacman/AUR package lists for Arch installs
- [`requirements/`](requirements/) – extra requirement files (e.g. Wayland)
- [`requirements.txt`](requirements.txt) – package prerequisites for apt/snap/pipx installs

## Installation
The [`install.sh`](install.sh) script symlinks everything into place. Run it with one
of the following profiles:

```bash
./install.sh          # full profile (default)
./install.sh full     # full desktop setup: shell, Emacs, Hyprland, waybar, polybar,
                      # dunst, Firefox, systemd units, micromamba, i3 fallback
./install.sh vm       # minimal VM setup: bash, Emacs, opencode, pi, scripts, systemd
./install.sh can      # VM setup + Canonical-specific provisioning (canonical/setup.sh)
./install.sh fn <function>...  # run only the named install functions,
                               # e.g. ./install.sh fn symlink_pi symlink_dunst
```

The installer first installs prerequisites from `requirements.txt` (via apt, snap, or
pipx), then sets up Bleach Lite (`install_blesh`) and Atuin (`install_atuin`), and
finally symlinks the dotfiles and scripts. Each step prompts for confirmation unless
`AUTOMATE` is set (the profiles set it automatically).

On Arch-based systems, packages can be restored from
[`arch-packages/pkglist.txt`](arch-packages/pkglist.txt) and
[`arch-packages/pkglist-AUR.txt`](arch-packages/pkglist-AUR.txt):

```bash
pacman -S --needed - < arch-packages/pkglist.txt
```

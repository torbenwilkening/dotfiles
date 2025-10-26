
#!/bin/sh

git pull

ln -sf $(pwd)/.config/aerospace /home/$(whoami)/.config/aerospace
ln -sf $(pwd)/.config/alacritty /home/$(whoami)/.config/alacritty
ln -sf $(pwd)/.config/anyrun /home/$(whoami)/.config/anyrun
ln -sf $(pwd)/.config/hypr /home/$(whoami)/.config/hypr
ln -sf $(pwd)/.config/mako /home/$(whoami)/.config/mako
ln -sf $(pwd)/.config/waybar /home/$(whoami)/.config/waybar






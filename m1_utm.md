# M1 Mac UTM (QEMU wrapper) Notes

Notes for using virtual machines via [UTM](https://mac.getutm.app/)
([Github](https://github.com/utmapp/UTM)) on M1 macOS machines.

## Useful links

- [Wiki](https://github.com/utmapp/UTM/wiki)
- [Install Ubuntu ARM64 on Apple M1](https://github.com/utmapp/UTM/wiki/Install%20Ubuntu%20ARM64%20on%20Apple%20M1)
- [Linux VM tips](https://github.com/utmapp/UTM/wiki/Useful-tips#linux)
- [OpenVPN 3 Linux](https://community.openvpn.net/openvpn/wiki/OpenVPN3Linux)

## VM settings

For some reason, modifying the VM settings as follows reduces the
frequency of slowdown in the VM after switching away and back from a
UTM session.

> Set CPU Cores in System > Show Advanced Settings (e.g. on an M1 Pro with 4
> performance cores, this can be set to 4)

> Enable Force Multicore in System > Show Advanced Settings

In the VM, disable monitor sleep (usually in Settings > Power > Blank Screen)
by setting it to "Never".

## Enabling retina resolution for Macbooks

Install `spice-vdagent` and `spice-webdavd`:

```sh
sudo apt install spice-vdagent spice-webdavd
```

Make the following modifications to the VM settings.

> Edit > Display > Resolution > Enable "Fit to screen" and
> "Retina mode"

Check the screen resolution for the system.

> Apple > About this Mac > Displays

Create the following script somewhere (for example, this can be
`$HOME/.local/bin/set-res.sh` assuming `$HOME/.local/bin` is in
`$PATH`) where it can be called easily from the command-line.

```sh
#!/bin/sh

# Script to set resolution for current screen

# For a Linux VM running under a MacBook Pro 16 2021 (M1 Pro), which
# has resolution 3456x2234@60Hz, full-screen the VM window and run
#
#   $ set-res.sh 3456 2234 60
#
# When running under UTM, make sure the SPICE tools are installed
#
#   $ sudo apt install spice-vdagent spice-webdavd
#
# and "Fit to screen" and "Retina mode" are enabled if using UTM
#
# Note that cvt is used to get the required mode parameters:
#
#   $ cvt 3456 2234 60
#   3456x2234 59.98 Hz (CVT) hsync: 138.80 kHz; pclk: 664.00 MHz
#   Modeline "3456x2234_60.00"  664.00  3456 3744 4120 4784  2234 2237 2247 2314 -hsync +vsync

RETINA=${RETINA:-1}  # Default to HiDPI

if test "$#" -lt 2 || test "$#" -gt 3;
then
    echo "Usage: $0 XRES YRES [REFRESHRATE]"
    exit 1
fi

output=$(xrandr | grep -e " connected [^(]" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/")
moderaw=$(cvt "$@" | tail -1 | cut -d" " --complement -f1)
modename=$(echo $moderaw | cut -d" " -f1 | sed -e 's/^"//' | sed -e 's/"$//')
modeparams=$(echo $moderaw | cut -d" " --complement -f1)

if $(xrandr -q -d "$DISPLAY" | grep -q "$modename");
then
    echo "Modename $modename exists. Not adding a new entry."
else
    xrandr --newmode "$modename" $modeparams
    xrandr --addmode "$output" "$modename"
fi

if test "$RETINA" -gt 0;
then
    export GDK_SCALE=2
    export GDK_DPI_SCALE=0.5
fi

xrandr --output "$output" --mode "$modename"
```

To make setting resolution even easier, create
`$HOME/Desktop/set-fullscreen-res.desktop` with the following contents
(modifying the `Exec` parameter so the correct absolute path is used
and the numbers reflect the fullscreen resolution of the machine).

```desktop
[Desktop Entry]
Version=1.0
Name=Fullscreen Res
Exec=/PATH/TO/set-res.sh 3456 2234
Terminal=false
Type=Application
Comment=Set full-screen resolution for HiDPI machine
```

Right-click the desktop file located on the user interface desktop and
select `Allow Launching`. This allows for a simpler workflow for
setting fullscreen Retina resolution---login to the VM, fullscreen the
window, and double-click this desktop file (which should appear on the
desktop with the description corresponding to the `Name` entry).

### Deprecated

Create the following script in the VM, say `mbp16-retina.sh` for a
16-inch Macbook Pro (2021 model), and make it executable. The
resolution in the script (`3456x2234`) is for a 16-inch Macbook Pro,
and should be modified as appropriate for the host device.

```sh
#!/bin/sh

# Script to set resolution for a Linux VM running under a MacBook Pro 16 2021 (M1 Pro)
# which has resolution 3456x2234@60Hz

# When running under UTM, make sure the SPICE tools are installed
# > sudo apt install spice-vdagent spice-webdavd
# and "Fit to screen" and "Retina mode" are enabled if using UTM

# > cvt 3456 2234 60
# 3456x2234 59.98 Hz (CVT) hsync: 138.80 kHz; pclk: 664.00 MHz
# Modeline "3456x2234_60.00"  664.00  3456 3744 4120 4784  2234 2237 2247 2314 -hsync +vsync

modename="3456x2234_60.00"
xrandr --newmode $modename 664.00 3456 3744 4120 4784 2234 2237 2247 2314 -hsync +vsync

currentdisplay=$(xrandr | grep -e " connected [^(]" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/")
xrandr --addmode "$currentdisplay" "$modename"

export GDK_SCALE=2
export GDK_DPI_SCALE=0.5

xrandr --output Virtual-1 --mode "3456x2234_60.00"
```

Expand the UTM session window to full-screen mode and run the script.

## Shared drives

Make the following modifications to the VM settings.

> Check "Enable Directory Sharing" in Edit > Sharing > Shared Directory

Attach a shared directory to the VM in the UTM admin window.

In the running VM session, the shared directory is available as a
WebDAV service at `http://localhost:9843` with no credentials needed.

This can be attached as a network drive in GNOME Files. Once the
network drive is attached, it is accessible either through a GNOME
application or the filesystem at some mounted directory within
`${XDG_RUNTIME}/gvfs/` (`$XDG_RUNTIME` is usually `/run/user/<USERID>`
and the mounted directory name is something like
`dav+sd:host=Spice%2520client%2520folder._webdav._tcp.local`).

Alternatively, it can be mounted to a directory using instructions
listed
[here](https://github.com/utmapp/UTM/wiki/Useful-tips#mounting-webdav-shares)
(note that for an M1 machine, install the `davfs2` package rather than
the `dav2fs` package).

## Find IP address

Previously, this could be done via the `ifconfig` command but that has
been deprecated and now `ip` should be used instead (see
[link](https://ubuntu.com/blog/if-youre-still-using-ifconfig-youre-living-in-the-past)).

```sh
ip addr
```

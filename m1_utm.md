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
# has resolution 3456x2234@60Hz with the notch and 3456x2160 without the notch
# (in a full-screen VM, since that does not include the notch, without
# adjusting the resolution run "xrandr -q" to see the current resolution and
# double it), full-screen the VM window and run
#
#   $ set-res.sh 3456 2160 60
#
# When running under UTM, make sure the SPICE tools are installed
#
#   $ sudo apt install spice-vdagent spice-webdavd
#
# and "Fit to screen" and "Retina mode" are enabled if using UTM
#
# Note that cvt is used to get the required mode parameters:
#
#   $ cvt 3456 2160 60
#   3456x2160 59.99 Hz (CVT 7.46MA) hsync: 134.20 kHz; pclk: 642.00 MHz
#   Modeline "3456x2160_60.00"  642.00  3456 3744 4120 4784  2160 2163 2169 2237 -hsync +vsync

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
Name=Fullscreen HiDPI
Exec=/PATH/TO/set-res.sh 3456 2160
Terminal=false
Type=Application
Comment=Set full-screen resolution for HiDPI machine
```

Right-click the desktop file located on the user interface desktop and
select `Allow Launching`. This allows for a simpler workflow for
setting fullscreen Retina resolution---login to the VM, fullscreen the
window, and double-click this desktop file (which should appear on the
desktop with the description corresponding to the `Name` entry).

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

## Minimal software list

- [Chromium](https://snapcraft.io/chromium)
- [DBeaver](https://dbeaver.io/download/) (zip without Java included)
- [Emacs](https://snapcraft.io/emacs)
- [Miniforge](https://github.com/conda-forge/miniforge) (Mambaforge)
- [OpenJDK](https://openjdk.java.net/) (APT package `openjdk-17-jre`)

---
title: Arch Linux on MacBook Air 2013
break: on MacBook
date: 2016-11-01
synopsis: How I set up Arch Linux with Full-Disk Encryption alongside macOS on my MacBook Air.
---

This post summarizes how I install and dual-boot Arch Linux with
Full-Disk Encryption alongside macOS. It is not meant to be a
replacement for the [Installation Guide][installation] or the
former [Beginner's Guide][beginners]. Rather, it mostly serves as a
small summary with a few useful notes about the gotchas.

[installation]: https://wiki.archlinux.org/index.php/installation_guide
[beginners]: https://csdietz.github.io/arch-beginner-guide/

So, make sure you understand what you type into your terminal. If you
don't, checking out the Arch wiki should probably be your first step.

_Note:_ you will need internet access throughout the installation and
the MacBook Air's WiFi doesn't work out of the box on Arch Linux. I
recommend using your phone's USB Tethering (if it does support it), or
using an Ethernet-USB adapter.

## Shrinking the macOS partition

The first step I take is resizing the HFS+ macOS partition to make
room for the new <abbr>GNU/Linux</abbr> installation. There are plenty
of tutorials on how to do this using macOS's Disk Utility, so do that
and then come back!

## Creating a bootable Arch Linux Installer USB

There are different ways of creating a bootable Arch Linux USB, all
documented on the [USB flash installation media][usb_install] page on
the Arch wiki, but the simplest one is using `dd` if you already have
access to another UNIX system.

[usb_install]: https://wiki.archlinux.org/index.php/USB_flash_installation_media

<span class="red">Warning:</span> make sure you backup the data on
your flash drive, as `dd` will irrevocably destroy all data on it.

Use `lsblk` to find the name (block device) of your USB drive, then
run `dd` (as root) as shown below:

``` bash
dd bs=4M if=/path/to/archlinux.iso of=/dev/sdx status=progress && sync
```

Replace `/path/to/archlinux.iso` with the path to the Arch image you
have downloaded, and `/dev/sdx` with your drive.

## Booting up from the USB

After creating the install USB, reboot your laptop and hold the alt key and boot
into the USB.

When booting is complete and you're presented with the prompt, it's a good time
to make sure you're connected to the internet (see the _note_ at the top of this
post).

Use `ping` to verify that you've established a connection:

```bash
ping archlinux.org
```

## Updating the system clock

Once you're connected to the internet, make sure the system clock is accurate:

```bash
timedatectl set-ntp true  # start and enable systemd-timesyncd
```

You can check the service status using `timedatectl status`.

## Partitioning

I won't dive into partitioning and instead, I'll refer you to
the [Partitioning][partitioning] page of Arch wiki. Of the available
partitioning tools, I personally prefer `cfdisk`.

[partitioning]: https://wiki.archlinux.org/index.php/Partitioning

## Setting up LVM & LUKS

I use a [LVM on LUKS][lvm_on_luks] setup, where I set up LVM on top of
the encrypted partition.

First, let's set up the underlying encrypted partition:

``` bash
cryptsetup -v --cipher aes-xts-plain64 --key-size 512 --hash sha512 \
           --iter-time 5000 --use-urandom -y luksFormat /dev/sdaX
```

where `/dev/sdaX` is the partition you created in the last step
(e.g. `/dev/sda4`). For more information about the `cryptsetup`
options, see the [LUKS encryption options][luks_options].

[lvm_on_luks]: https://wiki.archlinux.org/index.php/Dm-crypt/Encrypting_an_entire_system#LVM_on_LUKS
[luks_options]: https://wiki.archlinux.org/index.php/Dm-crypt/Device_encryption#Encryption_options_for_LUKS_mode

Then we open the container:

``` bash
cryptsetup open --type luks /dev/sdaX lvm
```

Now it's time to use lvm and prepare the logical volume(s):

``` bash
pvcreate /dev/mapper/lvm
vgcreate vg /dev/mapper/lvm
lvcreate --extents +100%FREE -n root vg
```

This will create a physical volume on the mapping we just opened,
create a volume group named `vg` on the physical volume, and create a
logical volume named `root` that spans the entire volume group. More
complex setups are possible thanks to the great flexibility of lvm.

We now format the logical volume with `ext4`:

``` bash
mkfs.ext4 /dev/mapper/vg-root
```

## Installing the base system

Let's mount the logical volume, make a directory for the mount point
of the boot partition, and mount the boot partition (`/dev/sda1`):

``` bash
mount /dev/mapper/vg-root /mnt
mkdir /mnt/boot
mount /dev/sda1 /mnt/boot
```

Finally, let's install the base system (and optionally `base-devel`):

``` bash
pacstrap /mnt base base-devel
```

## Configuring the system

Let's generate the fstab:

``` bash
genfstab -U /mnt >> /mnt/etc/fstab
```

Use your favorite terminal-based editor, edit the fstab file and add
the `discard` option for the root partition to enable TRIM on the
SSD.

Now we change root into our newly installed system and will configure
it. Adjust these according to your own setup.

``` bash
arch-chroot /mnt /bin/bash
passwd  # set the root password
echo myhostname > /etc/hostname  # set the hostname
ln -s /usr/share/zoneinfo/Canada/Eastern /etc/localtime  # time zone
hwclock --systohc --utc   # write system clock to hardware clock (UTC)
useradd -m -G wheel -s /bin/bash myuser  # create myuser
passwd myuser  # set the password for myuser
echo "myuser ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers.d/myuser
# uncomment en_US.UTF-8 UTF-8 and other needed locales in /etc/locale.gen
locale-gen
echo LANG=en_US.UTF-8 > /etc/locale.conf
export LANG=en_US.UTF-8
```

Then adjust the initramfs hooks in `/etc/mkinitcpio.conf` and enable
the `encrypt` and `lvm2` hooks, and make sure `keyboard` is available
before `encrypt` so you can actually type in the LUKS password when
booting. Your `HOOKS` line should look similar to this:

```
HOOKS="base udev autodetect modconf block keyboard encrypt lvm2 filesystems fsck"
```

After adjusting the hooks, build the initramfs:

``` bash
mkinitcpio -p linux
```

Now, install the `intel-ucode` package. We'll configure the bootloader
to enable intel microcode updates.

``` bash
pacman -S intel-ucode
```

Create the `/boot/loader/loader.conf` with the following content
(adjust the timeout to your liking):

```
default arch
timeout 3
```

Then create the entry for Arch:

``` bash
mkdir -p /boot/loader/entries
touch /boot/loader/entries/arch.conf
```

Now edit `/boot/loader/entries/arch.conf` to specify the Arch entry:

```
title    Arch Linux
linux    /vmlinuz-linux
initrd   /intel-ucode.img
initrd   /initramfs-linux.img
options  cryptdevice=/dev/sdaX:vg:allow-discards root=/dev/mapper/vg-root rw
```

Again, `/dev/sdaX` is the partition you created in the
**Partitioning** step as the underlying encrypted partition.

Finally, install the bootloader, exit the chroot, umount and reboot!

``` bash
bootctl install
exit
umount -R /mnt
reboot
```

## Post-installation recommendations

Congratulations! You now have a minimal Arch installation.

At this point, I usually install my favorite AUR
helper, [pacaur][pacaur], then I
install the [broadcom-wl-dkms][broadcom-wl-dkms] wireless driver
and [mba6x_bl-dkms][mba6x_bl-dkms] backlight driver to fix the post
suspend/resume issue where three's no brightness after waking up from
suspend, and the only available brightness would be 100%.

[broadcom-wl-dkms]: https://aur.archlinux.org/packages/broadcom-wl-dkms/
[mba6x_bl-dkms]: https://aur.archlinux.org/packages/mba6x_bl-dkms/

``` bash
pacaur -S linux-headers dkms  # linux-headers is required for dkms
pacaur -S broadcom-wl-dkms
pacaur -S mba6x_bl-dkms
```

[pacaur]: https://aur.archlinux.org/packages/pacaur/

Then, I'd like to install

- input, graphics, and sound drivers,
- a desktop environment (I prefer Xfce or LXQt),
- a display manager for login screen (lightdm or sddm), and
- a network manager (NetworkManager or ConnMan).

Check out the [General recommendations][gen_reqs] for more details.

[gen_reqs]: https://wiki.archlinux.org/index.php/General_recommendations

## References

Here are some resources I've come across each with lots of useful bits
and pieces, about installing Arch on a MacBook:

- [pandeiro/arch-on-air](https://github.com/pandeiro/arch-on-air)
- [Arch Linux on MacBook Pro Retina 2014 with DM-Crypt, LVM and suspend to disk](https://loicpefferkorn.net/2015/01/arch-linux-on-macbook-pro-retina-2014-with-dm-crypt-lvm-and-suspend-to-disk/)
- [Installing Archlinux on Macbook Air 2013](http://frankshin.com/installing-archlinux-on-macbook-air-2013/)
- [Arch Linux Installation with OS X on Macbook Air (Dual Boot)](http://panks.me/posts/2013/06/arch-linux-installation-with-os-x-on-macbook-air-dual-boot/)
- [Installing (encrypted) Arch Linux on an Apple MacBook Pro](https://visual-assault.org/2016/03/05/install-encrypted-arch-linux-on-apple-macbook-pro/)
- [Installing Arch Linux on a MacBook Air 2013](http://alexeyzabelin.com/arch-on-mac)
- [Arch Linux running on my MacBook](https://medium.com/phils-thought-bubble-of-recent-stuff/arch-linux-running-on-my-macbook-2ea525ebefe3)
- [Dual boot Arch Linux on MacBook Pro Installation](http://codylittlewood.com/arch-linux-on-macbook-pro-installation/)

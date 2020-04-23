# Garmin Setup
This readme covers setting up your computer to automatically *rsync* fit files from your Garmin to the computer.

Run the following command and then plug in your Garmin watch.
```
$ udevadm monitor --kernel --property --subsystem-match=usb
```
You should see the following:
```
KERNEL[4720.669445] add      /devices/pci0000:00/0000:00:14.0/usb1/1-1 (usb)
ACTION=add
BUSNUM=001
DEVNAME=/dev/bus/usb/001/020
DEVNUM=020
DEVPATH=/devices/pci0000:00/0000:00:14.0/usb1/1-1
DEVTYPE=usb_device
MAJOR=189
MINOR=19
PRODUCT=91e/2a2c/509
SEQNUM=7573
SUBSYSTEM=usb
TYPE=0/0/0
```

Run the following commands replacing `<PRODUCT>` with the value from the udeavadm monitor command.

```
$ echo "ACTION==\"add\", SUBSYSTEM==\"usb\", ENV{PRODUCT}==\"<PRODUCT>\", RUN+=\"/usr/bin/python3 /usr/local/bin/backup.py\"" > /etc/udev/rules.d/
$ sudo cp /path/to/LifeOfMatt/scripts/garmin/garmin_rule.py /usr/local/bin/
$ sudo udevadm control --reload
```

Next we need to add an fstab rule for the garmin so that it gets mounted properly.

```
$ sudo blkid # To find UUID
$ sudo mkdir /mnt/GARMIN
$ sudo echo "UUID=<UUID>  /mnt/GARMIN vfat  defaults 0 2" > /etc/fstab
```


## Future Work

1. Trigger the full fit file processing pipeline.
2. Figure out why the script needs to be located in `/usr/local/bin/`
3. Automate the setup process.

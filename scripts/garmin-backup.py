#!/usr/bin/python3.4
from pathlib import Path
import subprocess
import time

LIFE_OF_MATT = Path('/home/matt/projects/LifeOfMatt')


def copy_activities():
    for fi in Path('/media/GARMIN/Garmin/Activity/').iterdir():
        target = LIFE_OF_MATT / 'logs' / 'fit'
        res = subprocess.check_output(['cp', str(fi), str(target)])
        outfile.write(res.decode('utf8') + '\n')


time.sleep(1)

outfile = Path('/tmp/garmin-backup.txt').open('w')
outfile.write('Hi Matt: ' + str(time.time()) + '\n')

try:
    res = subprocess.check_output(['blkid'],
                                  stderr=subprocess.STDOUT,
                                  shell=True).decode('utf8').split('\n')

    for line in res:
        if 'GARMIN' in line:
            dev_location = line.split(':')[0]

    res = subprocess.check_output(['sudo', 'mount', dev_location, '/media/GARMIN'])
    outfile.write(res.decode('utf8') + '\n')

    copy_activities()

except Exception as ex:
    outfile.write(str(ex))

# Always try to unmount the GARMIN
res = subprocess.check_output(['sudo', 'umount', '/media/GARMIN'])
outfile.write(res.decode('utf8') + '\n')

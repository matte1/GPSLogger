#!/usr/bin/python3

from pathlib import Path
import subprocess
import time

class Mounter():
  """Generic class for sending emails."""
  def __init__(self, dev, mnt, logfile):
    """Start a smt server using a local smtp-relay on port 25."""
    self.dev = dev
    self.mnt = mnt
    self.logfile = logfile

  def __enter__(self):
    """Start a smt server using a local smtp-relay on port 25."""
    assert self._attemp_to_mount(), "Failed to mount {self.dev} to {self.mnt}!"
    return self

  def _attemp_to_mount(self):
    '''todo'''
    time.sleep(5)
    proc = subprocess.run(['sudo', 'mount', self.dev, self.mnt])
    if proc.returncode == 0:
      return True
    self.logfile.write(f'Failed to mount {self.dev} to {self.mnt}!\n{proc.stderr}')
    return False

  def __exit__(self, exc_type, exc_value, traceback):
    """Close the smtp server."""
    subprocess.run(['sudo', 'umount', self.mnt])

def rsync():
  local = Path('/home/matt/projects/LifeOfMatt/data/garmin/fit/')
  assert local.exists(), f'The LifeOfMatt directory doesn\'t at {LIFE_OF_MATT}.'

  error_file = local / Path(time.strftime('rsyncs/%Y_%m_%d-%H:%M:%S.rsync'))
  # TODO: Convert this to an email client!
  with error_file.open('w') as logfile:
    with Mounter('/dev/sda', '/mnt/GARMIN', logfile):

      # Check that the garmin device has been mounted correctly.
      garmin = Path('/mnt/GARMIN/Garmin/Activity/')
      if not garmin.exists():
        logfile.write(f'The garmin device was not mounted correctly!\n')
        exit(1)

      # Rsync activites to local machine.
      proc = subprocess.run(['rsync', '-av', str(garmin) + '/', str(local) + '/'])
      if proc.returncode != 0:
        logfile.write(f'Failed to rsync from garmin!\n')
        logfile.write(f'{proc.stderr}')
        exit(1)

      logfile.write(f'Success!\n')

if __name__ == '__main__':
  rsync()
  exit(0)

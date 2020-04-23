#!/usr/bin/python3

from pathlib import Path
import subprocess
import time

def _wait_for_garmin(outfile):
  for i in range(60):
    time.sleep(1)
    # Reload mount to make sure that the Garmin is mounted.
    proc = subprocess.run(["sudo", "mount", "-av"], capture_output=True)
    if proc.returncode == 0:
      return
    outfile.write(f"Attempting to mount... {i}\n")
  outfile.write(f"Unable to mount the garmin device!!\n")
  exit(1)

if __name__ == '__main__':

  local = Path('/home/matt/projects/LifeOfMatt/data/garmin/fit/')
  assert local.exists(), f"The LifeOfMatt directory doesn't at {LIFE_OF_MATT}."

  error_file = local / Path(time.strftime("rsyncs/%Y_%m_%d-%H:%M:%S.rsync"))
  # TODO: Convert this to an email client!
  with error_file.open("w") as outfile:

    # Attempt to mount the garmin over a full minute.
    _wait_for_garmin(outfile)

    # Check that the garmin device has been mounted correctly.
    garmin = Path('/mnt/GARMIN/Garmin/Activity/')
    if not garmin.exists():
      outfile.write(f"The garmin device was not mounted correctly!\n")
      exit(1)

    # Rsync activites to local machine.
    proc = subprocess.run(["rsync", "-av", str(garmin) + "/", str(local) + "/"], capture_output=True)
    if proc.returncode != 0:
      outfile.write(f"Failed to rsync from garmin!\n")
      outfile.write(f"{proc.stderr}")
      exit(1)

    outfile.write(f"Success!")
    outfile.write(f"{proc.stdout}")

  # Attempt to unmount the garmin.
  subprocess.run(["sudo", "umount", "/mnt/GARMIN/"], capture_output=True)
  exit(0)

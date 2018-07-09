# coding=utf8

import exifread
import os
import sys
import math
import argparse
import time
import datetime
import json
import simplekml
import shutil
import subprocess
from pprint import pprint
import re
from PIL import Image

from GeoImage import GeoImage

from pathlib import Path


def fucking_apple(images):
    pass

def parse_geojson(file):
    coordinates = []
    with file.open() as infile:
        data = json.load(infile)
        for d in data['features'][0]['geometry']['coordinates']:
            coordinates.extend(d)
    coordinates.sort(key=lambda x: x[3])
    return coordinates


def create_tracks(kml, tracks):
    for track in tracks:
        if len(track) > 0:
            coords = [(coord[0], coord[1]) for coord in track if len(coord) == 4]
            trk = kml.newgxtrack(name=time.strftime('%Y:%m:%d %H:%M:%S', time.localtime(track[0][3])))
            trk.extrude = 1
            trk.newgxcoord(coords)
            trk.stylemap.normalstyle.iconstyle.icon.href = 'http://earth.google.com/images/kml-icons/track-directional/track-0.png'
            trk.stylemap.normalstyle.linestyle.color = "ffffff00"
            trk.stylemap.normalstyle.linestyle.width = 6
            trk.stylemap.highlightstyle.iconstyle.icon.href = 'http://earth.google.com/images/kml-icons/track-directional/track-0.png'
            trk.stylemap.highlightstyle.iconstyle.scale = 1.2
            trk.stylemap.highlightstyle.linestyle.color = "ffffff00"
            trk.stylemap.highlightstyle.linestyle.width = 12


def correct_image_rotations(dir, geotagged_imgs):
    # Make a new directory to hold rotated images.
    tmp_dir = os.path.join(dir, 'rotated_images')

    if os.path.exists(tmp_dir):
        shutil.rmtree(tmp_dir)
    os.mkdir(tmp_dir)

    # Use exiftool to correct rotation
    for img in geotagged_imgs:
        new_path = os.path.join(tmp_dir, img.name)
        try:
          subprocess.check_output(["exiftran", "-a", img.path, "-o", new_path])
        except Exception as ex:
          print(ex)
        img.path = new_path


def sort_geotagged_images(geotagged_imgs):
    imgs = []
    for img in geotagged_imgs:
        imgs.append([img.epoch, img])

    imgs.sort(key=lambda x: x[0])
    for idx, img in enumerate(imgs):
        img[1].idx = idx

# TODO: HANDLE HEIC TIMESTAMPING ISSUE
if __name__ == '__main__':
    aparser = argparse.ArgumentParser()
    aparser.add_argument('--folder', help='directory with KML information',
                         type=str, required=True)
    args = aparser.parse_args()

    folder = Path(args.folder)
    coords = [parse_geojson(fi) for fi in folder.iterdir()
              if fi.suffix == '.geojson']

    # Read exif data from images
    geotagged_imgs = []
    imgs = [fi for fi in folder.iterdir() if fi.suffix == '.jpg']

    for img in imgs:
        with img.open('rb') as infile:
            tags = exifread.process_file(infile)
            geotagged_imgs.append(GeoImage(tags, str(img), sum(coords, [])))

    # Create KML and add tracks to it from geojson
    kml = simplekml.Kml()
    create_tracks(kml, coords)
    correct_image_rotations(args.folder, geotagged_imgs)
    sort_geotagged_images(geotagged_imgs)

    # Add images to KMZ and save it
    for geotagged in geotagged_imgs:
        if geotagged.latitude is not None:
            path = kml.addfile(geotagged.path)
            pnt = kml.newpoint()
            pnt.coords = [(geotagged.longitude, geotagged.latitude)]
            pnt.name = str(geotagged.idx)
            pnt.description = '<img src="{:s}" alt="picture" width="{:d}" height="{:d}"/>'.format(path, geotagged.width, geotagged.height)
        else:
            print('Unknown Lat/long for {:s}'.format(geotagged.path))

    # kml.savekmz(os.path.join(args.dir, args.dir.split('/')[-1] + ".kmz"), format=False)  # Saving as KMZ
    kml.savekmz(str(folder / (folder.name + ".kmz")), format=False)  # Saving as KMZ

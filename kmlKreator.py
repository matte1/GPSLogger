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

def dms2dec(dms_str):
    """Return decimal representation of DMS """

    dms_str = re.sub(r'\s', '', dms_str)

    sign = -1 if re.search('[swSW]', dms_str) else 1

    numbers = list(filter(len, re.split('\D+', dms_str, maxsplit=4)))

    degree = numbers[0]
    minute = numbers[1] if len(numbers) >= 2 else '0'
    second = numbers[2] if len(numbers) >= 3 else '0'
    frac_seconds = numbers[3] if len(numbers) >= 4 else '0'

    second += "." + frac_seconds
    return sign * (int(degree) + float(minute) / 60 + float(second) / 3600)


class GeoImage(object):
    """GeoImage class for holding information on geotagged images"""
    def __init__(self, exif_tags, path, coords):
        self.path      = path
        self.tags      = exif_tags
        self.name      = path.split('/')[-1]
        self.gps_found = False
        self.lon_ref   = None
        self.lat_ref   = None
        self.bearing   = None
        self.altitude  = None
        self.longitude = None
        self.latitude  = None
        self.idx       = None

        # Get image width and height
        img = Image.open(self.path)
        self.width = 800
        self.height = int((float(img.size[1]) * float((self.width / float(img.size[0])))))

        self._get_time()
        if 'GPS GPSLatitude' and 'GPS GPSLongitude' in self.tags:
            self._latlon_from_exif()
        else:
            self._latlon_from_coordinates(coords)

    # TODO: Find way to remove dependency on last modified time if exif not
    # present
    def _get_time(self):
        '''
        Get both unix and datetime information for the image. Sometimes the
        images come without any exif data so we need to read the last modified
        date of the image which is potentially hazardaous.
        '''
        if 'EXIF DateTimeOriginal' in self.tags.keys():
            self.date_time = str(tags['EXIF DateTimeOriginal'])
            tokens = [int(t) for t in self.date_time.replace(' ', ':').split(':')]
            self.epoch = datetime.datetime(tokens[0], tokens[1], tokens[2],
                                           tokens[3], tokens[4]).timestamp()
        else:
            info = os.stat(self.path)
            self.epoch = info.st_mtime
            self.date_time = time.strftime('%Y:%m:%d %H:%M:%S',
                                           time.localtime(info.st_mtime))

    def _latlon_from_exif(self):
        raw_lat = self.tags['GPS GPSLatitude'].values
        raw_lon = self.tags['GPS GPSLongitude'].values
        raw_lat_ref = str(self.tags['GPS GPSLatitudeRef'])
        raw_lon_ref = str(self.tags['GPS GPSLongitudeRef'])

        raw_lat = str(raw_lat[0]) + '°' + str(raw_lat[1]) + '\'' + \
            str(float(raw_lat[2].num) / raw_lat[2].den) + '\"' + raw_lat_ref
        raw_lon = str(raw_lon[0]) + '°' + str(raw_lon[1]) + '\'' + \
            str(float(raw_lon[2].num) / raw_lon[2].den) + '\"' + raw_lon_ref

        self.latitude  = dms2dec(raw_lat)
        self.longitude = dms2dec(raw_lon)
        self.gps_found = True

    def _latlon_from_coordinates(self, coords):
        best_time_delta = 9e9
        best_coord = None

        for coord in coords:
            delta = math.fabs(self.epoch - coord[3])
            if delta < best_time_delta:
                best_time_delta = delta
                best_coord = coord

        if best_time_delta < 300.0:
            self.latitude  = best_coord[1]
            self.longitude = best_coord[0]
            self.altitude  = best_coord[2]
            self.gps_found = True


def parse_geojson(file):
    coordinates = []
    with open(file) as infile:
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
            trk.stylemap.normalstyle.linestyle.color =  "ffffff00"
            trk.stylemap.normalstyle.linestyle.width = 6
            trk.stylemap.highlightstyle.iconstyle.icon.href = 'http://earth.google.com/images/kml-icons/track-directional/track-0.png'
            trk.stylemap.highlightstyle.iconstyle.scale = 1.2
            trk.stylemap.highlightstyle.linestyle.color =  "ffffff00"
            trk.stylemap.highlightstyle.linestyle.width = 12


def correct_image_rotations(dir, geotagged_imgs):
    # Make a new directory to hold rotated images.
    tmp_dir = os.path.join(dir, 'rotated_images')

    if os.path.exists(tmp_dir): shutil.rmtree(tmp_dir)
    os.mkdir(tmp_dir)

    # Use exiftool to correct rotation
    for img in geotagged_imgs:
        new_path = os.path.join(tmp_dir, img.name)
        subprocess.check_output(["exiftran", "-a", img.path, "-o", new_path])
        img.path = new_path


def sort_geotagged_images(geotagged_imgs):
    imgs = []
    for img in geotagged_imgs:
        imgs.append([img.epoch, img])

    imgs.sort(key=lambda x: x[0])
    for idx, img in enumerate(imgs):
        img[1].idx = idx


if __name__ == '__main__':
    aparser = argparse.ArgumentParser()
    aparser.add_argument('--dir', help='directory with KML information',
                         type=str, required=True)
    args = aparser.parse_args()

    # Read coordinates from geojason files
    coords = [[]]
    kml_dir = os.path.join(args.dir, 'gaia')
    for file in os.listdir(kml_dir):
        coords.append(parse_geojson(os.path.join(kml_dir, file)))

    # Read exif data from images
    geotagged_imgs = []
    imgs = [file for file in os.listdir(args.dir) if '.jpg' in file]
    for img in imgs:
        with open(os.path.join(args.dir, img), 'rb') as infile:
            tags = exifread.process_file(infile)
            geotagged_imgs.append(GeoImage(tags, os.path.join(args.dir, img),
                                  sum(coords, [])))

    # Create KML and add tracks to it from geojson
    kml = simplekml.Kml()
    create_tracks(kml, coords)
    correct_image_rotations(args.dir, geotagged_imgs)
    sort_geotagged_images(geotagged_imgs)

    # Add images to KMZ and save it
    for geotagged in geotagged_imgs:
        if geotagged.latitude is not None:
            path = kml.addfile(geotagged.path)
            pnt = kml.newpoint()
            pnt.coords = [(geotagged.longitude, geotagged.latitude)]
            pnt.name = str(geotagged.idx)
            pnt.description = '<img src="{:s}" alt="picture" width="{:d}" height="{:d}"/>'.format(path, geotagged.width, geotagged.height)
            # pnt.description = '<img src="' + path + '" alt="picture"/>'
    kml.savekmz(args.dir.split('/')[-1] + ".kmz", format=False)  # Saving as KMZ

import time
import os
import re
import datetime
import math
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
    MAX_LATLON_TIME_DELAY = 300

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
            self.date_time = str(self.tags['EXIF DateTimeOriginal'])
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

        if best_time_delta < self.MAX_LATLON_TIME_DELAY:
            self.latitude  = best_coord[1]
            self.longitude = best_coord[0]
            self.altitude  = best_coord[2]
            self.gps_found = True

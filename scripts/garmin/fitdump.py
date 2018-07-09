#!/usr/bin/env python3

import argparse
import datetime
import json
import sys
from pathlib import Path
from collections import OrderedDict

import fitparse

def format_messages(messages):
  '''Formats a fit 'record' into a something digestable by haskell.'''
  def snakecase_to_camelcase(word):
    '''Converts underscore to camelcase'''
    return ''.join([x.capitalize() if i else x for i, x in enumerate(word.split('_'))])

  formatted_messages = []
  for message in messages:
    formatted_message = {}
    for field in message:
      if 'unknown' not in field.name:
        if field.name == 'timestamp':
          x = str(field.value).split(' ')
          field.value = x[0] + 'T' + x[1] + 'Z'
        formatted_message[snakecase_to_camelcase(field.name)] = field.value

    formatted_messages.append(formatted_message)

  return formatted_messages

def fit2json(fitfile, outfile):
  '''TODO'''
  fitfile = fitparse.FitFile(
      str(fitfile),
      data_processor=fitparse.StandardUnitsDataProcessor(),
      check_crc = True,
  )

  try:
    sport   = format_messages(fitfile.get_messages(name=['sport']))[0]
    records = format_messages(fitfile.get_messages(name=['record']))
    session = format_messages(fitfile.get_messages(name=['session']))

    x = {}
    x['sport'] = sport['name'].replace(' ', '')
    x['records'] = records

    with outfile.open('w') as fout:
      json.dump(x, fout, indent=4, sort_keys=True, default=str)
  except Exception as ex:
    print(f"Failed to dump {fitfile}!")
    print(str(ex))

if __name__ == '__main__':
  """Main."""
  parser = argparse.ArgumentParser(description='Convert a fit file into a friendlier json.')
  parser.add_argument('--folder', type=str, required=True, help='Folder containing fit files.')
  flags = parser.parse_args()

  # Convert all fit files.
  for fitfile in Path(flags.folder).iterdir():
    if fitfile.suffix == ".fit":
      fit2json(fitfile, fitfile.with_suffix('.json'))

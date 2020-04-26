#!/usr/bin/env python3

import argparse
import json
from pathlib import Path

import fitparse

def snakecase_to_camelcase(word):
  '''Converts underscore to camelcase'''
  return ''.join([x.capitalize() if i else x for i, x in enumerate(word.split('_'))])

def convert_to_timestamp(ts):
  '''Converts a datetime style timestamp into one parseable by haskell.'''
  ts = str(ts).split()
  assert len(ts) == 2, f"Unknown timestamp format! {ts}"
  return ts[0] + 'T' + ts[1] + 'Z'

def maybe(d, k):
  if k in d:
    return f'Just {d[k]}'
  return 'Nothing'

def format_messages(messages):
  '''Formats a fit 'record' into a something digestable by haskell.'''
  formatted_messages = []
  for msg in messages:
    msg = msg.get_values()
    formatted_messages.append(
      { 'cadence': maybe(msg, 'cadence'),
        'distance': maybe(msg, 'distance'),
        'altitude': maybe(msg, 'enhanced_altitude'),
        'speed': maybe(msg, 'enhanced_speed'),
        'fractionalCadence': maybe(msg, 'fractional_cadence'),
        'heartRate': maybe(msg, 'heart_rate'),
        'lat': maybe(msg, 'position_lat'),
        'lon': maybe(msg, 'position_long'),
        'temperature': maybe(msg, 'temperature'),
        'timestamp': convert_to_timestamp(msg['timestamp']),
      }
    )

  return formatted_messages

# TODO(matte): Wrap this an emailer context.
def get_sport(fitfile):
  '''We should only ever have one sport. If we have zero than throw an error!'''
  sports = list(fitfile.get_messages(name='sport'))
  assert len(sports) == 1, f'The length of sports should always be 1!\n'
  return sports[0].fields[0].raw_value.replace(' ', '')

def get_records(fitfile):
  records = fitfile.get_messages(name=['record'])
  return format_messages(records)

def get_session(fitfile):
  return fitfile.get_messages(name=['session'])

def fit2json(filename):
  '''Convert a fit file into a human readable json.'''
  try:
    fitfile = fitparse.FitFile(str(filename))
    print(get_sport(fitfile))
    x = {'sport': get_sport(fitfile), 'records': get_records(fitfile)}
    return json.dumps(x, indent=4, sort_keys=True, default=str)
  except Exception as ex:
    print(filename)
    print(ex)
    exit(1)

def save_fits_as_jsons(input_folder, output_folder):
  for fitfile in Path(input_folder).iterdir():
    if fitfile.suffix == ".fit":
      output = fit2json(fitfile)
      with (Path(output_folder) / fitfile.with_suffix('.json').name).open("w") as outfile:
        outfile.write(output)

if __name__ == '__main__':
  """Main."""
  parser = argparse.ArgumentParser(description='Convert a fit file into a friendlier json.')
  parser.add_argument('--dump', type=str, help='Singel file to dump to stdout')
  parser.add_argument('--input_folder', type=str, help='Folder containing fit files.')
  parser.add_argument('--output_folder', type=str, help='Folder containing fit files.')
  flags = parser.parse_args()

  if flags.dump:
    print(fit2json(flags.dump))
  elif flags.input_folder and flags.output_folder:
    assert str(flags.output_folder) != str(flags.input_folder), "Woah there nelly... Use different folders."
    save_fits_as_jsons(flags.input_folder, flags.output_folder)
  else:
    print("Unrecognized command line arguments!")

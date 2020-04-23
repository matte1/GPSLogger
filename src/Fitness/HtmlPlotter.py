import sys
import numpy as np
from pathlib import Path

from bokeh.io import show
from bokeh.layouts import column
from bokeh.models import ColumnDataSource, RangeTool
from bokeh.plotting import figure
from bokeh.sampledata.stocks import AAPL

if __name__ == '__main__':
  fitness_csv = Path(sys.argv[1])

  data = {}
  with fitness_csv.open("r") as infile:
    for line in list(infile)[1:]:
      tokens = line.split(',')
      date = int(tokens[0])
      week = int(tokens[1])
      miles = float(tokens[4])

      if date not in data.keys():
        data[date] = {'x': [], 'y': []}

      data[date]['x'].append(week)
      data[date]['y'].append(miles)

for date in data:
  p = figure(title=f'{date}', plot_width=300, plot_height=300)
  p.line(x=data[date]['x'], y=data[date]['y'])
  show(p)

# output_file("test.html")
# save(p)

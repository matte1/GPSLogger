# Introduction

### gaia2kml

gaia2kml creates a simple correspondence between a set of images and geojson tracks from gaia by matching looking at image EXIF data when available and timestamps when not present. Its saved as a kmz which can be loaded easily into google earth. Expects a directory setup like the following.

![Alt text](data/gaia2kml_example.png?raw=true "Title")

###### Directory
```
|___logs/
    |___gaia/
        |___YEAR_NAME
          NAME.geojson
          img1.jpg
          ...
          imgN.jpg
```


###### Usage
```
python3 gaia2kml --dir log/gaia/folder
```

### TODO

1. Running Stats
  - GPS History (Colorize mile by pace)
  - Races
  - Fastest times
2. Garmin Daily Stats FitFile
  - sleep schedule
  - steps
3. Surfing Stats
  - GPS History
  - Favorite Spots
  - Time in Water
4. Adventure Stats
  - GPS History + Pictures
  - Faster times, most elevation, longest hike, etc...
5. Expense Stats
  - Breakdown by category per week/month/year (pull from Mint)
6. Investment Stats
  - Only percentage / diversity
7. Driving Stats
  - GPS History
  - ML Dataset?
8. Google Maps Timeline data
9. GeoViz - Google earth, open street view, etc...

### NOTES

1. Fit lap message contains calories expended during that lap

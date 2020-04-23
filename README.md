# Introduction

### Design Notes

#### Website

 1. Sweet website with all the derived plots from Running / Financials
 2. Link to google photo albums

#### Running

 1. Write a tool that attempts to calculate AeT from seen runs.
 2. Write a tool that attempts to calculate how Heart Rate scales with speed.
 3. Generate training plans algoithmically based on where I'm at and a mix of hill/interval work.
 4. Write a basic html plotting tool that displays time in HeartRate Zones per week, per month, per year


#### Financials

 1.

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

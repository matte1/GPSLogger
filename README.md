# Introduction

### Design Notes

#### Motivation

I want be able to pull insights from data I collect exercise and from my financial planning.

#### Website

 1. Sweet website with all the derived plots from Running / Financials
 2. Link to google photo albums

#### Fitness

###### Running

 1. ~~Write a tool that attempts to calculate AeT from seen runs.~~ This might not be necessary as the garmin metrics do a pretty good job of predicting VO2max, etc...
 2. ~~Write a tool that attempts to calculate how Heart Rate scales with speed.~~ Not sure this is worth it...
 3. Generate training plans algoithmically based on where I'm at and a mix of hill/interval work.
 4. Plot running stats..
  - Weekly: HR Zones, HR Zones to go, miles, miles to go, elevation gain
  - Monthly: Same as weekly?
  - Yearly: Same as weekly?
  - Meters per heart beat 

###### Climbing


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

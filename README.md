
#  [BirdMApp](https://orsondewitt.com/birdmapp)

BirdMApp is a tool to empower bird enthusiasts with high-level knowledge of their surroundings, best times and places to find certain species, and a convenient way to track their hobby.

## Table of Contents

  - [Features & Use Cases](#features--use-cases)
  - [How it Works](#how-it-works)
  - [Installation](#installation)
  - [Import](#import)
  - [Photo Ratings](#photo-ratings)
  - [Species Distribution](#species-distribution)
  - [Rarity](#rarity)
  - [Map Layout](#map-layout)
  - [Limitations and Missing Species](#limitations-and-missing-species)
  - [Tech Stack](#tech-stack)
  - [License](#license)
  - [Need help?](#need-help)

## Features & Use Cases

BirdMApp allows to easily import all your photos, and, provided that they have tags with species names, prepares an in-depth analysis of your progress. Alternatively, you may upload any csv file with species names, for example, from eBird. Then, it's as simple as clicking "Sync" to update the data.

**See all species you are likely to encounter in any given region, in any given month**; see how many you've already seen in any particular month, or overall.
Each region shows up as a convenient table that can serve as a checklist for your future trip; you can filter by month and group by family, as well as see each bird's photo, relative rarity and IUCN Red List category. It helps identify new lifers, too.

https://github.com/OrsonDeWitt/BirdMApp/assets/26813733/1d929f8d-4c88-4bbc-9cf2-b7e263f3bb72


**Rarity** allows you to judge at a glance how likely you are to see a species, and whether finding it requires preparation. The default sorting in country tables is by rarity; the darker the blue, the more ubiquitous the species; the darker the red, the more rare. 

![image](https://github.com/OrsonDeWitt/BirdMApp/assets/26813733/a8f1406a-f183-464b-9e5e-6a849b024dc1)
*Note: The rarer the species, the more important it is that we report sightings of them on on eBird, iNaturalist, etc.*

**Plot all your observations on a map; see species distribution; track monthly migrations**. The darker the area, the more likely you are to see the bird there. With more than 1 billion occurrences of more than 10000 species analyzed from the years 2011 through 2021, the bird ranges are up-to-date and are not extrapolated or otherwise edited to fill the gaps. 

https://github.com/OrsonDeWitt/BirdMApp/assets/26813733/945eea3b-7e46-47a2-926c-6b3e9e243b47

**Track your progress with charts**. The insights you gather will take your bird-watching to the next level; knowing that you've seen all the marine or diurnal species in a region, or that you're most active in the summer months might mean you need to change your habits if you wish to discover more species. 

https://github.com/OrsonDeWitt/BirdMApp/assets/26813733/91e857a2-2034-4c74-a25e-6704bacb569e

Additionally, **use timeline** at the bottom to filter observations and see how far you've come.

## How it Works

BirdMApp is a mostly self-contained Shiny application. Despite the fact that it runs from your browser, only a few things rely on an active Internet connection, namely: map rendering (Leaflet tiles), flag icons (Flagpedia), and bird photos (mostly iNaturalist). Otherwise, it is completely local; all the data is processed and stored on your computer only. Your photos do not get uploaded anywhere, which allows for quick analysis of metadata (by your own computer).

### Installation

No installation is required. Simply unzip the archive and run "BirdMApp.bat" from the root directory. The app is portable, which means you can copy it to a flash drive or another computer and use it anywhere without losing your data. 

You may receive a warning from Windows that the file is from an unknown source. As far as I understand, this can be fixed by creating an installer and signing it, but it's just a (less) convenient wrapper for a zip archive, so I didn't bother (any advice from experienced developers is appreciated). 

If you may want to put an icon on the desktop, the icon is included in the root folder, just change the path to wherever you put the app in. 

*Note: there must not be any non-Latin characters in the path. So if your computer's name has hieroglyphics, don't put it on desktop (the full path to desktop contains your computer name). Put it on C:/ or any other drive instead.*

### Import

You may import your data in one of two ways:
 - photo folder (.jpg/.jpeg)
 - csv file (i.e. eBird data)
 
#### Photo Folder

Choosing a folder on your computer allows BirdMApp to read metadata contained in your photos with the help of [ExifTool](https://exiftool.org)/[ExifToolR](https://github.com/JoshOBrien/exiftoolr>). The following metadata is read: date, time, GPS coordinates and tags.
If your photos do not have GPS coordinates, they will not be plotted on the map, but all the other functions will work fine.
If your photos do not have tags, they will not be processed at all. You may search "image tagging tools" in your favorite search engine to assign them. To test if a particular tool is properly assigning tags, check that you can see the tags when navigating to your photo with the explorer. For example, in Windows, tags will show up in "Details" section of Properties. You may also edit your tags there.

The following tags are automatically excluded from analysis: "bug", "spider", "gastropod", "mammal", "fish", "uncertain". This means that, if you have a general "wildlife" folder, you may choose it without fear, just be aware that it will take more time to process. If you are using another system for naming your non-bird photos, let me know, and I'll add it to the list so you don't have to worry about moving your photos around. Additionally, you may choose whether you wish to include or exclude photos with the tag "zoo". 

After that, the tags are compared to an internal bird list — semi-manual edit of the Handbook of the Birds of the World ([HBW-BirdLife Version 7.0 from December 2022](http://datazone.birdlife.org/species/taxonomy)) containing over 10000 species. The tags do not have to perfectly match and case is not important. Furthermore, Jaro-Winkler Distance Algorithm under the hood will handle any possible typos, and additional import options will let you choose between auto-assigning regional names ("Asian", "Indian", "Javan", "European", etc.), common adjectives ("Ruddy", "Crested", etc.), or simply the word "Common". Tags in Latin are also not a problem.

When the import is finished (it takes less than 5 minutes for <1000 photos and about 20 minutes for ~6000), you will see a nicely formatted table that will list all the errors that might have prevented some certain photos from being added and analyzed, so you can use this to see which photos are missing tags or locations, and fix them.

If some of the species are not imported ("tags could not be matched") but you think they should have been, let me know, it might be that a synonym has to be added; or you may check *missing_birds.txt*, it might be there (see [Limitations and Missing Species](#limitations-and-missing-species)).

#### .csv (eBird)

When importing a .csv file, it has to contain the following columns: *scientificName/scientific name/commonName/common name* (either of the 4), *latitude*, *longitude*, and *date*. eBird provides this data to you in a .csv format when downloading your data from "My eBird". Naturally, there will be no photos to plot, but you'll otherwise be able to see markers on the map, explore countries and see your statistics. 

### Photo Ratings

If you check "Use my photos in species lists" when importing, your photos will show up in country lists and on the tree map in the "charts" panel. Which photo is chosen depends on the rating of the photo. If none of your photos have rating, the newest photo will be chosen.

### Species Distribution

For calculating relative abundance and determining which countries and areas have which species, I used all bird observations with coordinates from the years 2011-2021, provided by [GBIF](https://www.gbif.org/), which has data from eBird, iNaturalist, Xeno-Canto, Swedish Species Observation System, and others. I filtered out zero coordinates, duplicates and dubious records, as well as observations from a curated list of zoos and bird parks, and matched coordinates with the respective countries/areas. I then applied weighted normalization to account for variance in sampling effort to those areas where the species has been seen at least once, and calculated the relative abundance.

I established a threshold of 0.033 for relative abundance to determine the presence or absence of a bird species within a particular country or area. In other words, if the **weighted** abundance of a bird species in a given area represents less than 3.3% of the weighted abundance of the area with the highest sightings, it is considered to be absent. This threshold is based on personal field experience and is somewhat subjective. If somebody wanted to play around with these values, I might be able to make it adjustable.  

### Rarity

Rarity allows you to judge at a glance how likely you are to see a species, and whether finding it requires preparation. The default sorting in country tables is by rarity; the darker the blue, the more ubiquitous the species; the darker the red, the more rare. You may see the whole range of colors by clicking on India or nearly any other Asian country.

Rarity is based on number of unique recorded observations in the span of 11 years (2011-2021). If the species is recently-split, it might be falsely marked as rare. You may refer to IUCN Red List category for additional context.

### Map Layout

You may notice that some countries are divided into regions/states, while others are combined into one. This is done for the sole purpose of accurate representation; for example, Monaco does not have any unique species that would separate it from the French Alpes-Maritimes department, but it does somewhat skew the results as it does not have many bird observations in general. Therefore, it is combined with France. On the other hand, you would not be searching for yellow-legged gulls in the Sahara Desert, so it makes sense to split up Algeria for more accurate representation of its regional biodiversity. A few other big countries are separated for the same reasons, such as Saudi Arabia, Australia, Russia, and others.
Similarly, leaving France (and other countries with overseas territories) with 1000+ exotic species would hinder anyone from gathering any meaningful insight from either France itself or any of its territories. Therefore, most islands (even Corsica and Sardinia) have been... made independent for the purposes of BirdMApp :)

## Limitations and Missing Species

Taxonomies are not static. It might be that a species gets split into multiple, has its family revised, becomes extinct, and so on. Currently, BirdMApp concerns species that have at least a few observations in the years 2011 through 2021. This might change as more data becomes available for subsequent years.

In the attached *missing birds.txt* file is the full list of species that are present in the 2022 HBW-BirdLife Handbook, but are excluded from BirdMApp (along with the reasoning). It currently contains about 500 bird species (~4.5% of all birds), most of which are newly-split, and have barely any observations to draw conclusions from (due to delays in taxonomy updates or disagreements with the changes*). These have been sorted out manually, so I am sure there are plenty of birds that should and should not be in the list. Please let me know if you find errors.
Additionally, subspecies are not included, but I might explore this possibility in the future if there is enough interest. It's important to note, though, that subspecies are recorded rarely (as compared to species), and such analysis could be less accurate.

**Or a million other reasons that we just have to accept*

## Tech Stack

##### Front-end

Shiny, JavaScript, HTML, CSS

DesktopDeployR (R package)

##### Back-end

R (refer to app/packages.txt for the packages used in BirdMApp)

ExifTool

##### Data sources

GBIF (API & RGBIF), https://doi.org/10.15468/dl.mqbrn2 (18 July 2023)

[Handbook of the Birds of the World Version 7.0 from December 2022](http://datazone.birdlife.org/species/taxonomy)

The accompanying blog post for this project can be found [here](https://orsondewitt.com/posts/birdmapp-retrospective/).

## License

The app is released under the GNU Affero General Public License version 3 (GNU AGPLv3).

## Need help?

 - If running "BirdMApp.bat" does not open a pop-up with progress bar, gives you an error or just does not open a new tab in your default browser, copy and replace the *config.cfg* file from *BirdMApp* folder to *BirdMApp/app* and try again
 - If your app is running but something doesn't work like it should, try closing the tab and then launching the application again.
 - If an issue persists, you may want to delete *use.RData* from *app\shiny\userData* and import your data again.
Alternatively, you may open an issue on [Github](https://github.com/OrsonDeWitt/BirdMApp/issues). I'll be happy to help.

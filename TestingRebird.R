#>max defaults to all results - (possibly 10,000)
#>The limiting factor is how many days back you want to look
#>This package specifically only allows access to recent bird sightings
#   -back = 14 by default. This means that the default is taking back records by
#    14 days
#   -The back variable can only be between 1 and 30 days
#>Another Limiting factor is that the only ways to search are primarily location 
# based and only have a secondary reliance on scientfic names.
#    -you can only search for a location within 50 kilomenters of a point
#    -If no coordinates are entered it approximates your location with your IP
#>It's not the best package for searching for a specific species outside of a 50
# kilometer radious


library(rebird)
#ebirdfreq-Download historical frequencies of bird obs
#   -Must be a search by either states, counties, or hotspots
#   -startyear defaults to 1900 and endyear defaults to the current year
ebirdfreq(loctype = "states", loc = "US-SC")

#ebirdgeo-sightings at a specific lat/long location within up to 50 kilometers
ebirdgeo(species = "Cyanocitta cristata", lat = 32.776475, lng = -79.931051, back = 30)

#ebirdhotspot-recent observations at a birdsighting hotspot
#requires an ID for the hotspot you want to look up

#ebirdloc-recent observations at a locality
#also requires an ID for the location

#ebirdnotable- the most recent notable sightings dependent on location within 50 Km
# and up to 30 days
ebirdnotable(lat = 32.776475, lng = -79.931051)

#ebirdregion-recent obs at a region
#requires a region code

#ebirdregioncheck-checks to see if a region is valid
#checks to make sure your region code is valid

#ebirdtaxononmy-ebird taxonomy
#The webpage with the explaination for this no longer exists

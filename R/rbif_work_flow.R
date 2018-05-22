library(rgbif)
sp_key = name_suggest(q = 'Caretta caretta', rank = 'species')$key[1]

# preforms the query
res = occ_download(paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
             user = 'danmcglinn', pwd = 'moreplants1', 
             email = 'danmcglinn@gmail.com')

# checks on if complete
occ_download_meta(res)

# downloads and imports
dat <- occ_download_get(res[1], overwrite = TRUE) %>%
       occ_download_import()

class(dat)

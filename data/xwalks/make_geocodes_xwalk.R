
# TODO: ----
# when nygeotype=undefined_cousub
#   I have not bothered to get the county code and county name, would not be hard
#   there are 9 of these records
#   shortname is missing for these records, although fullname is not
# nygeotype=CDP
#   I have not bothered to get the county code and county name, probably hard


# places ------------------------------------------------------------------

# from chatGPT
# To determine the primary county for a place in the American Community Survey
# (ACS), you can use the "Geographic Names Information System" (GNIS) database
# maintained by the U.S. Geological Survey.
#
# The GNIS database contains information on place names and their associated
# counties, including the population and geographic coordinates of each place.
# You can download the GNIS data from the U.S. Board on Geographic Names website
# (https://geonames.usgs.gov/domestic/download_data.htm).
#
# Once you have downloaded the data, you can filter it to select only the places
# that you are interested in and then sort by population to determine the
# primary county. For incorporated places, you can use the "PLA" feature class
# in the GNIS database, which contains information on all incorporated places in
# the United States.
#
# Alternatively, the Census Bureau provides a dataset called the "Relationship
# Files" which link all geographic areas, including places, to their associated
# county or counties. These files can be accessed through the Census Bureau's
# website
# (https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html).
#
# Using either of these data sources, you can determine the primary county for a
# place based on the county with the largest share of the place's population.


# Overview ----

# Goal:
#   create geocodes xwalk that has:
#     multiple codes for each geographic area of interest
#     state abbreviations
#     county codes and county names for lower-level geographies
#     NY geotypes - nation, state, county, city, town, village, CDP, others

# General approach: start with a full set of Census codes in the ACS, and extend it

#  Background: Census places (e.g., NY cities and villages) do not have codes
#  that indicate the county they are in because under Census geography rules
#  they may cross county borders

#  We want to assign a dominant county to each Census place in NY
#  NYS has a file that has this information but only has gnis codes
#  Geographic Names Information System (GNIS) 
#  https://www.usgs.gov/faqs/what-geographic-names-information-system-gnis
#  https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
#  the census code placens is a gnis code

#  Method: We can get dominant county by a circuitous method:
#    TIGER/Line Shapefiles
#    see https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf
#      p.3-52+ 


# create a data frame with geocodes, names, and dominant county for places we care about

# str_detect(shortname, " UT") ~ "unorganized",  # what is this?? Chautauqua Lake UT unorganized territory

# libraries and functions ------------------------------------------------------
# census_apikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
# census_api_key(census_apikey, install=TRUE)

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))

# constants ---------------------------------------------------------------

# "external" folders
xdacs <- r"(E:\data\acs\sf\2021_5year)"

# folders in this project
dacs <- here::here("data", "acs")
dxwalks <- here::here("data", "xwalks")



# download full-table acs files -------------------------------------------
# example
# acsdt5y2021-b01001.dat
# https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b01001.dat


tabnames <- c("B01001", "B01002", "B01003", "B07001", "B07009", "B07409", "B15003", "B25071")

f <- function(tab, destdir, overwrite=FALSE){
  urlbase <- "https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData"
  
  str_sub(tab, 1, 1) <- str_to_lower(str_sub(tab, 1, 1))
  fname <- paste0("acsdt5y2021-", tab, ".dat")
  url <- path(urlbase, fname)
  
  fpath <- path(destdir, fname)
  if(!file_exists(fpath) | overwrite){
    print(paste0("downloading... ", fname))
    download.file(url, fpath, mode="wb")
  } 
  return()
}

purrr::map(tabnames, f, xdacs)

## read from big zip file ----
# 5YRData.zip\data\prt01\prod\sumfile_new\output\2021\5YRData
(zpath <- path(xdacs, "5YRData.zip"))
fname <- "acsdt5y2021-b01001.dat"
(zsub <- path(r"(data\prt01\prod\sumfile_new\output\2021\5YRData)", fname))

a <- proc.time()
df1 <- read_delim(unz(zpath, zsub))
b <- proc.time()
b - a # 3.6 secs

system.time(df1 <- read_delim(unz(zpath, zsub))) # 2.8 secs
system.time(df2 <- read_delim(path(xdacs, fname))) # 1.3 secs


fnames <- utils::unzip(zpath, list=TRUE)$Name |> path_file()
ht(fnames)




# get primary county for each place in NY ----
## get place-parts and place-part population from full-file tables for 2017-2021 ----
# https://www2.census.gov/programs-surveys/acs/summary_file/
# https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/documentation/
# https://github.com/uscensusbureau/acs-summary-file/wiki
# https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/documentation/ACS_Table_Based_SF_Excel_Import%20GEO%20Names_Tool.pdf
# https://www.census.gov/content/dam/Census/library/publications/2019/acs/acs_summary-file_handbook_2019.pdf
# https://www.census.gov/content/dam/Census/library/publications/2021/acs/acs_summary_file_handbook_2021.pdf
# https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b01001.dat
# https://api.census.gov/data/2021/acs/acs5/geography.html
# 050 county

# 0700000US361150256102550 fullname: Argyle village, Argyle town, Washington County, New York
# geoid elements:
# 1-3 sumlevel 070
# 4-7 ?? all zeros 0000
# 8-9 US US
# 10-11 stfips 36
# 12-14 county 115
# 15-19 cousub 02561
# 20-24 place 02550

# 430C100US3697310001

# "0400001US36","New York -- Urban"
# "0400043US36","New York -- Rural"


# rural
# https://mtgis-portal.geo.census.gov/arcgis/apps/MapSeries/index.html?appid=49cd4bc9c8eb444ab51218c1d5001ef6


## ONETIME get 20215 table shells ----
df1 <- vroom(path(xdacs, "ACS20215YR_Table_Shells.txt"))
glimpse(df1)

tabvars <- df1 |> 
  rename(table=`Table ID`,
         variable=`Unique ID`) |> 
  lcnames()

saveRDS(tabvars, path(xdacs, "tabvars.rds"))

tables <- tabvars |> 
  select(table, title, universe) |> 
  distinct()
saveRDS(tables, path(xdacs, "tables.rds"))

tables |> filter(str_detect(title, "EDUCATION"))


## get place-part county information for all NY places, directly from web (or download full file) ----
geourl <- "https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/documentation/Geos20215YR.txt"

geo1 <- vroom(geourl, col_select = 
                c(GEO_ID, STUSAB, SUMLEVEL, COUNTY, COUSUB, PLACE, COMPONENT,
                  NAME)) |> 
  filter(STUSAB=="NY", SUMLEVEL=="070") # place parts JUST WHAT WE NEED

geo2 <- geo1 |> 
  lcnames() |> 
  rename(geoid=geo_id, fullname=name) |> 
  # drop: 99999 appears to be full town, or TOV; also drop NYC
  filter(place!="99999", place!="51000") |>
  mutate(ss=str_split(fullname, ","),
         lss=purrr::map(ss, length) |> str_trim() |> unlist()) |> # length of ss
  hoist(ss, placename = list(1), cousubname=list(2), countyname=list(3), stname=list(4)) |> 
  mutate(across(contains("name"), str_trim)) |> 
  # after examining lss to be sure it is 4 for all, drop it
  select(-lss)
# count(geo2, lss)

tmp <- geo2 |> 
  filter(countyname=="Washington County")

## get population for place-parts from pop age sex table ----
pasurl <- "https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b01001.dat"

pdf1 <- vroom(pasurl, col_select = c(GEO_ID, B01001_E001)) |> 
  filter(GEO_ID %in% geo2$geoid) |> 
  rename(geoid=GEO_ID, pop=B01001_E001)

# pop of all the places and place parts in NY
gpop1 <- geo2 |> 
  left_join(pdf1, by = join_by(geoid))

# determine primary county
gpop <- gpop1 |> 
  # collapse by place, county to get the county pop for each placepart
  summarise(pop_part=sum(pop),
            .by=c(stusab, county, countyname, place, placename)) |> 
  mutate(pop_place=sum(pop_part),
         primaryco=pop_part==max(pop_part),
         .by=c(stusab, place, placename))
  
saveRDS(gpop, path(dacs, "nyplaceparts.rds"))

# done

# a few more

# B15003   EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
tabvars |> 
  filter(table=="B15003")

# geoid elements:
# 1-3 sumlevel 070
# 4-7 ?? all zeros 0000
# 8-9 US US
# 10-11 stfips 36
# 12-14 county 115
# 15-19 cousub 02561
# 20-24 place 02550

# selected summary levels
# 010	us	
# 020	region	
# 030	division	
# 040	state	
# 050	state› county	
# 060	state› county› county subdivision	
# 067	state› county› county subdivision› subminor civil division	
# 070	state› county› county subdivision› place/remainder (or part)	
# 140	state› county› tract	
# 150	state› county› tract› block group	
# 155	state› place› county (or part)	
# 160	state› place	
# 170	state› consolidated city
# 400	urban area
# 410	urban area› state (or part)
# 430	urban area› state (or part)› county (or part)

url <- "https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b15003.dat"

ussumlevs <- c("010", "040")
nysumlevs <- c("050", "060", "160", "170", "400", "410", "430")

# str_sub(GEO_ID, 4, 7)=="0000",
tmp <- vroom(url) |> 
  filter(str_sub(GEO_ID, 1, 3) %in% ussumlevs |
         (str_sub(GEO_ID, 10, 11)=="36" &  
            str_sub(GEO_ID, 1, 3) %in% nysumlevs) |
           GEO_ID %in% c("0400001US36", "0400043US36"))
tmp2 <- tmp |> filter(str_sub(GEO_ID, 1, 1)=="4")

tmp2 <- tmp |> 
  filter(GEO_ID %in% c("0400001US36", "0400043US36"))


tmp <- vroom(url) |> 
  filter(str_sub(GEO_ID, 1, 3) %in% sumlevs,
         # str_sub(GEO_ID, 4, 7)=="0000",
         str_sub(GEO_ID, 10, 11)=="36")



# , col_select = c(GEO_ID, STUSAB, SUMLEVEL, COUNTY, COUSUB, PLACE, COMPONENT, NAME)
# https://www.toptrix.net/2015/07/never-loose-download-of-large-files.html

# ONETIME: get the base ACS names and codes -------------------------------------------------------
## get names data with population ----
# when geometry=TRUE get_acs returns NAME twice, as NAME.x and NAME.y
# df1 <- get_names(table="B01003", geometry=TRUE) # get a table with only one variable, for exploring names

df1 <- get_acstab(acstab="B01003", year=2021, geometry=TRUE) # get a table with only one variable, for exploring names
glimpse(df1)

## drop geometry  ----
df2 <- df1 |> 
  select(-geometry) |>
  rename(pop=estimate) |> 
  select(-c(variable, moe, table))
glimpse(df2)
tmp <- count(df2, shortname, fullname)
saveRDS(df2, path(dxwalks, "acs_geocodes_raw.rds"))


# abc


# prepare to enhance the ACS codes ----
acscodes1 <- readRDS(path(dxwalks, "acs_geocodes_raw.rds"))
cocodes <- readRDS(path(dxwalks, "primary_county.rds")) |> 
  filter(primaryco) |> 
  select(geoid=fips, place, cntycode=countya, county, placepop)
stdf <- fips_codes |> # state abbreviations
  select(stabbr=state, statefp=state_code, stname=state_name) |> 
  distinct()
# data(fips_codes) # tidycensus
# glimpse(fips_codes)
# count(fips_codes, state, state_code, state_name) # 57 -- no US

acscodes2 <- acscodes1 |> 
  mutate(statefp=ifelse(is.na(statefp),
                        str_sub(geoid, 1, 2),
                        statefp)) |> 
  left_join(stdf, by = "statefp") |> 
  mutate(stabbr=case_when(geoid=="1" ~ "US",
                          is.na(stabbr) & 
                            str_ends(fullname, ", New York", negate = FALSE) ~ "NY",
                          TRUE ~ stabbr),
         stname=case_when(is.na(stname) & stabbr=="US" ~ "United States",
                          is.na(stname) & stabbr=="NY" ~ "New York",
                          TRUE ~ stname))

## add county codes to places that don't have one ---
# at this point, places don't have a county assigned to them

count(acscodes2, stabbr, stusps, statefp, stname) # we no longer need stusps
count(acscodes2, stabbr, stname, state_name) # don't need state_name

# define nygeotypes -------------------------------------------------------
# check the code used below

# cousubs
bool_borough <- expression(geotype=="cousub" & str_detect(fullname, " borough")) # note the space
bool_reservation <- expression(geotype=="cousub" & str_detect(fullname, " Reservation")) # space to be safe
bool_town <- expression(geotype=="cousub" & str_detect(fullname, " town")) # note the space
bool_cousub_undef <- expression(geotype=="cousub" & str_detect(fullname, "not defined"))
bool_cousub_unorg <- expression(geotype=="cousub" & str_detect(fullname, "Chautauqua Lake UT"))

# cities -- see detective work below
bool_cousub_city <- expression(geotype=="cousub" & str_detect(fullname, " city")) # 61, county, space to be safe
bool_place_city <- expression(geotype=="place" & str_detect(fullname, " city")) # 62, no county


bool_village <- expression(geotype=="place" & str_detect(fullname, " village")) # note the space

tmp <- acscodes2 |> 
  filter(eval(bool_village)) |> 
  select(contains("geo"), contains("name"), countyfp, cousubfp, placefp, placens, pop, -c(state_name, stname))


# city detective work ----
# summary:
#   place city records have all 62 cities including NYC, but/and:
#     have one and only one record for each city
#     do not have county info
#     the Geneva record's water area includes the water in Seneca county
#   cousub city records have 61 city records, but/and:
#     do not include Sherrill
#     include 2 records for Geneva:
#       the full city record in Ontario County
#       plus an all-water record with zero pop, in Seneca county
#   my solution will be to use place records for cities and put the relevant county information on
#     for place records:
#       create a faux cousubfp for city places from last 5 digits of geoid
#     for cousub records: 
#       drop geneva water record in Seneca county geoid 3609928640
#     merge place faux cousubfp against actual cousubfp for corresponding cousubs in New York 

# Q: will that work for villages to get dominant county?


# we have 2 Geneva city cousub      2 records
cities <- acscodes2 |> 
  filter(eval(bool_place_city) | eval(bool_cousub_city)) |> 
  select(contains("geo"), contains("name"), cousubfp, pop, -c(state_name, stname)) |> 
  select(geotype, namelsad, pop) |> 
  pivot_wider(names_from = geotype, values_from = pop)

cities |> 
  unnest(cols = c(cousub, place)) |> 
  filter(is.na(cousub) | is.na(place) | cousub != place)
# namelsad      cousub   place
# <chr>          <dbl>   <dbl>
# 1 Geneva city        0   12577
# 2 New York city     NA 8736047
# 3 Sherrill city     NA    3054

# key info for geneva
geneva <- acscodes2 |> 
  filter(eval(bool_place_city) | eval(bool_cousub_city)) |> 
  filter(namelsad=="Geneva city") |> 
  select(geoid, affgeoid, fullname, geotype, pop, aland, awater, countyfp, cousubfp, namelsadco)
# affgeoid            fullname                              geotype   pop    aland  awater countyfp namelsadco    
# <chr>               <chr>                                 <chr>   <dbl>    <dbl>   <dbl> <chr>    <chr>         
#   1 0600000US3606928640 Geneva city, Ontario County, New York cousub  12577 10914565   11463 069      Ontario County
# 2 0600000US3609928640 Geneva city, Seneca County, New York  cousub      0        0 4214444 099      Seneca County 
# 3 1600000US3628640    Geneva city, New York                 place   12577 10914565 4225907 NA       NA            


# village detective work --------------------------------------------------
# geotype=="place" & str_detect(fullname, "village") ~ "village",



# resume coding -----------------------------------------------------------


acscodes3 <- acscodes2 |> 
  select(-stusps, -state_name) |> 
  mutate(nygeotype=case_when(
    geotype %in% c("nation", "state", "county", "schooldist") ~ geotype,
    
    # cousubs
    eval(bool_borough) ~ "borough",
    eval(bool_town) ~ "town",
    eval(bool_reservation) ~ "reservation",
    eval(bool_cousub_city) ~ "city_cousub",
    eval(bool_cousub_undef) ~ "undefined_cousub",
    # next is Chautauqua Lake UT, Chautauqua County, New York - unorganized territory
    eval(bool_cousub_unorg) ~ "unorganized_cousub",
  
    # caution: places don't have a county assigned to them
    eval(bool_place_city) ~ "city", # place records will be our official city records
    geotype=="place" & str_detect(fullname, "village") ~ "village",
    geotype=="place" & str_detect(fullname, "CDP") ~ "CDP",
    
    TRUE ~ "other")) 

count(acscodes3, geotype, nygeotype)

# check for availability of county information
count(acscodes3 |> filter(geotype %in% "place"), countyfp) # no countyfp
count(acscodes3 |> filter(geotype %in% "cousub"), countyfp)
count(acscodes3 |> filter(geotype %in% "county"), countyfp)
# we have countyfp for all but 9 cousub and all county recs, but none for place recs

# look at how NYC is coded
check <- acscodes3 |> filter(str_detect(fullname, "New York city"))
# geoid 3651000, countyfp missing, placens 02395220



# clean names -------------------------------------------------------------
name_check <- expression(basename != shortname)


tmp <- acscodes2 |> 
  filter(eval(name_check)) |> 
  select(contains("geo"), contains("name"), cousubfp, pop, -c(state_name, stname))


# put "City" in New York City's shortname
acscodes1 |> filter(str_detect(shortname, "New York")) |> select(c(2, 4:7))
acscodes1a <- acscodes1 |> 
  mutate(shortname=ifelse(geotype=="place" & geoid=="3651000",
                          "New York City", 
                          shortname))
acscodes1a |> filter(str_detect(shortname, "New York")) |> select(c(2, 4:7))



# put dominant county on file ----
# match placens (gnis code) to a NYS data file that has gnis and dominant county
count(acscodes3, placens) |> ht()
# here is the placens code for the city of Albany: 00978659 
# the codes are 8 characters long, zero-padded on the left

## ONETIME: download NYS data with dominant county county fips and name ----
# https://data.ny.gov/Government-Finance/New-York-State-Locality-Hierarchy-with-Websites/55k6-h6qq
# https://data.ny.gov/Government-Finance/New-York-State-Locality-Hierarchy-with-Websites/55k6-h6qq/data
nyfn <- "New_York_State_Locality_Hierarchy_with_Websites.csv"
url <- "https://data.ny.gov/api/views/55k6-h6qq/rows.csv?accessType=DOWNLOAD&sorting=true"
download.file(url, path(dxwalk, nyfn), mode="wb")

## get NY codes data and put dominant county code on the file ----
df1 <- read_csv(path(dxwalks, nyfn), col_types = cols(.default = col_character()))
glimpse(df1)

df2 <- df1 |> 
  rename(swis=`SWIS Code`, type=`Type Code`, typef=Type, 
         county=`County Name`, city=`City Name`, town=`Town Name`, village=`Village Name`,
         county2=`2nd County`, web=Website, muni=Municipality, 
         gnis=`GNIS ID`, stfips=`State FIPS`, cntycode=`County Code`, cntyfips=`County FIPS`)

count(df2, type, typef)
count(df2, gnis) |> ht()
# NY gnis codes are 6 characters, no zero padding

# create placens codes that are 8 characters long, zero-padded on the left
str_pad("978659", width=8, side="left", pad="0")

domco1 <- df2 |> 
  filter(muni != "New York City") |> # not coded the way I want, we'll do NYC by hand
  filter(typef %in% c("City", "Village")) |> 
  mutate(placens=str_pad(gnis, width=8, side="left", pad="0")) |> 
  select(typef, placens, muni, cntycode, county)

acscodes4 <- acscodes3 |> 
  left_join(domco1, by="placens")

check <- acscodes4 |> filter(nygeotype %in% c("city", "village"))
# to see nonmatches, sort check by muni
# we did not match (geoid, fullname, placens):
#   3651000 New York city, New York 02395220 (intentional)
#   3646085 Mastic Beach village, New York 02390131
#   3664749 Salamanca city, New York 00979450

domco1 |> filter(str_detect(muni, "Mastic"))
# Village 02680279 Mastic Beach 103      Suffolk
domco1 |> filter(str_detect(muni, "Salamanca"))
# City  00979451 Salamanca 009      Cattaraugus

# make sure the to-be-adjusted placens codes aren't elsewhere in the acs data
acscodes4 |> filter(placens %in% c("02680279", "00979451"))

# create revised domco that adjusts these 2
domco2 <- domco1 |> 
  mutate(placens=case_when(placens=="02680279" ~ "02390131",
                           placens=="00979451" ~ "00979450",
                           TRUE ~ placens))

acscodes5 <- acscodes3 |> # start with the previous good acscodes
  left_join(domco2, by="placens")
check <- acscodes5 |> filter(nygeotype %in% c("city", "village"))  
count(check, nygeotype, typef) 

## put county names of dominant county on the file, for all records ----
counties <- acscodes5 |> 
  filter(nygeotype=="county") |> 
  select(geoid, countyfp, stabbr, shortname, fullname, pop)

acscodes6 <- acscodes5 |> 
  mutate(countyfp=ifelse(is.na(countyfp), cntycode, countyfp)) |> 
  left_join(counties |> 
              select(stabbr, countyfp, countyname=shortname),
            by=c("stabbr", "countyfp")) |> 
  # handle NYC manually
  mutate(countyfp=case_when(is.na(cntycode) & 
                              geotype=="place" & 
                              geoid=="3651000" ~ "51000",
                            TRUE ~ countyfp),
         countyname=case_when(is.na(cntycode) & 
                              geotype=="place" & 
                              geoid=="3651000" ~ "New York City",
                            TRUE ~ countyname),
         stcntyfips=ifelse(!is.na(countyfp),
                           paste0(statefp, countyfp),
                           NA_character_)) |> 
  relocate(muni, .after = fullname) # make it easier to compare names
count(acscodes6, countyname, county)

# examine selected records
check <- acscodes6 |> filter(countyname=="Albany")
check <- acscodes6 |> filter(str_detect(fullname, "New York city"))
check <- acscodes6 |> filter(nygeotype=="city")
check <- acscodes6 |> filter(nygeotype=="village")

acscodes6 |> 
  filter(is.na(countyname)) |> 
  count(geotype, nygeotype)
# everything looks ok - I haven't bothered with counties for Census Designated Places

# drop unneeded NY codes variables, put vars in desired order, and save
# we don't need cntycode, county, typef from the NY file
acscodes7 <- acscodes6 |>
  select(affgeoid, geoid, geotype, nygeotype, shortname, fullname,
         stabbr, stname, statefp, statens, 
         stcntyfips, countyfp, countyns, countyname,
         cousubfp, cousubns, 
         placefp, placens, 
         lsad, 
         pop, aland, awater,
         survey, endyear)

# saveRDS(acscodes7, path(dxwalks, "acs_geocodes.rds"))  

tmp <- acscodes7 |> filter(str_detect(fullname, "New York city"))
tmp <- acscodes7 |> filter(str_detect(fullname, "New York city"))


# final step - put NY region codes on the acscodes file -----------------------------------
# acscodes7 <- readRDS(path(dxwalks, "acs_geocodes.rds"))  
rgns <- read_excel(path(dxwalks, "nycounty_xwalk.xlsx"), sheet="region_codes")

glimpse(acscodes7)
glimpse(rgns)

acscodes8 <- acscodes7 |> 
  left_join(rgns |> 
              select(stcntyfips=geoid,
                     tmpcounty=county, 
                     rgn_num, rgn_code, rgn_osc), 
            by = "stcntyfips")
count(acscodes8, rgn_num, rgn_code, rgn_osc, stcntyfips, countyname, tmpcounty)
# all good
glimpse(acscodes8)

acscodes9 <- acscodes8 |> 
  select(-tmpcounty) |> 
  relocate(rgn_num, rgn_code, rgn_osc, .after=lsad)
glimpse(acscodes9)

saveRDS(acscodes9, path(dxwalks, "acs_geocodes.rds"))  


# explore -----------------------------------------------------------------

acscodes <- readRDS(path(dxwalks, "acs_geocodes.rds"))  
glimpse(acscodes)


# possible alternative approaches ----

## get gnis data -----------------------------------------------------------
url <- "https://geonames.usgs.gov/docs/stategaz/AllStates.zip"
gnis_path <- path(dxwalks, path_file(url))
download.file(url, gnis_path, mode="wb")
# NY_Features_20210825.txt
fn <- "NY_Features_20210825.txt"

df1 <- read_delim(unz(gnis_path, fn))
glimpse(df1)
count(df1, FEATURE_CLASS)

df2 <- df1 |> 
  lcnames() |> 
  filter(feature_class %in% c("Census", "Civil", "Locale", "Populated Place")) |> 
  select(starts_with("feature"), starts_with("state"), starts_with("county"),
         latitude=prim_lat_dec, longitude=prim_long_dec, mapname=map_name)

# read_csv(unz(zfile, file), col_types = cols(.default = col_character()), n_max=Inf)



## get gazeteer files ------------------------------------------------------
url <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2022_Gazetteer/2022_Gaz_place_national.zip"
gaz_path <- path(dxwalks, path_file(url))
download.file(url, gaz_path, mode="wb")
# NY_Features_20210825.txt
# fn <- "NY_Features_20210825.txt"

# df1 <- read_delim(unz(gnis_path, fn))
# glimpse(df1)
# count(df1, FEATURE_CLASS)


## tiger line shape files --------------------------------------------------
url <- "https://www2.census.gov/geo/tiger/TIGER2022/PLACE/tl_2022_36_place.zip"
tiger_path <- path(dxwalks, path_file(url))
download.file(url, tiger_path, mode="wb")

library(sf)
sfn <- "tl_2022_36_place.shp"
sfn_path <- path(dxwalks, "tl_2022_36_place", sfn)
df <- st_read(sfn_path)

# US_place_2021.shp
sfn <- "US_place_2021.shp"
sfn_path <- path(dxwalks, "nhgis0007_shape", sfn)
df <- st_read(sfn_path)


path_file(geourl)
gpath <- path(dacs, path_file(geourl))

# geoco <- vroom(geourl, col_select = 
#                  c(GEO_ID, STUSAB, SUMLEVEL, COUNTY, COUSUB, PLACE, COMPONENT,
#                    NAME)) |> 
#   filter(STUSAB=="NY", SUMLEVEL=="050") # counties
# 
# geo2 <- geo1 |> 
#   left_join(geoco |> 
#               select(STUSAB, COUNTY, CONAME=NAME),
#             by = join_by(STUSAB, COUNTY))

path_file(pasurl)
# download.file(pasurl, path(dacs, path_file(pasurl)), mode="wb")

geoco <- vroom(geourl, col_select = 
                 c(GEO_ID, STUSAB, SUMLEVEL, COUNTY, COUSUB, PLACE, COMPONENT,
                   NAME)) |> 
  filter(STUSAB=="NY", SUMLEVEL=="155") # county parts NO DON'T WANT THIS

# pdf <- vroom(pasurl, col_select = c(GEO_ID, B01001_E001)) # wow - pop for full US geo -- 621k recs



# download.file(geourl, path(dacs, path_file(geourl)), mode="wb")
df <- read_delim(gpath)
glimpse(df)

df2 <- df |> 
  lcnames() |> 
  filter(stusab=="NY", sumlevel=="070")

tmp <- df2 |> 
  select(geo_id, name, county, cousub, place, component)
tmp |> filter(str_detect(name, "Cambridge"))

ppath <- path(dacs, path_file(pasurl))
pdf1 <- read_delim(ppath)
ns(pdf1)
pdf2 <- pdf1 |> 
  lcnames() |> 
  filter(geo_id %in% df2$geo_id) |> 
  select(geo_id, b01001_e001)

df3 <- df2 |> 
  left_join(pdf2, by = join_by(geo_id))

df4 <- df3 |> 
  filter(place!="99999")

df5 <- df4 |> 
  select(stusab, sumlevel, component, state, county, cousub, place, geoid=geo_id,
         name, pop=b01001_e001)

df5 |> 
  filter(str_detect(name, "Cambridge"))



# censusapi ----

# census hierarchy: https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf

# https://www.census.gov/data/developers/data-sets/acs-5year.2021.html

# https://www.census.gov/data/developers/data-sets/acs-5year.html
# hierarchy: https://api.census.gov/data/2021/acs/acs5/geography.html
# requirements: 
# examples: https://api.census.gov/data/2021/acs/acs5/subject/examples.html
# variables: https://api.census.gov/data/2021/acs/acs5/variables.html

# profiles: https://api.census.gov/data/2021/acs/acs5/profile.html



# https://api.census.gov/data/2021/acs/acs5?get=NAME,B01001_001E&for=place/remainder%20(or%20part):*&in=state:17%20county:031%20county%20subdivision:14000
# https://api.census.gov/data/2021/acs/acs5?get=NAME%2CB01001_001E&for=place%2Fremainder%2520%28or%2520part%29%3A%2A&in=state%3A36
df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  vars = c("NAME", "state", "county", "county subdivision", "B01001_001E"), # total population B01003_001
  region = "place/remainder (or part):*",
  regionin = "state:36")

# good!
df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  show_call = TRUE,
  vars = c("NAME", "B01001_001E"),
  region = "place/remainder (or part):*",
  regionin = "state:17 county:031 county subdivision:14000")

# good
df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  show_call = TRUE,
  vars = c("NAME", "B01001_001E"),
  region = "place/remainder (or part):*",
  regionin = "state:17&county:031&county subdivision:14000")

# state› place› county (or part)

df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  show_call = TRUE,
  vars = c("NAME", "B01001_001E"),
  region = "county (or part):*",
  regionin = "state:36&place:02842")



df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  show_call = TRUE,
  vars = c("NAME", "B01001_001E"),
  region = "place/remainder (or part):*",
  regionin = "state:17&county:*")


df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  show_call = TRUE,
  vars = c("NAME", "B01001_001E"),
  region = "county (or part):*",
  regionin = "state:17&place:*")



df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  show_call = TRUE,
  vars = c("NAME", "B01001_001E"),
  region = "place/remainder (or part):*",
  regionin = "state:17 county:031 county subdivision:*")


df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  show_call = TRUE,
  vars = c("NAME", "B01001_001E"),
  region = "place/remainder+(or+part):*",
  regionin = "state:17+county:031+county+subdivision:14000")


# NAME,B01001_001E&for=place/remainder%20(or%20part):*&in=state:17%20county:031%20county%20subdivision:14000`


df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  vars = c("NAME", "B01001_001E"), # total population B01003_001
  region = "county+(or+part):*",
  regionin = "state:36")



df <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  vars = c("NAME", "B01001_001E"), # total population B01003_001
  region = "070",
  regionin = "state:36")

# county%20%28or%20part%29%3A%2A
# county%20(or%20part):*

vars <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2021,
  type = "variables")

v2 <- vars |> filter(label=="Geography")

acs_geographies <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2021,
  type = "geographies")



# find primary county -- nhgis -------------------------------------------------------------------
# this is the way to go, or consider working with the Census api??
# https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_api_handbook_2020_ch02.pdf

# NHGIS Extract: nhgis0008 http://data2.nhgis.org/downloads
# Documentation for NHGIS datasets is available at https://www.nhgis.org/documentation.

# Research using NHGIS data should cite it as:
#   Steven Manson, Jonathan Schroeder, David Van Riper, Tracy Kugler, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 17.0 [dataset]. Minneapolis, MN: IPUMS. 2022. http://doi.org/10.18128/D050.V17.0

# For policy briefs or articles in the popular press, we recommend that you cite the use of NHGIS data as follows:
#   IPUMS NHGIS, University of Minnesota, www.nhgis.org

# nhgis0008_ds254_20215_place_070.csv

fnz <- "nhgis0008_csv.zip" # 
zpath <- path(dxwalks, fnz)

df1 <- read_csv(unz(zpath, "nhgis0008_csv/nhgis0008_ds254_20215_place_070.csv"))
glimpse(df1)

df2 <- df1 |> 
  lcnames() |> 
  filter(stusab=="NY")
skim(df2)

df3 <- df2 |> 
  select(gisjoin, year, stabbr=stusab,
         geoid=geo_id, 
         name_e, name_m, 
         county, countya, cousub, cousuba, 
         place, placea, 
         pop=aon4e001,
         popmoe=aon4m001)

tmp <- df3 |> 
  filter(place=="Cambridge village")
# 718 + 1,601 = 2,319 -- same as Censusreporter.org

df4 <- df3 |> 
  filter(!is.na(place), placea!="51000") |> # drop NYC
  arrange(placea, place, countya, county) |>
  summarise(n=n(), partpop=sum(pop), .by=c(placea, place, countya, county)) |> 
  mutate(n=n(), placepop=sum(partpop), primaryco=partpop==max(partpop), .by=c(placea, place)) |> 
  mutate(fips=paste0("36", placea)) |> 
  select(fips, placea, place, countya, county, placepop, partpop, primaryco)

df4 |> filter(countya=="115")
df4 |> filter(placepop!=partpop)
tmp <- df4 |> filter(primaryco)

saveRDS(df4, path(dxwalks, "primary_county.rds"))



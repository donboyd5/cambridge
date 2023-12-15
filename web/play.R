library(censusapi)

Sys.getenv("CENSUS_KEY")
# obtained at https://api.census.gov/data/key_signup.html. This function will
# default to a `CENSUS_KEY` stored in your .Renviron if available.

# Detailed Tables contain the most detailed cross-tabulations, many of which are published down to block groups. The data are population counts. There are over 20,000 variables in this dataset.
# Subject Tables provide an overview of the estimates available in a particular topic.  The data are presented as population counts and percentages.  There are over 18,000 variables in this dataset. 
# Data Profiles contain broad social, economic, housing, and demographic information. The data are presented as population counts and percentages. There are over 1,000 variables in this dataset.
# Comparison Profiles are similar to Data Profiles but also include comparisons with past-year data.  The current year data are compared with prior 5-Year data and include statistical significance testing.  There are over 1,000 variables in this dataset.

# https://www.census.gov/data/developers.html

# acs 5-year 2022 ----
# api http://api.census.gov/data/2022/acs/acs5
# geo https://api.census.gov/data/2022/acs/acs5/geography.html
# vars https://api.census.gov/data/2022/acs/acs5/variables.html
# groups http://api.census.gov/data/2022/acs/acs5/groups.html
# 



# geographies -------------------------------------------------------------
# geo variable names
# CBSA CD CNECTA CONCIT COUNTY COUSUB CSA DIVISION GEO_ID GEOCOMP METDIV NATION NECTA
# NECTADIV PLACE PLACEREM PRINCITY PUMA REGION SDELM SDSEC SDUNI SLDL SLDU STATE
# SUBMCD SUMLEVEL TBLKGRP TRACT TRISUBREM TTRACT UA ucgid ZCTA


# -----
# https://api.census.gov/data/2022/acs/acs5/geography.html


capis <- listCensusApis()

tmp <- capis |> 
  filter(str_detect(title, "American Community"))

tmp <- capis |> 
  filter(str_detect(name, "acs5"))

tmp <- capis |> 
  filter(str_detect(name, "acs5"), type=="Aggregate")
count(tmp, vintage)
count(tmp, temporal)
tmp |> 
  filter(vintage==2022) |> 
  select(-description)
# name:
# acs5 -- detailed table
# cprofile -- comparison profile
# profile
# subject http://api.census.gov/data/2022/acs/acs5/subject/examples.json

# geographies:
# county, place


# metadata ----------------------------------------------------------------

md2022 <- listCensusMetadata(
  name="acs/acs5",
  vintage = 2022,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = TRUE
)

vars <- md2022 |> 
  # filter(str_detect(name, "B01001")) |> 
  filter(group=="B01001") |> 
  mutate(label=str_remove(label, "Estimate!!"),
         label=str_remove(label, "Total:!!"),
         label=str_replace_all(label, ":!!", "_"),
         label=str_replace_all(label, ":", ""),
         moe=ifelse(str_sub(name, -1)=="E",
                    str_sub(name, 1, -2)  |> paste0("M"),
                    NA_character_)) |> 
  select(group, concept, label, estimate=name, moe) |> 
  pivot_longer(c(estimate, moe),
               names_to = "type",
               values_to = "variable") |> 
  arrange(type, variable) |> 
  relocate(concept, .after = variable)
vars$label
ht(vars)

myvars <- vars$variable

temp <- getCensus(
  name = "acs/acs5",
  vintage = 2022, 
  vars = c("GEO_ID", "NAME", myvars), 
  region = "county:091,113,115",
  regionin = "state:36")

t2 <- temp |> 
  pivot_longer(starts_with("B01001")) |> 
  left_join(vars |> select(c(variable, label)), by=c("name"="variable")) |> 
  mutate(type=case_when(str_sub(name, -1)=="E" ~ "estimate",
                        str_sub(name, -1)=="M" ~ "moe",
                        TRUE ~ "ERROR"))
count(t2, type)

t3 <- t2 |> 
  filter(type=="estimate") |> 
  mutate(pct=value / value[name=="B01001_001E"], .by=GEO_ID)
  


tmp <- getCensus(name = "acs/acs5",
                 vintage = 2022,
                 vars = myvars,
                 region = "state:36")


tmp <- getCensus(name = "acs/acs5",
                 vintage = 2016,
                 vars = myvars2,
                 region = "state:36")


myvars2 <- makeVarlist(name = "acs/acs5",
                      vintage=2022,
                      find = "5 to 9 years",
                      varsearch = "label")
myvars2



(tmp <- vars2$label[1:3])



vars <- md2022 |> 
  # filter(str_detect(name, "B01001")) |> 
  filter(group=="B01001") |> 
  mutate(label2=str_remove(label, "Estimate!!")) |> 
  mutate(label3=str_remove(label2, "Total:!!")),
         moe=ifelse(str_sub(name, -1)=="E",
                    str_sub(name, 1, -2)  |> paste0("M"),
                    NA_character_)) |> 
  select(group, concept, label, estimate=name, moe) |> 
  pivot_longer(c(estimate, moe),
               names_to = "type",
               values_to = "variable") |> 
  arrange(type, variable) |> 
  relocate(concept, .after = variable)
  # mutate(moe=name,
  #        str_sub(moe, -1, -1)=)



# examples ----
# https://www.hrecht.com/censusapi/articles/example-list.html

acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2022,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region = "county:*", # Geography to get.
  regionin = "state:36",
  show_call = TRUE)

acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2022,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region = "county subdivision", # Geography to get.
  regionin = "state:36",
  show_call = TRUE)

# ACS detail ----
# Get total population and median income for places (towns, cities, etc) in Indiana.
# GEO_ID GEOCOMP ucgid
acs_income <- getCensus(
  name = "acs/acs5",
  vintage = 2020, 
  vars = c("GEO_ID", "NAME", "B01001_001E", "B19013_001E"), 
  region = "place:*",
  regionin = "state:36")
head(acs_income)

# ACS Subject Tables ----
# Get the percent of people without an internet subscription by income for the five counties of New York City, with associated margins of error:
#   
# overall: S2801_C02_019E
# income less $20,000: S2801_C02_023E
# income $20,000 to $74,999: S2801_C02_027E
# income $75,000 or greater: S2801_C02_031E
acs_subject <- getCensus(
  name = "acs/acs1/subject",
  vintage = 2018, 
  vars = c("GEO_ID", "NAME", 
           "S2801_C02_019E", "S2801_C02_019M",
           "S2801_C02_023E", "S2801_C02_023M", 
           "S2801_C02_027E", "S2801_C02_027M",
           "S2801_C02_031E", "S2801_C02_031M"), 
  region = "county:005,047,061,081,085",
  regionin = "state:36")
head(acs_subject)

# ACS Comparison Profiles ---- Get the annual median household income in
# inflation-adjusted 2019 dollars for available New York cities for the past five years.

acs_comparison <- getCensus(
  name = "acs/acs1/cprofile",
  vintage = 2019, 
  vars = c("GEO_ID", "NAME", 
           "CP03_2015_062E", "CP03_2016_062E", "CP03_2017_062E", "CP03_2018_062E", "CP03_2019_062E"), 
  region = "place:*",
  regionin = "state:36")
head(acs_comparison)

# ACS Migration Flows ----
# American Community Survey Migration Flows documentation

# Get the number of people who moved in and out of Los Angeles county by their origin or destination.

flows <- getCensus(
  name = "acs/flows", # GEO_ID not allowed I think
  vintage = 2019,
  vars = c("MOVEDIN", "MOVEDOUT", "GEOID2", "FULL1_NAME", "FULL2_NAME"),
  # region = "county:115",
  # region = "place:11825", # not allowed
  # region="county subdivision:11511836",
  region="county subdivision:*",
  regionin = "state:36")
head(flows, n = 15L)

flows |> 
  # filter(FULL1_NAME=="Cambridge town, Washington County, New York" |
  #          FULL2_NAME=="Cambridge town, Washington County, New York") |> 
  filter(FULL1_NAME=="Cambridge town, Washington County, New York") |> 
  mutate(MOVEDIN=as.numeric(MOVEDIN),
         MOVEDOUT=as.numeric(MOVEDOUT)) |> 
  arrange(desc(MOVEDIN))

flows |> 
  # filter(FULL1_NAME=="Cambridge town, Washington County, New York" |
  #          FULL2_NAME=="Cambridge town, Washington County, New York") |> 
  filter(FULL1_NAME=="Cambridge town, Washington County, New York") |> 
  mutate(MOVEDIN=as.numeric(MOVEDIN),
         MOVEDOUT=as.numeric(MOVEDOUT)) |> 
  arrange(desc(MOVEDOUT))


# 3611511836

flows |> 
  mutate(MOVEDIN=as.numeric(MOVEDIN),
         MOVEDOUT=as.numeric(MOVEDOUT)) |> 
  arrange(desc(MOVEDIN))

flows |> 
  mutate(MOVEDIN=as.numeric(MOVEDIN),
         MOVEDOUT=as.numeric(MOVEDOUT)) |> 
  arrange(desc(MOVEDOUT))



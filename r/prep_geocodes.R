# create a tibble with geocodes for a superset of areas likely to be of interest.


# notes -------------------------------------------------------------------

# main data documentation is in E:\R_projects\projects\cambridge\acs

# censusapi info:
# https://github.com/hrecht/censusapi/
# https://www.hrecht.com/censusapi/index.html
# https://www.hrecht.com/censusapi/reference/getCensus.html
# https://www.hrecht.com/censusapi/articles/getting-started.html  useful
# https://www.hrecht.com/censusapi/articles/example-list.html see acs examples

# Census fips codes
# https://www.census.gov/library/reference/code-lists/ansi.html


# setup -------------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
sessioninfo::session_info()

dacs <- here::here("acs")


# get variable info -------------------------------------------------------

## get raw variable info ----
vars_acs2022_raw <- listCensusMetadata(
  name="acs/acs5",
  vintage = 2022,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = TRUE
)
comment(vars_acs2022_raw) <- "Variables in 2022 5-year ACS, as obtained from censusapi listCensusMetaData."
comment(vars_acs2022_raw)
saveRDS(vars_acs2022_raw, path(dacs, "vars_acs2022_raw.rds"))


## clean variable info ----
vars_acs2022_raw <- readRDS(path(dacs, "vars_acs2022_raw.rds"))

vars_acs2022_wide <- vars_acs2022_raw |> 
  filter(str_starts(label, "Estimate!!")) |> 
  select(group, estimate=name, concept, label) |> 
  mutate(label=str_remove(label, "Estimate!!"),
         label=str_replace_all(label, "!!", "_"),
         label=str_remove_all(label, ":"),
         moe=ifelse(str_sub(estimate, -1)=="E",
                    str_sub(estimate, 1, -2)  |> paste0("M"),
                    NA_character_)
         ) |> 
  arrange(estimate)

vars_acs2022 <- vars_acs2022_wide |> 
  pivot_longer(c(estimate, moe),
               names_to = "vtype",
               values_to = "variable") |> 
  select(variable, vtype, concept, label, group) |> 
  arrange(vtype, variable)
count(vars_acs2022, vtype)
saveRDS(vars_acs2022, path(dacs, "vars_acs2022.rds"))

## get geographic vars

geovars1 <- readRDS(path(dacs, "vars_acs2022_raw.rds")) |> 
  filter(label=="Geography" |
           name %in% c("GEOCOMP", "SUMLEVEL"))

geovars_acs2022 <- geovars1 |> 
  select(variable=name, category=label) |> 
  arrange(variable)

saveRDS(geovars_acs2022, path(dacs, "geovars_acs2022.rds"))

    
# rm(vars_acs2022, vars_acs2022_raw, vars_acs2022_wide)

# get census codes and total population, from ACS 2022 5-year -------------------

acsgeos <- listCensusMetadata(
  name = "acs/acs5", 
  vintage = 2022,
  type = "geography")
# we'll want us, state, county, county subdivision, place

## identify total population variable ----
v2022 <- readRDS(path(dacs, "vars_acs2022.rds"))
# Table B01003: Total Population
v2022 |> filter(group=="B01003")
# B01003_001E is total population

## identify geovars of interest ----
g2022 <- readRDS(path(dacs, "geovars_acs2022.rds"))

## define variables of interest ----

(vars <- c("GEO_ID", "SUMLEVEL", "NAME", "B01003_001E", setdiff(g2022$variable, c("GEO_ID", "SUMLEVEL"))))

# get acs data ----

us <- getCensus(name = "acs/acs5",
                 vintage = 2022,
                 vars = vars,
                 region = "us") |> 
  filter(GEO_ID %in% c("0100000US", "010C201US", "010C243US")) # keep nation and rural/urban parts, etc.

ny <- getCensus(name = "acs/acs5",
                 vintage = 2022,
                 vars = vars,
                 region = "state:36") |> 
  filter(GEO_ID %in% c("0400000US36", "040C201US36", "040C243US36")) # statewide, urban, and rural parts

vt <- getCensus(name = "acs/acs5",
                vintage = 2022,
                vars = vars,
                region = "state:50") |> 
  filter(GEO_ID %in% c("0400000US50", "040C201US50", "040C243US50")) # statewide, urban, and rural parts

nycounties <- 
  getCensus(name = "acs/acs5",
            vintage = 2022,
            vars = vars,
            region = "county",
            regionin = "state:36")

vtcounties <- 
  getCensus(name = "acs/acs5",
            vintage = 2022,
            vars = vars,
            region = "county",
            regionin = "state:50")

nycousubs <- 
  getCensus(name = "acs/acs5",
            vintage = 2022,
            vars = vars,
            region = "county subdivision",
            regionin = "state:36")

vtcousubs <- 
  getCensus(name = "acs/acs5",
            vintage = 2022,
            vars = vars,
            region = "county subdivision",
            regionin = "state:50")

nyplaces <- 
  getCensus(name = "acs/acs5",
            vintage = 2022,
            vars = vars,
            region = "place",
            regionin = "state:36")

vtplaces <- 
  getCensus(name = "acs/acs5",
            vintage = 2022,
            vars = vars,
            region = "place",
            regionin = "state:50")

# stack and save the geographic information -------------------------------

stack1 <- bind_rows(
  us |> mutate(geotype="nation"),
  
  ny |> mutate(geotype="state"),
  vt |> mutate(geotype="state"),
  
  nycounties |> mutate(geotype="county"),
  vtcounties |> mutate(geotype="county"),
  
  nycousubs |> mutate(geotype="cousub"),
  vtcousubs |> mutate(geotype="cousub"),
  
  nyplaces |> mutate(geotype="place"),
  vtplaces |> mutate(geotype="place")
  )
count(stack1, geotype)

# clean up the stacked data
stack2 <- stack1 |> 
  select(-c(state, county, county_subdivision, place)) |> # remove lower-case names that censusapi created
  select(-us) |>  # not needed
  lcnames() |> 
  mutate(stabbr=case_when(sumlevel=="010" ~ "US",
                          state=="36" ~ "NY",
                          state=="50" ~ "VT",
                          TRUE ~ "ERROR"),
         geotype=case_when(str_sub(geo_id, 4, 7) == "C201" ~ paste0(geotype, "_urban"),
                           str_sub(geo_id, 4, 7) == "C243" ~ paste0(geotype, "_rural"),
                           TRUE ~ geotype)) |> 
  select(geo_id, sumlevel, stabbr, name, geotype, state, county, cousub, place, everything())

count(stack2, stabbr, geotype)

saveRDS(stack2, path(dacs, "geocoded_acs2022.rds"))

# temp <- getCensus(
#   name = "acs/acs5",
#   vintage = 2022, 
#   vars = c("GEO_ID", "NAME", myvars), 
#   region = "county:091,113,115",
#   regionin = "state:36")

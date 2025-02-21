
# acs notes ----
# https://walkerke.github.io/tidycensus/articles/basic-usage.html#geography-in-tidycensus

# B01003_001E population
# B01002_001E median age
# B19013_001E median income
# B25077_001E median home value
# B25064_001E median gross rent
# B25003_001E total housing units
# B25002_002E owner occupied housing units
# B25002_003E renter occupied housing units
# B25002_001E total occupied housing units
# B25002_001E - B25003_001E vacant housing units
# ai did the following VERIFY
# B25002_001E / B25003_001E vacancy rate
# B25002_001E / B01003_001E housing units per person
# B25002_001E / B01002_001E housing units per person
# B25002_001E / B19013_001E housing units per dollar of income

# villages (and places)
# 3602550 Argyle village
# 3611825 Cambridge village
# 3630675 Greenwich village
# 3635474 Hoosick Falls village
# 3664771 Salem CDP
# 3665255 Saratoga Springs city
# 3665750 Schuylerville village
# 5004750 Bennington CDP


# explore and save ACS metadata ----

# https://censusreporter.org/topics/table-codes/

acsvars <- load_variables(2023, "acs5", cache = TRUE)
count(acsvars, geography)
unique(acsvars$concept) |> sort()

acsvars2 <- acsvars |>
  select(-geography) |> # missing for all
  separate_wider_delim(cols = name,
                       delim = "_",
                       names = c("table", "vnum"),
                       too_few = "align_start",
                       cols_remove = FALSE)  |>
  mutate(numvars=n(), .by=c(table, concept)) |>
  arrange(table, numvars, name)
# acsvars2$vnum |> unique() |> sort()

acs_tables_info <- acsvars2 |>
  select(table, concept, numvars) |>
  distinct()

saveRDS(acsvars2, fs::path(ddir, "acs_variables.rds"))
saveRDS(acs_tables_info, fs::path(ddir, "acs_tables_info.rds"))

# download and save individual ACS tables data ----

acs_variables <- readRDS(fs::path(ddir, "acs_variables.rds"))
acs_tables_info <- readRDS(fs::path(ddir, "acs_tables.rds"))

get_concept <- function(aconcept){
  acs_tables |>
  filter(str_detect(concept, coll(aconcept, ignore_case = TRUE))) |>
  arrange(numvars) |>
  select(table, concept, numvars)
}

poptabs <- get_concept("population")
inctabs <- get_concept("income")
edtabs <- get_concept("education")
povtabs <- get_concept("poverty")
movetabs <- get_concept("moved")
mobiletabs <- get_concept("mobility")

# B07004A Geographical Mobility in the Past Year for Current Residence in the United States

# selected tables and concepts
# B01002  Median Age by Sex
# B01003  Total Population
# B07003  Geographical Mobility in the Past Year by Sex for Current Residence in the United States
# B15003  Educational Attainment for the Population 25 Years and Over
# B17001  Poverty Status in the Past 12 Months by Sex by Age
# B19013  Median Household Income in the Past 12 Months (in 2023 Inflation-Adjusted Dollars)
# B25008  Total Population in Occupied Housing Units by Tenure
# B25071  Median Gross Rent as a Percentage of Household Income in the Past 12 Months (Dollars)

# make full_tabs a complete list of all tables we want
full_tabs <- c("B01002", "B01003", "B07003", "B15003", "B17001", "B19013", "B25003", "B25008", "B25071")

keeptabs <- acs_variables |>
  filter(table %in% c("B01002", "B01003", "B25008", "B19013", "B25071")) |>
  select(table, concept, numvars) |>
  distinct()

# to see what tables we've already saved:
saved <- fs::dir_ls(tdir) |>
  fs::path_file() |>
  str_remove(".rds") |>
  as_tibble() |>
  separate_wider_delim(cols = value, names = c("table", "geotype"), delim="_") |>
  mutate(exists="exists") |>
  pivot_wider(names_from = geotype, values_from = exists, values_fill = "missing")
saved

# define new tables of interest and get and save them in the next step
newtabs <- acs_variables |>
  filter(table %in% c("B07003")) |>
  # filter(table %in% full_tabs) |>
  select(table, concept, numvars) |>
  distinct()

# add new tables as needed

f <- function(tab){
    for(geo in c("state", "county", "place", "county subdivision")) {
      geotype <- ifelse(geo == "county subdivision", "cousub", geo)
      print(paste0(tab, " for ", geotype))
      fname <- paste0(tab, "_", geotype, ".rds")
      data <- get_acs(geography = geo, table = tab, state = c("NY", "VT"), year = 2023)
      data <- data |>
        mutate(geotype = geotype)
      saveRDS(data, fs::path(ddir, "acs_tables", fname))
  }
}

# provide vector of desired (new) table names to walk
purrr::walk(newtabs$table, f)


# combine acs tables, clean, and save ----

files <- fs::dir_ls(tdir)

acs_base <- purrr::map(files, readRDS) |>
  list_rbind()

acs_data <- acs_base |>
  rename(geoid=GEOID, geoname=NAME) |>
  mutate(year5=2023,
         stfips = str_sub(geoid, 1, 2)) |>
  left_join(usmap::statepop |>
              select(stfips=fips, stabbr=abbr, stname=full),
            by = join_by(stfips)) |>
  left_join(acs_variables |>
              mutate(label = str_remove(label, "Estimate!!")) |>
              select(variable = name, table, concept, vnum, label),
            by = join_by(variable)) |>
  mutate(trimname = str_remove(geoname, ",[^,]*$"),
         trimname = case_when(
           geotype == "county" ~ str_remove(trimname, " County"),
           # geotype == "place" ~ str_remove(trimname, " village"),
           geotype == "cousub" ~ str_remove(trimname, ",[^,]*$"), # remove county info
           .default = trimname),
         trimname = ifelse(geotype != "state" & stabbr == "VT",
                           paste0(trimname, ", VT"),
                           trimname)) |> 
  # get the base name, but don't remove CDP
  mutate(trimname_base = case_when(
    str_detect(trimname, " city") ~ str_remove(trimname, " city"),
    str_detect(trimname, " town") ~ str_remove(trimname, " town"),
    str_detect(trimname, " village") ~ str_remove(trimname, " village"),
    str_detect(trimname, " borough") ~ str_remove(trimname, " borough"),
    .default = trimname)
  )

check <- count(acs_data, geoid, geotype, stabbr, geoname, trimname)
count(acs_data, table, concept)

check <- count(acs_data, geoid, geotype, stabbr, trimname, trimname_base)

saveRDS(acs_data, fs::path(ddir, "acs_data.rds"))

check <- count(acs_data, variable, concept, label)
count(acs_data, geotype)

geographies <- acs_data |>
  select(geoid, geotype, stabbr, geoname, trimname, trimname_base) |>
  distinct()
saveRDS(geographies, fs::path(ddir, "acs_geographies.rds"))

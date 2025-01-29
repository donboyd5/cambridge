
source(here::here("slides", "setup.R"))

# county population estimates from NYS DOL based on Census PEP ----
# https://labor.ny.gov/stats/lsproj.shtm not this
# blob:https://data.ny.gov/61a4cddf-5446-400d-b12b-4bf7fef74721

fname <- "Annual_Population_Estimates_for_New_York_State_and_Counties__Beginning_1970_20250123.csv"
fpath <- fs::path(ddir, "population", fname)

# url <- "https://data.ny.gov/61a4cddf-5446-400d-b12b-4bf7fef74721"
# download.file(url, fpath, mode = "wb")
# copy directly unless I figure out how to download

df <- vroom(fpath)
glimpse(df)

pop <- df |> 
  rename(geoid=1, geoname=2, year=3, src=4, pop=5) |>
  mutate(geoid = ifelse(geoid == "36000", "36", geoid),
         trimname = str_remove(geoname, " County"), 
         geotype = ifelse(geoid == "36", "state", "county")) |> 
  relocate(geotype, .before = geoid)
tail(pop)  
count(pop, geoid, geoname)
count(pop, geotype)

saveRDS(pop, fs::path(ddir, "population", "county_population.rds"))


# check the data ----------------------------------------------------------

geo <- "36115"
counties <- c("36083", "36091", "36113", "36115")
pop |> 
  filter(geoid == counties[2]) |> 
  select(year, pop) |> 
  ggplot(aes(x=year, y=pop)) +
  geom_line() +
  labs(title="Population Estimates",
       x="Year",
       y="Population")

counties <- c("36083", "36091", "36113", "36115")

pop |> 
  filter(geoid %in% counties) |>
  mutate(ipop=pop / pop[year==2015], .by=geoid) |>
  filter(year >= 2015) |>
  ggplot(aes(x=year, y=ipop, colour=trimname)) +
  geom_line() +
  labs(title="Population Estimates",
       x="Year",
       y="Population")

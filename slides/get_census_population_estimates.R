
# https://www.hrecht.com/censusapi/articles/getting-started.html
# 
# ny place 2020-2023 https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/cities/totals/sub-est2023_36.csv
# us place 2020-2023 https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/cities/totals/sub-est2023.csv
# ny place 2010-2020 https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/cities/SUB-EST2020_36.csv
# us place 2010-2020 https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/cities/SUB-EST2020_ALL.csv

# pre-2020 use intercensal -- walks from one census to next
# us place 2000-2010 https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/cities/sub-est00int.csv
# us place 2010-2020 https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/intercensal/cities/sub-est2020int.csv
# us place 2020-2023 https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/cities/totals/sub-est2023.csv

source(here::here("slides", "setup.R"))
library(vroom)
library(btools)
library(censusapi)
get_api_key()


# download data ----------------------------------------------------------------

d20002010 <- "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/cities/sub-est00int.csv"
d20102020 <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/intercensal/cities/sub-est2020int.csv"
d2020plus <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/cities/totals/sub-est2023.csv"

download.file(d20002010, here::here("data", "population", "sub-est00int.csv"), mode = "wb")
download.file(d20102020, here::here("data", "population", "sub-est2020int.csv"), mode = "wb")
download.file(d2020plus, here::here("data", "population", "sub-est2023.csv"), mode = "wb")


# get place pop data ------------------------------------------------------

df2000p <- vroom(fs::path(popdir, "sub-est00int.csv")) 
df2010p <- vroom(fs::path(popdir, "sub-est2020int.csv")) 
df2020p <- vroom(fs::path(popdir, "sub-est2023.csv")) 

# create long files
# 2000-2010
long2000 <- df2000p |> 
  rename_with(str_to_lower) |>
  pivot_longer(cols = -(sumlev:stname), names_to = "poptype")
count(long2000, poptype)

long2000a <- long2000 |> 
  filter(str_starts(poptype, "popestimate")) |>
  mutate(year = str_sub(poptype, -4, -1) |> as.integer(),
         src = "2000-2010") |> 
  select(-poptype)
glimpse(long2000a)
count(long2000a, sumlev) # 040, 050, 061, 071, 157, 162

# 2010-2020
long2010 <- df2010p |> 
  rename_with(str_to_lower) |>
  pivot_longer(cols = -(sumlev:stname), names_to = "poptype")
glimpse(long2010)
count(long2010, poptype)

long2010a <- long2010 |> 
  filter(str_starts(poptype, "popestimate")) |>
  mutate(year = str_sub(poptype, -4, -1) |> as.integer(),
         src = "2010-2020") |> 
  select(-poptype)
glimpse(long2010a)
count(long2010a, sumlev) # 040, 050, 061, 071, 157, 162  plus 170 , 172

# 2020-2023
long2020 <- df2020p |> 
  rename_with(str_to_lower) |>
  pivot_longer(cols = -(sumlev:stname), names_to = "poptype")
glimpse(long2020)
count(long2020, poptype)

long2020a <- long2020 |> 
  filter(str_starts(poptype, "popestimate")) |>
  mutate(year = str_sub(poptype, -4, -1) |> as.integer(),
         src = "2020-2023") |> 
  select(-poptype)
glimpse(long2020a)
count(long2020a, sumlev) # 040, 050, 061, 071, 157, 162  plus 170 , 172


# combine files -----------------------------------------------------------

popests <- bind_rows(long2000a, long2010a, long2020a)
ht(popests)
saveRDS(popests, fs::path(popdir, "popests_raw.rds"))

# now clean the data for our purposes
constants$keep_places

# salem needs more data!!
popests |>  filter(state=="36", str_detect(name, "Salem"), year==2022)
popests |>  filter(state=="36", str_detect(name, "Salem"), year==2015) 

# construct geoids
check <- popests |>  filter(state=="36", county=="115")
check2 <- check |> filter(str_detect(name, "Salem"))


# place geoid cambridge is 3611825
# salem pop est 2020 census is 811
# let's get it from data.gov.ny

pdata <- popests |> 
  # filter(state=="36", county=="115", sumlev=="157", place=="11825") |>
  filter(state=="36", sumlev=="157") |>  
  mutate(geoid = paste0(state, place)) |> 
  filter(geoid %in% constants$keep_places) |>
  mutate(name=ifelse(place=="64771", paste0(name, "/CDP"), name)) |> 
  add_row(geoid="3664771", state="36", place="64771", name="Salem village/CDP", year=2020, src="2020 census", value=811) 

pdata |> 
  ggplot(aes(year, value)) +
  geom_line(data = pdata  |>  filter(!(geoid=="3664771" & year > 2010)), linetype="solid") +
  geom_line(data = pdata |> 
              filter(geoid=="3664771", year >= 2010), linetype = "dashed") + # dashed line for Salem
  geom_point() +
  facet_wrap(~name, scales="free_y", ncol=3)


# save data ---------------------------------------------------------------




# sumlev ------------------------------------------------------------------
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html
# SUMLEV: Geographic Summary Level
# 010 = Nation
# 020 = Region
# 030 = Division
# 040 = State and/or Statistical Equivalent
# 050 = County and/or Statistical Equivalent
# 061 = Minor Civil Division
# 071 = Minor Civil Division place part
# 157 = County place part
# 162 = Incorporated place
# 170 = Consolidated city
# 172 = Consolidated city -- place within consolidated city
# 310 = Metropolitan Statistical Area/Micropolitan Statistical Area
# 314 = Metropolitan Division
# 330 = Combined Statistical Area



# determine apis we want --------------------------------------------------
cenapis <- listCensusApis(name = NULL, vintage = NULL)
glimpse(cenapis)
pep <- cenapis |> 
  # filter(str_detect(title, coll("Population", ignore_case = TRUE))) |> 
  filter(str_detect(name, "pep"))

ts <- cenapis |> 
  # filter(str_detect(title, coll("Population", ignore_case = TRUE))) |> 
  filter(str_starts(name, "pep"))


pop |> filter(str_detect(description, "place"))
pop |> filter(str_detect(title, coll("place", ignore_case = TRUE)))

pop |> filter(name == "pep/subcty")
count(pop, name)

poverty_rate <- getCensus(
  name = "timeseries/poverty/saipe",
  vars = c("NAME", "SAEPOVRTALL_PT"),
  region = "county:037",
  regionin = "state:06",
  time = "from 2010")

listCensusMetadata(name = "timeseries", type = "variables")
listCensusMetadata(name = "timeseries/healthins/sahie", type = "variables")

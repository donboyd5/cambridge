
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
library(censusapi)
get_api_key()


popdir <- fs::path(ddir, "population")

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
# 



names(long2000) |> sort()



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

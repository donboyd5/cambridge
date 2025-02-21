# get_school_districts_saipe_data.R
 
# https://www.census.gov/data/datasets/2023/demo/saipe/2023-school-districts.html
 
 
# 2023 school district layout ----

# Position   Variable
# 1- 2       FIPS State code (00 for US record)
# 4- 8       District ID
# 10-81       District Name
# 83-90       Total Population 
# 92-99       Population of Relevant Children 5 to 17 years of Age
# 101-108      Estimated Number of Relevant Children 5 to 17 years old
# in Poverty Related to the Householder 
# 110-130      A tag indicating the file name and date of creation
 

# libraries and constaints ------------------------------------------------
source(here::here("slides", "setup.R"))


# download saipe school district data ----

urlbase <- "https://www2.census.gov/programs-surveys/saipe/datasets/"
f <- function(year){
  ext <- ifelse(year < 2003, ".dat", ".txt")
  y2 <- str_sub(year, 3, 4)
  url <- paste0(urlbase, year, "/", year, "-school-districts/ussd", y2, ext)
  fname <- fs::path_file(url)
  print(fname)
  download.file(url, fs::path(ddir, "saipe", fname), mode="wb")
}
purrr::walk(1999:2023, f) 


# read saipe school district data ----

# read sd data with all trailing info as a single var to be parsed 
files <- fs::dir_ls(saipedir) |> 
  str_subset("ussd.*\\.(dat|txt)$") |> 
  sort()

starts <- c(1, 4, 10)
ends <- c(2, 8, 130)
cnames <- c("stfips", "distid", "restofrec")
df <- vroom_fwf(files,
                id = "id",
                col_positions = fwf_positions(start=starts, end=ends,
                                              col_names=cnames))
count(df, id)


# clean the data ----------------------------------------------------------

# create integer year
df2 <- df |> 
  mutate(y2=str_sub(id, -6, -5),
         year = case_when(y2 > 25 ~ paste0("19", y2),
                          .default = paste0("20", y2)),
         year = as.integer(year)) |> 
  select(-y2, -id) |> 
  relocate(year) |> 
  arrange(stfips, distid, year)
glimpse(df2)

# parse restofrec into sdname and numeric fields
system.time(
  df3 <- df2 |> 
    mutate(
      restofrec2 = str_remove(restofrec, "(?i)ussd.*$") |> str_trim(),
      # Match exactly 3 numeric fields at the end using lookahead assertion
      numbers_part = str_extract(restofrec2, "(\\d+\\s+){2}\\d+$"),
      # Remove only the 3 trailing numbers while preserving numbers in names
      name = str_remove(restofrec2, "\\s*(\\d+\\s+){2}\\d+$") |> str_trim()
    ) |> 
    separate(
      numbers_part,
      into = c("num1", "num2", "num3"),  # Now expecting exactly 3 columns
      sep = "\\s+",
      convert = TRUE
    )
  )
summary(df3)

# when num4 has a value, make num1 part of name and shift others left ----
fyear <- 2023
fstfips <- "01"
fdistid <- "00006"

fyear <- 2004
fstfips <- "17"
fdistid <- "09930"

df3 |> filter(year == fyear, stfips == fstfips, distid == fdistid) |> select(-restofrec)
df3 |> filter(year == fyear, stfips == fstfips, distid == fdistid) |> select(num1, num2, num3, num4, num5)





# examine results, clean district names, change variable names as needed --------------
# pop, childpop, childpoverty
#  Estimated Number of Relevant Children 5 to 17 years old in Poverty Related to the Householder
glimpse(df3)
summary(df3)

df4 <- df3 |> 
  select(year, stfips, distid, name, pop=num1, childpop=num2, childpoverty=num3) |> 
  arrange(year, stfips, distid) |> 
  mutate(geoname=last(name), .by=c(stfips, distid)) |> 
  mutate(cpovrate = childpoverty / childpop * 100) |> 
  select(-name) |>
  relocate(geoname, .after=distid)
  

# look at a district or two -----------------------------------------------

# argyle 03210
# cambridge 06210
# greenwich 12900
# salem 25470
# schuylerville 26160
# hoosick falls 14760

keep_sds <- c("03210", "06210", "12900", "25470", "26160", "14760") 
tmp <- df4 |> 
  filter(stfips=="36", distid %in% keep_sds)

tmp |> 
  ggplot(aes(year, cpovrate)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 30, 2)) +
  facet_wrap(~geoname, scales="fixed", ncol=3)


# save the file -----------------------------------------------------------

saveRDS(df4, fs::path(ddir, "sd_saipe.rds"))


# data links --------------------------------------------------------------
# saipe
# https://www.census.gov/data/datasets/2023/demo/saipe/2023-school-districts.html
# https://www2.census.gov/programs-surveys/saipe/datasets/2010/2010-school-districts/
# https://www2.census.gov/programs-surveys/saipe/datasets/1999/1999-school-districts/ussd99.dat
# https://www2.census.gov/programs-surveys/saipe/datasets/2000/2000-school-districts/ussd00.dat
# https://www2.census.gov/programs-surveys/saipe/datasets/2001/2001-school-districts/ussd01.dat
# https://www2.census.gov/programs-surveys/saipe/datasets/2002/2002-school-districts/ussd02.dat
# https://www2.census.gov/programs-surveys/saipe/datasets/2003/2003-school-districts/ussd03.txt
# https://www2.census.gov/programs-surveys/saipe/datasets/2004/2004-school-districts/ussd04.txt
# https://www2.census.gov/programs-surveys/saipe/datasets/2005/2005-school-districts/ussd05.txt
# https://www2.census.gov/programs-surveys/saipe/datasets/2023/2023-school-districts/ussd23.txt
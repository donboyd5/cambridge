
# 
# get_acstab <- function(acstab, year=2021, geometry=FALSE){
#   # get a single table for a single year, for
#   # acstab <- "B01003"
#   # year <- 2021
#   # geometry <- FALSE
#   # geometry <- TRUE
#   
#   # unfortunately, when geometry=TRUE
#   #  get_acs returns two NAME variables, NAME.x and NAME.y
#   #  except for school districts
#   # otherwise it only returns NAME
#   # so drop NAME.y if it exists, and rename NAME.x if it exits
#   # do this before bind rows
#   
#   nation <- get_acs(geography = "us", 
#                     table = acstab, 
#                     year=year,
#                     survey="acs5",
#                     state=NULL,
#                     geometry = geometry,
#                     keep_geo_vars = geometry,  # TRUE only works if we set geometry=TRUE
#                     cache_table = TRUE) |> 
#     mutate(geotype="nation") |> 
#     rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
#   
#   states <- get_acs(geography="state",
#                 state=state.abb,
#                 geometry = geometry,
#                 keep_geo_vars = geometry,
#                 table=acstab,
#                 year=year) |> 
#     mutate(geotype="state") |> 
#     rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
#   
#   counties <- get_acs(geography="county",
#                 state="NY",
#                 # county=counties,
#                 geometry = geometry,
#                 keep_geo_vars = geometry,
#                 table=acstab,
#                 year=year) |> 
#     mutate(geotype="county") |> 
#     rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
#   # name.x is short, name.y has " County, New York"
#   
#   cousubs <- get_acs(geography="county subdivision",
#                 state="NY",
#                 # county=cosubs_county,
#                 geometry = geometry,
#                 keep_geo_vars = geometry,
#                 table=acstab,
#                 year=year) |> 
#     mutate(geotype="cousub") |> 
#     rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
#   # for cousubs, name.y is a full name, e.g.:
#   #   name.x      Manhattan
#   #   name.y      Manhattan borough, New York County, New York
#   #   namelsad    Manhattan borough
#   #   namelsadco  New York County
#   
#   
#   places <- get_acs(geography="place",
#                 state="NY",
#                 table=acstab,
#                 geometry = geometry,
#                 keep_geo_vars = geometry,
#                 year=year) |> 
#     mutate(geotype="place") |> 
#     rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
#   
#   schools <- get_acs(geography="school district (unified)",
#                 state="NY",
#                 geometry = geometry,
#                 table=acstab,
#                 year=year) |> 
#     mutate(geotype="schooldist")  |> 
#     rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
#   
#   # dfzip <- get_acs(geography="zcta",
#   #               state="NY",
#   #               # zcta=c("12816", "12834"),
#   #               zcta=c(12816, 12834),
#   #               table=acstab,
#   #               year=2019) # doesn't work for 2020 or 2021
#     
#     df1 <- bind_rows(nation, states, counties, cousubs, places, schools)
# 
#     # names2 <- names(df1)
#     # names2 <- str_replace(names2, "NAME.x", "NAME")
#     
#     df2 <- df1 |>
#       as_tibble() |> # important so that we can work with columns
#       lcnames() |> 
#       rename(basename=name) |> 
#       mutate(shortname=str_extract_before_first(basename, ","),
#              endyear=!!year, table=acstab, survey="acs5") |> 
#       relocate(shortname, .after=fullname)
#     
#    # define variables to keep, depending on status of geometry
#     # keepxgeo <- quote(c(geotype, year, geoid, geoname=name, variable, estimate, moe))
#     # keepgeo <- quote(c(geotype, year, geoid, geoname=name, variable, estimate, moe, geometry))
#     # if(geometry) {
#     #   keepvars <- keepgeo
#     #   } else keepvars <- keepxgeo
# 
#     # df <- df2 |> 
#     #   select(!!keepvars)
#     
#     return(df2)
# }
# 
# # NAMELSAD:  concatenated variable length geographic area name and legal/statistical area description (LSAD) (e.g., Atlanta city)
# 
# # tmp2 <- tmp |> filter(shortname != geoname)
# # 
# # tmp1 <- get_acstab(acstab="B01003", year=2021, geometry=FALSE)
# # tmp2 <- get_acstab(acstab="B01003", year=2021, geometry=TRUE)
# # 
# # ns(tmp)
# # ns(tmp2)
# # tmp1n <- tmp1 |> select(geoid, geotype, contains("name"))
# # tmp2n <- tmp2 |> select(geoid, geotype, contains("name"))

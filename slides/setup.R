

# libraries ---------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(usmap)
library(vroom)

tprint <- 75
options(tibble.print_max = tprint, tibble.print_min = tprint) 


# folders -----------------------------------------------------------------

ddir <- here::here("data")
tdir <- fs::path(ddir, "acs_tables")
popdir <- fs::path(ddir, "population")


# plot items: freestanding ----------------------------------------------------
legend_none <- theme(legend.position = "None")
legend_notitle <- theme(legend.title = element_blank())
caption_left <- theme(plot.caption = element_text(hjust = 0))
x90 <- theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5))


# constants geo -----------------------------------------------------------

# examine geograhies to define areas to focus on
# geographies <- readRDS(fs::path(ddir, "acs_geographies.rds"))

nycos <- paste0("36", c("001", "083", "091", "093", "113", "115"))
vtcos <- "50003"
keep_cos <- c(nycos, vtcos)

keep_places <- c("3602550", "3611825", "3630675", "3664771", "3665750", "3635474")

keep_cousubs <- c("3611511836", "3611530686", "3611564782", "3611581578", "3609165244", "3611502561", "3608335463", "3611538143")
# geographies |> filter(geoid %in% keep_cousubs)
 
constants <- list()
constants$keep_counties <- keep_cos
constants$keep_places <- keep_places
constants$keep_cousubs <- keep_cousubs
constants$comp_county <- "36115"
constants$comp_place <- "3611825"
constants$comp_cousub <- "3611511836"

rm(keep_cos, keep_places, keep_cousubs, nycos, vtcos)


# functions ---------------------------------------------------------------

barline <- function(gtype, barvar, data,
                     constants,
                     ybreaks = ybreaks, 
                     titlebase = NULL, 
                     ylab = NULL){
  # data must have variable, geotype, geoid, trimname, estimate
  
  if(gtype == "county"){
    keepgeos <- constants$keep_counties
    compgeo <- constants$comp_county
    title <- paste0(titlebase, ", selected area counties")
  } else if(gtype == "place"){
    keepgeos <- constants$keep_places
    compgeo <- constants$comp_place
    title <- paste0(titlebase, ", selected area places")
  } else if(gtype == "cousub"){
    keepgeos <- constants$keep_cousubs
    compgeo <- constants$comp_cousub
    title <- paste0(titlebase, ", selected area towns")
  } else {
    stop("gtype must be county, place, or cousub")
  }
  
  data <- data |> 
    filter(variable == barvar, geotype == gtype, geoid %in% keepgeos) |> 
    mutate(compgeo=geoid==compgeo)
  
  yint <- data |> filter(compgeo) |> pull(estimate)
  
  capt1 <- "Source: U.S. Census Bureau, 2019-2023 American Community Survey"
  capt2 <- NULL
  if(gtype %in% c("place", "cousub")) capt2 <- "Caution: Village and town data may be subject to considerable sampling error."
  capt <- paste0("\n", capt1, "\n", capt2)
  
  data |> 
    ggplot(aes(x = reorder(trimname, estimate), y = estimate, fill = compgeo)) +
    geom_col(width = 0.2) +
    scale_fill_manual(values = c("lightblue", "blue")) +
    geom_hline(yintercept = yint, color = "red", linetype = "solid", linewidth = .75) +
    scale_y_continuous(name=ylab, 
                       labels = scales::label_comma(),
                       breaks = ybreaks) +
    labs(title = title,
       subtitle = NULL,
       x = NULL) +
    theme_minimal() +
    theme(legend.position = "None") +
    labs(caption = capt) +
    caption_left
}

# https://bookdown.org/yihui/rmarkdown-cookbook/reuse-chunks.html

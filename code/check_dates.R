library(tidyverse)
library(haven)
library(janitor)
library(lubridate)
library(labelled)
library(sjlabelled)

ess_path <- 'data/ess_data/'

files <- list.files(ess_path)
files

ess_list <- list()


# -----------------------------------------------------------------------------
# Load all ESS data sets
# -----------------------------------------------------------------------------

for (j in seq_along(files)) {
  
  path <- files[j]
  path <- paste(ess_path, path, sep = "")
  print(path)
  
  # read data
  data <- read_dta(path, encoding = "latin1")
  
  # remove labels from ID variables
  var_label(data$cntry) <- NULL
  var_label(data$idno) <- NULL
  
  ess_list[[j]] <- data
  rm(data)
  
}


# ----- Unpack data list
ess_data <- ess_list %>% 
  bind_rows(., .id = "ess_id")

object.size(ess_data) / 10^9


# -----------------------------------------------------------------------------
# Load ESS dates
# -----------------------------------------------------------------------------
dates <- readRDS("data/ess_dates.rds")



# -----------------------------------------------------------------------------
# Merge data
# -----------------------------------------------------------------------------
ess <- ess_data %>% 
  select(name, cntry, idno)

ess <- ess %>% 
  tidylog::left_join(.,
                     dates,
                     by = c('name', 'cntry', 'idno'))


ess <- ess %>% 
  #mutate(date = yday(start_date))
  mutate(date = as.Date(start_date)) %>% 
  mutate(date = format(date, format="%m-%d"))

ess <- ess %>% 
  #mutate(t = as.character(date)) %>% 
  mutate(t = lubridate::mday(t))


ess <- ess %>% 
  mutate(x = format(as.Date(start_date, "%m-%d-%y"), "%m-%d"))

ess %>% 
  ggplot(., aes(x = x)) +
  geom_density() +
  #scale_x_date() +
  facet_wrap(~ name)

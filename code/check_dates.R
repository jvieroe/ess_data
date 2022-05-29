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
  filter(!is.na(start_date))



# -----------------------------------------------------------------------------
# Plot data
# -----------------------------------------------------------------------------

ess <- ess %>% 
  mutate(date = format(start_date, format="%m-%d")) %>% 
  mutate(date = as.character(date)) %>% 
  mutate(date = paste('2000', date, sep = '-')) %>% 
  mutate(date = ymd(date))
  

# ----- Density plot
ess %>% 
  ggplot(., aes(x = date)) +
  geom_density() +
  scale_x_date(date_breaks = '2 months',
               date_labels = '%b') +
  facet_wrap(~ name)

ggsave(plot = last_plot(),
       filename = 'output/date_distribution_density.png')



# ----- Histogram
ess %>% 
  ggplot(., aes(x = date)) +
  geom_histogram() +
  scale_x_date(date_breaks = '2 months',
               date_labels = '%b') +
  facet_wrap(~ name)

ggsave(plot = last_plot(),
       filename = 'output/date_distribution_histogram.png')

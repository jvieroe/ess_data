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
  
}




# -----------------------------------------------------------------------------
# Prepare ESS round 1
# -----------------------------------------------------------------------------
ess_1 <- ess_list[[1]]

temp <- ess_1 %>% 
  select(c(inwyr, inwmm, inwdd,
           # start
           inwshh, inwsmm,
           # end
           inwehh, inwemm,
           # IDs
           cntry, idno)) %>% 
  label_to_colnames() %>% 
  clean_names() %>% 
  mutate(start_of_interview_year = year_of_interview,
         end_of_interview_year = year_of_interview,
         start_of_interview_month = month_of_interview,
         end_of_interview_month = month_of_interview,
         start_of_interview_day_of_month = day_of_month_of_interview,
         end_of_interview_day_of_month = day_of_month_of_interview) %>% 
  select(cntry,
         idno,
         start_of_interview_year,
         start_of_interview_month,
         start_of_interview_day_of_month,
         start_of_interview_hour,
         start_of_interview_minute,
         end_of_interview_year,
         end_of_interview_month,
         end_of_interview_day_of_month,
         end_of_interview_hour,
         end_of_interview_minute)


ess_1 <- ess_1 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyr, inwmm, inwdd,
            inwshh, inwsmm,
            inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 2
# -----------------------------------------------------------------------------
ess_2 <- ess_list[[2]]

temp <- ess_2 %>% 
  select(c(inwyr, inwmm, inwdd,
           # start
           inwshh, inwsmm,
           # end
           inwehh, inwemm,
           # IDs
           cntry, idno)) %>% 
  label_to_colnames() %>% 
  clean_names() %>% 
  mutate(start_of_interview_year = year_of_interview,
         end_of_interview_year = year_of_interview,
         start_of_interview_month = month_of_interview,
         end_of_interview_month = month_of_interview,
         start_of_interview_day_of_month = day_of_month_of_interview,
         end_of_interview_day_of_month = day_of_month_of_interview) %>% 
  select(cntry,
         idno,
         start_of_interview_year,
         start_of_interview_month,
         start_of_interview_day_of_month,
         start_of_interview_hour,
         start_of_interview_minute,
         end_of_interview_year,
         end_of_interview_month,
         end_of_interview_day_of_month,
         end_of_interview_hour,
         end_of_interview_minute)


ess_2 <- ess_2 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyr, inwmm, inwdd,
            inwshh, inwsmm,
            inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 3
# -----------------------------------------------------------------------------
ess_3 <- ess_list[[3]]

temp <- ess_3 %>% 
  select(c(cntry, idno,
           # start
           inwyys, inwmms, inwdds, inwshh, inwsmm,
           # end
           inwyye, inwmme, inwdde, inwehh, inwemm)) %>% 
  label_to_colnames() %>% 
  clean_names()


ess_3 <- ess_3 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyys, inwmms, inwdds, inwshh, inwsmm,
            inwyye, inwmme, inwdde, inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 4
# -----------------------------------------------------------------------------
ess_4 <- ess_list[[4]]

temp <- ess_4 %>% 
  select(c(cntry, idno,
           # start
           inwyys, inwmms, inwdds, inwshh, inwsmm,
           # end
           inwyye, inwmme, inwdde, inwehh, inwemm)) %>% 
  label_to_colnames() %>% 
  clean_names()


ess_4 <- ess_4 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyys, inwmms, inwdds, inwshh, inwsmm,
            inwyye, inwmme, inwdde, inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 5
# -----------------------------------------------------------------------------
ess_5 <- ess_list[[5]]

temp <- ess_5 %>% 
  select(c(cntry, idno,
           # start
           inwyys, inwmms, inwdds, inwshh, inwsmm,
           # end
           inwyye, inwmme, inwdde, inwehh, inwemm)) %>% 
  label_to_colnames() %>% 
  clean_names()


ess_5 <- ess_5 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyys, inwmms, inwdds, inwshh, inwsmm,
            inwyye, inwmme, inwdde, inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 6
# -----------------------------------------------------------------------------
ess_6 <- ess_list[[6]]

temp <- ess_6 %>% 
  select(c(cntry, idno,
           # start
           inwyys, inwmms, inwdds, inwshh, inwsmm,
           # end
           inwyye, inwmme, inwdde, inwehh, inwemm)) %>% 
  label_to_colnames() %>% 
  clean_names()


ess_6 <- ess_6 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyys, inwmms, inwdds, inwshh, inwsmm,
            inwyye, inwmme, inwdde, inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 7
# -----------------------------------------------------------------------------
ess_7 <- ess_list[[7]]

temp <- ess_7 %>% 
  select(c(cntry, idno,
           # start
           inwyys, inwmms, inwdds, inwshh, inwsmm,
           # end
           inwyye, inwmme, inwdde, inwehh, inwemm)) %>% 
  label_to_colnames() %>% 
  clean_names()


ess_7 <- ess_7 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyys, inwmms, inwdds, inwshh, inwsmm,
            inwyye, inwmme, inwdde, inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 8
# -----------------------------------------------------------------------------
ess_8 <- ess_list[[8]]

temp <- ess_8 %>% 
  select(c(cntry, idno,
           # start
           inwyys, inwmms, inwdds, inwshh, inwsmm,
           # end
           inwyye, inwmme, inwdde, inwehh, inwemm)) %>% 
  label_to_colnames() %>% 
  clean_names()


ess_8 <- ess_8 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyys, inwmms, inwdds, inwshh, inwsmm,
            inwyye, inwmme, inwdde, inwehh, inwemm))


# -----------------------------------------------------------------------------
# Prepare ESS round 9
# -----------------------------------------------------------------------------
ess_9 <- ess_list[[9]]

temp <- ess_9 %>% 
  select(c(cntry, idno,
           # start
           inwyys, inwmms, inwdds, inwshh, inwsmm,
           # end
           inwyye, inwmme, inwdde, inwehh, inwemm)) %>% 
  label_to_colnames() %>% 
  clean_names()


ess_9 <- ess_9 %>% 
  tidylog::left_join(.,
                     temp,
                     by = c("cntry", "idno")) %>% 
  select(-c(inwyys, inwmms, inwdds, inwshh, inwsmm,
            inwyye, inwmme, inwdde, inwehh, inwemm))




# -----------------------------------------------------------------------------
# Append data
# -----------------------------------------------------------------------------
df_ <- bind_rows(ess_1,
                 ess_2,
                 ess_3,
                 ess_4,
                 ess_5,
                 ess_6,
                 ess_7,
                 ess_8,
                 ess_9)

ess <- df_

ess <- ess %>% 
  select(names(temp))


ess <- ess %>% 
  filter(!is.na(start_of_interview_year),
         !is.na(start_of_interview_month),
         !is.na(start_of_interview_day_of_month))

ess <- ess %>% 
  mutate(start_date = paste(start_of_interview_year,
                            start_of_interview_month,
                            start_of_interview_day_of_month,
                            sep = '-')) %>% 
  mutate(end_date = paste(end_of_interview_year,
                          end_of_interview_month,
                          end_of_interview_day_of_month,
                          sep = '-'))

ess <- ess %>% 
  mutate(start_time = paste(start_of_interview_hour,
                            start_of_interview_minute,
                            sep = ':')) %>% 
  mutate(end_time = paste(end_of_interview_hour,
                          end_of_interview_minute,
                          sep = ':'))

ess <- ess %>% 
  mutate(start_full = paste(start_date, start_time,
                            sep = ' ')) %>% 
  mutate(end_full = paste(end_date, end_time,
                          sep = ' '))


ess <- ess %>% 
  mutate(across(ends_with('_date'),
                ~ lubridate::ymd(.x),
                .names = "{.col}_new"))

library(tidyverse)
library(janitor)
library(here)

here::here()

# 1 - read the data ------------------------------------------------------
decathlon <- readr::read_rds(here::here("raw_data/decathlon.rds"))



# 2 - change the row names to numbers -----------------------------------------
decathlon$athlete_name <- rownames(decathlon)
rownames(decathlon) <- 1:nrow(decathlon)

# Comments: with this change , instead to has 13 columns/variables has 14. 
#           This happen because the the names of athletes that initially were 
#           in the row names turned into a new column called athlete_name.



# 3 - manage names ------------------------------------------------------------
## 3.1 - clean names ----------------------------------------------------------
decathlon_janitor_clean <- decathlon %>% 
  clean_names()

names(decathlon_janitor_clean)

## 3.2 - rename the names -----------------------------------------------------
decathlon_renamed <- decathlon_janitor_clean %>% 
  rename(
    "100_m"        = "x100m",
    "400_m"        = "x400m",
    "110_m_hurdle" = "x110m_hurdle",
    "1500_m"       = "x1500m"
  )



#4 - study the data ----------------------------------------------------------
glimpse(decathlon_renamed)
summary(decathlon_renamed)

decathlon_renamed %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

# Comments: There are not Missing values even values not confirmed as expected,
#           not extra treatment is needed.            



# 5 - manage variable athlete_name -------------------------------------------
## 5.1 - first letter Caps and all others lower ------------------------------
decathlon_athlete_name <- decathlon_renamed %>% 
  mutate(athlete_name = str_to_sentence(athlete_name))

## 5.2 - change the order of athlete variable --------------------------------
decathlon_relocate <- decathlon_athlete_name %>% 
  relocate(athlete_name,.before = `100_m`)



# 6 - Turm the table longer -----------------------------------------------
decathlon_tidy_longer <- decathlon_relocate %>% 
  pivot_longer(cols      = c(2:11),
               names_to  = "event",
               values_to = "score"
  )

# Comments: All events of decatlhon will be in the same variable(event), and 
#           the score for each event will be in the score's variable.
#           The table managed (decathlon_relocate) had 14 variables, but turns 
#           with just 6 columns. Since column 2 to 11 were joined in a variable 
#           called event.



# 7 - write data cleaned in a csv file ----------------------------------------
decathlon_clean <- decathlon_tidy_longer

decathlon_clean %>% 
  write.csv("clean_data/decathlon_clean.csv")

library(tidyverse)
library(tidyr)
library(readxl)
library(janitor)

# 1 - read the data -----------------------------------------------------------
candy_data_2015 <- read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
candy_data_2016 <- read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
candy_data_2017 <- read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))



# 2 - clean_names() & remove_empty rows and cols-------------------------------
# Through the janitor package, first the variables were turned for a more 
# standard way using function 'clean_names()', then to remove entire empty 
# rows and columns was used the function 'remove_empty()'.
candy_data_2015_janitor <- candy_data_2015 %>%
  clean_names() %>% 
  remove_empty(c("rows", "cols"))
  
candy_data_2016_janitor <- candy_data_2016 %>%
  clean_names() %>% 
  remove_empty(c("rows", "cols")) 

candy_data_2017_janitor <- candy_data_2017 %>%
  clean_names() %>% 
  remove_empty(c("rows", "cols"))



# 3 - first look for datasets -------------------------------------------------
### 3.1 - Missing values --------------------------------------------------
candy_data_2015_na <- candy_data_2015_janitor %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

candy_data_2016_na <- candy_data_2016_janitor %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

candy_data_2017_na <- candy_data_2017_janitor %>% 
  summarise(across(.fns = ~sum(is.na(.x))))


### 3.2 - glimpse(), summary()  ----------------------------------------------
glimpse(candy_data_2015_janitor)
glimpse(candy_data_2016_janitor)
glimpse(candy_data_2017_janitor)

summary(candy_data_2015_janitor)
summary(candy_data_2016_janitor)
summary(candy_data_2017_janitor)



# 4 - rename variables starts by letter 'q' -----------------------------------
# Many variables started by letter 'q' followed the a number and then _.
# Will be removed from all variables the pattern definied (e.g. 'q6_').
names(candy_data_2017_janitor) <- 
  str_remove(names(candy_data_2017_janitor), '^q[0-9]*_')



# 5 - manage & compare variables of each dataset ------------------------------
 # First comparison between the all variables from each dataset to see which 
 # to try to answer questions as:
 #  - Are there variables with similar names that do not match in all datasets?
 #  - Are there a commum variables that are irrelevant for us by now and it is 
 #    possible to a pattern do delete?
compare_three_dataset <- compare_df_cols(candy_data_2015_janitor,
                                         candy_data_2016_janitor,
                                         candy_data_2017_janitor)



## 5.1 - variables w/ similar names that don't match in all datasets ----------
 # Were identified variables that have the different names and the same 
 # meaning and are relevant. Could be important to do not have duplicates 
 # when join the different datasets.
 # Variable agr there is in data from 2017 already but as character variable, 
 # so will be changed by numeric.
candy_data_2015_rename <- candy_data_2015_janitor %>%
  rename(
    "age"           = "how_old_are_you",
    "going_out"     = "are_you_going_actually_going_trick_or_treating_yourself",
    "100_grand_bar" = "x100_grand_bar",
    "box_raisins"   = "box_o_raisins"
  )

candy_data_2016_rename <- candy_data_2016_janitor %>%
  rename(
    "age"           = "how_old_are_you",
    "gender"        = "your_gender",
    "country"       = "which_country_do_you_live_in",
    "going_out"     = "are_you_going_actually_going_trick_or_treating_yourself",
    "100_grand_bar" = "x100_grand_bar",
    "box_raisins"   = "boxo_raisins"
  )

candy_data_2017_rename <- candy_data_2017_janitor %>%
  rename(
    "box_raisins" = "boxo_raisins"
  )


## 5.2 - Create a variable called year in each dataset -------------------------
 # To identify each dataset after combine them all in just one, a variable 
 # called year was created in all datasets. In dataset od 2015 and 2016 throught
 # a pattern was extracted the year from timestamp variable. The dataset of 2017 
 # does not has any variable where it's possible to extract the correspondent 
 # year, so we create one manually without information extraction from other 
 # variables.
pattern_year  <- '^[0-9]{4}'
candy_data_2015_year <- candy_data_2015_rename %>% 
  mutate(year  = as.numeric(str_extract(timestamp, pattern_year)), .before = 1)

candy_data_2016_year <- candy_data_2016_rename %>% 
  mutate(year  = as.numeric(str_extract(timestamp, pattern_year)), .before = 1)

candy_data_2017_year <- candy_data_2017_rename %>% 
  mutate(year = as.numeric(2017), .before = 1)



#rm point------
rm(
  candy_data_2015,
  candy_data_2016,
  candy_data_2017,
  candy_data_2015_na,
  candy_data_2016_na,
  candy_data_2017_na,
  candy_data_2015_janitor,
  candy_data_2016_janitor,
  candy_data_2017_janitor,
  candy_data_2015_rename,
  candy_data_2016_rename,
  candy_data_2017_rename,
  compare_three_dataset
)


# 6 - combine the three datasets together ------------------------------------
candy_data_2015_to_2017 <- bind_rows(candy_data_2015_year,
                                     candy_data_2016_year,
                                     candy_data_2017_year)



## 6.1 - remove/delete variables from combined dataset -------------------
 # For a better understanding of variables we still have to deal, was build a 
 # table with the variables names, this way it is possible so sort by alfabetic
 # order, or filter names...
names_table_2015_to_2017 <- tibble(names(candy_data_2015_to_2017))


### 6.1.1 - remove variables with a certain pattern -----------------------------
 # Variables that starts with a certain word will not be considered (e.g. which,
 # media, please...). There are some words like 'bread', 'other', 
 # 'housewives' or 'sweetums', independently  the order that comes in the  
 # variable, will not be considered too. This pattern will be saved in a
 # vector called 'pattern_to_remove' to be used further in select().
pattern_remove <- str_c(
  "^which_[0-9a-z_]+",
  "^when_[0-9a-z_]+",
  "^what_[0-9a-z_]+",
  "^please_[0-9a-z_]+",
  "^click_[0-9a-z_]+",
  "^fill_[0-9a-z_]+",
  "^if_[0-9a-z_]+", 
  "^check_[0-9a-z_]+",
  "^media_[0-9a-z_]+",
  "^that_[0-9a-z_]+",
  "^those_[0-9a-z_]+",   
  "^guess_[0-9a-z_]+",
  "^whole_[0-9a-z_]+",
  "^person_[0-9a-z_]+",
  "^any_[0-9a-z_]+",
  "^sandwich[0-9a-z_]+",
  "^do_you_[0-9a-z_]+",
  "^anonymous[0-9a-z_]+",
  "^state_[0-9a-z_]+",
  "[0-9a-z_]*sweetums*[0-9a-z_]+",
  "[0-9a-z_]*bread*[0-9a-z_]+",
  "[0-9a-z_]*other*[0-9a-z_]+",
  "[0-9a-z_]*housewives*[0-9a-z_]+",
  sep = "|")

pattern_to_remove <- c(
  names(candy_data_2015_to_2017) %>% 
    str_extract(pattern_remove) %>% 
    na.omit()
)



### 6.1.2 - select specific variables to be removed ---------------------------
 # Create a vector called variables_to_remove that will have variables we would
 # like to remove. This vector will be used further in select() function.

variables_to_remove <- c(
  "timestamp",
  "vials_of_pure_high_fructose_corn_syrup_for_main_lining_into_your_vein",
  "brach_products_not_including_candy_corn",
  "candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants",
  "chardonnay",                #https://en.wikipedia.org/wiki/Chardonnay
  "cash_or_other_forms_of_legal_tender",
  "chick_o_sticks_we_don_t_know_what_that_is",
  "creepy_religious_comics_chick_tracts", #https://en.wikipedia.org/wiki/Chick_tract
  "internal_id",
  "bottle_caps",
  "take_5",
  "dress",
  "x114",
  "day",
  "real_housewives_of_orange_county_season_9_blue_ray",
  "vicodin",                  #https://www.drugs.com/vicodin.html 
  "generic_brand_acetaminophen", #https://www.drugs.com/ingredient/acetaminophen.html
  "abstained_from_m_ming",    #google search inconclusive
  "betty_or_veronica",        #https://en.wikipedia.org/wiki/Betty_and_Veronica
  "bonkers_the_board_game",    #https://en.wikipedia.org/wiki/Bonkers!_(game)
  "sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year"
)



## 6.2 - dataset without deleted variables ----------------------------------
candy_data_2015_to_2017_remove <- candy_data_2015_to_2017 %>% 
  select(-pattern_to_remove, -variables_to_remove)



#rm point------
rm(
  candy_data_2015_year,
  candy_data_2016_year,
  candy_data_2017_year,
  names_table_2015_to_2017,
)


# 7 -  relocate variables ----------------------------------------------------
 # The first variables of our dataset for this following order, should be: 
 # year, age, gender, country and going_out.
candy_data_2015_to_2017_relocate <- candy_data_2015_to_2017_remove %>% 
  relocate(age, 
           gender,
           country,
           going_out,
           .after = year)
 
 
# 8 - recode & turn 'going_out' variable as boolean  ------------------------
 # The variable going_out was turned in a boolean instead a character variable.
 # Instead of have 'Yes' or 'No', have TRUE and FALSE. 
candy_data_2015_to_2017_recode <- candy_data_2015_to_2017_relocate %>% 
  mutate(going_out = recode(going_out,
                            'Yes'    = TRUE,
                            'No'     = FALSE,
                            .default = NA)
  )


# 9 - clean age variable  ----------------------------------------------------
 # Some waste values like values with letters, mix with numbers and pontuations,
 # values extremely highers or lowers...
 # The variable was turned in numeric instead character, and trought 'if_else()'
 # function was created a condition to filter what should be considered as a 
 # age or turned as NA (were generated 182 NA).
candy_data_2015_to_2017_age <- candy_data_2015_to_2017_recode %>% 
  mutate(
    age = as.numeric(
      if_else(
        nchar(age) <= 4 & str_detect(age, '^[0-9]')==TRUE & between(age, 5, 80),
        age,            #if is TRUE
        NA_character_   #if is FALSE)
        ))
    )


# 10 - clean country variable ---------------------------------------------
  # Any value that starts for a number will be replaced as NA
candy_data_2015_to_2017_country <- candy_data_2015_to_2017_age %>%
  mutate(country = if_else(str_detect(country, '^[0-9]') == FALSE,
                           str_to_sentence(country),
                           NA_character_)
  )

candy_data_2015_to_2017_uk <- candy_data_2015_to_2017_country %>%
  mutate(country = recode(country, 
                          "United kingdom" = "UK",
                          "United kindom"  = "UK",
                          "U.k."           = "UK",
                          "England"        = "UK",
                          "Endland"        = "UK",
                          "Uk"             = "UK"))

candy_data_2015_to_2017_canada <- candy_data_2015_to_2017_uk %>%
  mutate(country = recode(country, 
                          "Canada"  = "Canada",
                          "Canada`" = "Canada",
                          "Can."    = "Canada"))


candy_data_2015_to_2017_usa <- candy_data_2015_to_2017_canada %>%
  mutate(country = recode(country, 
                          "Usa" = "USA",
                          "Usa!" = "USA",
                          "Usa (i think but it's an election year so who can really tell)" = "USA",
                          "Usa usa usa" = "USA",
                          "The best one - usa" = "USA",
                          "Usa! Usa! Usa!" = "USA",
                          "Usa!!!!!!" = "USA",
                          "Usa! Usa!"= "USA",
                          "Not the usa or canada"= "USA",
                          "Usa usa usa usa"= "USA",
                          "Usausausa"= "USA",
                          "Usa? Hard to tell anymore.."= "USA",
                          "Usas"= "USA",
                          "Usaa"= "USA",
                          "Usa usa usa!!!!"= "USA",
                          "United states of america"= "USA",
                          "United states"= "USA",
                          "United kingdom"= "USA",
                          "Units states"= "USA",
                          "United kindom"= "USA",
                          "United sates"= "USA",
                          "United stetes"= "USA",
                          "United  states of america"= "USA",
                          "United state"= "USA",
                          "United staes"= "USA",
                          "Unites states"= "USA",
                          "The united states"= "USA",
                          "The united states of america"= "USA",
                          "Unite states"= "USA",
                          "I pretend to be from canada, but i am really from the united states."= "USA",
                          "United stated"= "USA",
                          "United ststes"= "USA",
                          "United statss"= "USA",
                          "United statea"= "USA",
                          "United states of america"= "USA",
                          "America"= "USA",
                          "United  states of america"= "USA",
                          "The united states of america"= "USA",
                          "N. America"= "USA",
                          "Us" = "USA",
                          "Ussa" = "USA",
                          "Us of a" = "USA",
                          "U s a"= "USA",
                          "U s"= "USA",
                          "Unhinged states"= "USA",
                          "Unied states"= "USA",
                          "Eua"= "USA",
                          "U.s."= "USA",
                          "U.s.a."= "USA"
                          ))



# 11 - turn the table in long format long - pivot_longer() --------------------
## 11.1 - create an ID for each observation -----------------------------------
candy_data_2015_to_2017_id <- candy_data_2015_to_2017_usa %>% 
  mutate(id = str_c(year, 
                    c(1:nrow(candy_data_2015_to_2017_usa)),
                    sep = ""), 
         .before = 1)

## 11.2 - pivot_longer() ------------------------------------------------------
candy_data_2015_to_2017_longer <- candy_data_2015_to_2017_id %>% 
  pivot_longer(c(7:103),
               names_to = "candy",
               values_to = "answer")


# 12 - write csv file -----------------------------------------------------
candy_data_2015_to_2017_longer %>% 
  write.csv("clean_data/candy_data_clean.csv")


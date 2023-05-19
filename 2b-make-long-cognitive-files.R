################################################################################

# depends on: 
# 1-item_formats_09_12_15.csv
# 2a-student_index_standard.Rds
# INT_COG09_S_SPSS_DEC11.sav
# INT_COG09_TD_SPSS_DEC11.sav
# INT_COG12_S_DEC03.sav
# INT_COG12_DEC03.sav
# CY6_MS_CMB_STU_COG.sav

# produces:
# 2b-temp-2009-long.Rds
# 2b-temp-2012-long.Rds
# 2b-temp-2015-long.Rds

################################################################################

library(tidyverse)
library(haven)
library(labelled)
library(kableExtra)

rm(list=ls())

# set directory with raw PISA SPSS files


################################################################################
# functions #####

user_na_zap <- function(x) {zap_labels(user_na_to_na(x))}

# functions for scoring the 2015 responses:

scored <- function(x) {
  x <- as.character(as_factor(x))
  x <- ifelse(grepl("Full credit",x), "full", x)
  x <- ifelse(grepl("Partial credit",x), "part", x)
  x <- ifelse(grepl("No credit",x), "none", x)
  x <- ifelse(x %in% c("Valid Skip", "Not Applicable"), NA, x)
  x <- ifelse(x %in% c("No Response", "Invalid"), "none", x)
  return(x)
}

# documentation is unclear about "Not Applicable" for 2015. How is this different from Invalid?
# Not applicable indicates that a response was provided even though the response to an earlier 
# question should have directed the respondent to skip that question, or the response could not 
# be determined due to a printing problem or torn booklet. In the questionnaire data, it is also 
# used to indicate missing by design (i.e. the respondent was never given the opportunity to see this question).

scored_numeric <- function(x) {
  # poly
  if("part" %in% unique(x)) {
    x <- ifelse(x=="none","0",x)
    x <- ifelse(x=="part","1",x)
    x <- ifelse(x=="full","2",x)
    x <- ifelse(x=="Not Reached","8",x)
  }
  # binary
  if(!("part" %in% unique(x))) {
    x <- ifelse(x=="none","0",x)
    x <- ifelse(x=="full","1",x)
    x <- ifelse(x=="Not Reached","8",x)
  }
  return(as.numeric(x))
}


################################################################################
# load item names #####

items <- read.csv(file = "1-item_formats_09_12_15.csv")
items <- select(items, year, subject, item_id,
                item_format, 
                item_id_short, int_pval, is_mc, is_poly,
                process_short, subscale, text_format)

glimpse(items)

items09 <- items %>% filter(year==2009) %>% select(item_id) %>% as_vector() %>% as.character()
items12 <- items %>% filter(year==2012) %>% select(item_id) %>% as_vector() %>% as.character()
items15 <- items %>% filter(year==2015) %>% select(item_id) %>% as_vector() %>% as.character()

items <- items %>%
  rename(
    YEAR=year,
    SUB=subject
  )


################################################################################
# load student index #####

stuindex <- readRDS(file = "2a-student_index_standard.Rds") %>%
  mutate(inIndex = 1)


################################################################################
# process cognitive response files #####

# these will be ID variables in long format files

idvars <- c("YEAR", "CNT", "OECD", "SCHOOLID", "STUID", "AGE","GRADE",
            "BOOKID", "TESTLANG", "ADMINMODE", "Option_Read", "Option_Math",
            "Admin_Std", "GENDER")

## 2009 #####

### load data #####

# For item responses in scored file, skipped or invalid items are 
# already scored as incorrect.

# Item scores:
# 7 = N/A are not administered and get converted to N/A by user_na_zap()
# 8 = not reached

c09 <- read_spss(file = "pisa2009/data/INT_COG09_S_SPSS_DEC11.sav", user_na=TRUE)

table(c09$M033Q01, useNA="a")
val_labels(c09$M033Q01)

c09 <- c09 %>%
  rename(
    STUID=StIDStd) %>%
  mutate(across(everything(), user_na_zap)) %>%
  mutate(YEAR=2009) %>%
  select(YEAR, CNT, SCHOOLID, STUID, BOOKID, all_of(items09))

for (i in items09) {
  c09[,i] <- as.numeric(as_vector(c09[,i]))
}

table(c09$M033Q01, useNA="a")

### append student covariates #####

nrow(c09)
c09 <- left_join(c09, stuindex, by = c("YEAR","CNT","SCHOOLID","STUID","BOOKID")) %>%
  filter(inIndex==1) %>%
  select(-inIndex)
c09 %>%
  group_by(CNT, SCHOOLID, STUID) %>%
  summarise(N=n()) %>%
  filter(N>1)
nrow(c09)

### convert to long format #####

long09 <- c09 %>%
  pivot_longer(cols = -all_of(idvars), names_to = "item_id", values_to = "resp") %>%
  mutate(SUB = str_sub(item_id,1,1))
glimpse(long09)
gc()

### add raw responses to record skips #####

# Create long file with following responses:
# - "answer" = student answered (need to refer to scored file to determine correct/not)
# - "NA" = N/A as in not administered, missing by design
# - "skip" = invalid or missing response (codes 8 and 9)
# - "nr" = not reached (code r)

noscore09 <- read_spss(file = "pisa2009/data/INT_COG09_TD_SPSS_DEC11.sav",
                       user_na=TRUE)

noscore09 <- noscore09 %>%
  rename(
    STUID=StIDStd) %>%
  mutate(YEAR=2009) %>%
  select(CNT, SCHOOLID, STUID, BOOKID, all_of(items09))

for (i in items09) {
  noscore09[,i] <- to_factor(noscore09[,i])
}

noscore09 <- pivot_longer(noscore09, all_of(items09))

noscore09 <- noscore09 %>%
  mutate(
    resp_type = "answer",
    resp_type = ifelse(value %in% c("Invalid","Missing","8"), "skip", resp_type),
    resp_type = ifelse(value %in% c("Not reached"), "nr", resp_type),
    resp_type = ifelse(value %in% c("N/A"), "NA", resp_type)
  )

with(noscore09, table(value, resp_type, useNA="a"))

noscore09 <- noscore09 %>% zap_labels() %>% rename(item_id=name, raw_resp=value)

gc()

### merge #####

nrow(long09)
long09 <- left_join(long09, noscore09)
nrow(long09)
with(long09, table(resp_type, resp, useNA="a"))

### Save file #####

nrow(long09)
long09 <- filter(long09, !is.na(resp))
nrow(long09)
gc()
saveRDS(long09, file = "2b-temp-2009-long.Rds")
rm(c09, long09, noscore09)
gc()




## 2012 #####

### load data #####

# Item scores:
# 7 = N/A are not administered
# 8 = not reached

c12 <- read_spss(file = "pisa2012/data/INT_COG12_S_DEC03.sav", user_na=TRUE)

table(c12$PM00FQ01, useNA="a")
val_labels(c12$PM00FQ01)

c12 <- c12 %>% 
  rename(
    STUID=StIDStd) %>%
  mutate(across(everything(), user_na_zap)) %>%
  mutate(YEAR=2012) %>%
  select(YEAR, CNT, SCHOOLID, STUID, BOOKID, all_of(items12))

table(c12$PM00FQ01, useNA="a")

for (i in items12) {
  c12[,i] <- as.numeric(as_vector(c12[,i]))
}

table(c12$PM00FQ01, useNA="a")

### append student covariates #####

nrow(c12)
c12 <- left_join(c12, stuindex, by = c("YEAR","CNT","SCHOOLID","STUID","BOOKID")) %>%
  filter(inIndex==1) %>%
  select(-inIndex)

c12 %>%
  group_by(CNT, SCHOOLID, STUID) %>%
  summarise(N=n()) %>%
  filter(N>1)
nrow(c12)

### convert to long #####

long12 <- c12 %>%
  pivot_longer(cols = -all_of(idvars), names_to = "item_id", values_to = "resp") %>%
  mutate(SUB = str_sub(item_id,2,2))
glimpse(long12)
gc()

### add raw responses to record skips #####

noscore12 <- read_spss(file = "pisa2012/data/INT_COG12_DEC03.sav",
                       user_na=TRUE)

noscore12 <- noscore12 %>%
  rename(
    STUID=StIDStd) %>%
  select(CNT, SCHOOLID, STUID, BOOKID, all_of(items12))

# example values:
table(noscore12$PM00FQ01, useNA="a")
val_labels(noscore12$PM033Q01)

for (i in items12) {
  noscore12[,i] <- to_factor(noscore12[,i])
}

noscore12 <- pivot_longer(noscore12, all_of(items12))
data.frame(table(noscore12$value, useNA="a"))

noscore12 <- noscore12 %>%
  mutate(
    resp_type = "answer",
    resp_type = ifelse(value %in% c("Invalid","Missing"), "skip", resp_type),
    resp_type = ifelse(value %in% c("Not reached"), "nr", resp_type),
    resp_type = ifelse(value %in% c("N/A"), "NA", resp_type)
  )

noscore12 <- noscore12 %>% zap_labels() %>% rename(item_id=name, raw_resp=value)

### merge #####

nrow(long12)
long12 <- left_join(long12, noscore12)
gc()
nrow(long12)
with(long12, table(resp_type, resp, useNA="a"))

### Save file #####

nrow(long12)
long12 <- filter(long12, !is.na(resp))
nrow(long12)
gc()

saveRDS(long12, "2b-temp-2012-long.Rds")
rm(c12, long12, noscore12)
gc()




## 2015 #####

### load data #####

# Missing and not reached are coded differently in this year. 

c15 <- read_spss(file = "pisa2015/data/CY6_MS_CMB_STU_COG.sav", user_na=TRUE)

c15 <- c15 %>%
  rename(
    SCHOOLID=CNTSCHID,
    STUID=CNTSTUID
  ) %>%
  mutate(
    YEAR=2015,
    SCHOOLID=as.character(SCHOOLID),
    STUID=as.character(STUID) 
  ) %>%
  select(YEAR, CNT, SCHOOLID, STUID, BOOKID, all_of(items15))

c15 <- c15 %>% mutate(across(.cols = c("CNT","BOOKID"), user_na_zap))
c15

# Keeping the labels for item responses. Some examples of the information:

table(c15$CM955Q03S, useNA="a") # example of polytomous item
table(c15$DM155Q02C, useNA="a") # example of human coded item ending with "C"
table(as_factor(c15$DM155Q02C), useNA="a")
table(as_factor(c15$CM033Q01S), useNA="a")
table(c15$CM033Q01S,user_na_to_na(c15$CM033Q01S), useNA="a")

# The `scored` and `scored_numeric` functions first convert the scored responses
# to a factor so that we can work with the labels, and then convert the factors 
# to numeric values with the following definitions:

# - 0: incorrect or no response/invalid response
# - 1: full credit for binary item; partial credit for polytomous item
# - 2: full credit for polytomous item
# - 8: not reached
# - NA: not administered or valid skip

# examples
table(c15$CM033Q01S, scored(c15$CM033Q01S), useNA="a")
table(c15$CM033Q01S, scored_numeric(scored(c15$CM033Q01S)), useNA="a")
table(c15$CM955Q03S, scored(c15$CM955Q03S), useNA="a")
table(c15$DM155Q02C, scored(c15$DM155Q02C), useNA="a")
table(c15$DM155Q02C, scored_numeric(scored(c15$DM155Q02C)), useNA="a")

### scoring #####

#### Step 1 #####

c15 <- c15 %>% mutate(across(all_of(items15), scored))
select(c15, CM033Q01S, CM955Q03S, DM155Q02C) %>% apply(.,2,table)
checks <- select(c15, all_of(items15)) %>% apply(., 2, table, useNA="a")
checks

#### Step 2 #####

c15 <- c15 %>% mutate(across(all_of(items15), scored_numeric))
select(c15, CM033Q01S, CM955Q03S, DM155Q02C, DR455Q03C) %>% apply(.,2,table, useNA="a")

### append student covariates #####

nrow(c15)
c15 <- left_join(c15, stuindex, by = c("YEAR","CNT","SCHOOLID","STUID","BOOKID")) %>%
  filter(inIndex==1) %>%
  select(-inIndex)

c15 %>%
  group_by(CNT, SCHOOLID, STUID) %>%
  summarise(N=n()) %>%
  filter(N>1)
nrow(c15)

### convert to long #####

long15 <- c15 %>%
  pivot_longer(cols = -all_of(idvars), names_to = "item_id", values_to = "resp") %>%
  mutate(SUB = str_sub(item_id,2,2))
glimpse(long15)
gc()

### add raw responses to record skips #####

noscore15 <- read_spss(file = "pisa2015/data/CY6_MS_CMB_STU_COG.sav",
                       user_na=TRUE)

noscore15 <- noscore15 %>%
  rename(
    SCHOOLID=CNTSCHID,
    STUID=CNTSTUID
  ) %>%
  mutate(SCHOOLID=as.character(SCHOOLID),
         STUID=as.character(STUID)) %>%
  select(CNT, SCHOOLID, STUID, BOOKID, all_of(items15))

for (i in items15) {
  noscore15[,i] <- to_factor(noscore15[,i])
}

noscore15 <- pivot_longer(noscore15, all_of(items15))

# These are all the values we have to deal with:

data.frame(table(noscore15$value, useNA="a"))

# Convert to new codes:

noscore15 <- noscore15 %>%
  mutate(
    resp_type = "answer",
    resp_type = ifelse(value %in% c("Invalid","No Response"), "skip", resp_type),
    resp_type = ifelse(value %in% c("Not Reached"), "nr", resp_type),
    resp_type = ifelse(is.na(value) | value=="Not Applicable", "NA", resp_type)
  )

with(noscore15, table(value, resp_type, useNA="a"))

noscore15 <- noscore15 %>% zap_labels() %>% rename(item_id=name, raw_resp=value)
glimpse(noscore15)

### merge #####

nrow(long15)
long15 <- left_join(long15, noscore15)
nrow(long15)
with(long15, table(resp_type, resp, useNA="a"))
gc()

### Save file #####

nrow(long15)
long15 <- filter(long15, !is.na(resp))
nrow(long15)
gc()

rm(c15, noscore15)
gc()
saveRDS(long15, "2b-temp-2015-long.Rds")
rm(long15)
gc()






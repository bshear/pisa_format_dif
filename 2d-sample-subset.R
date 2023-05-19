
# subset to final sample for later analyses

# depends on:
# 2c-cog-scored-long-allyears-standard.Rds

# produces:
# 2d-cog-scored-long-allyears-standard-subset.Rds

# Limit to students with at least 5 responses
# Country-language-year-subject with at least 2,000 students
# Countries with all three years and both subjects
# More widely used language in some countries
# Drop items with p<0.01
# Drop math item with missing process data 2009
# Add item covariates

library(tidyverse)
library(kableExtra)

rm(list=ls())

gc()

outfilename <- "2d-cog-scored-long-allyears-standard-subset.Rds"

long <- readRDS(file = "2c-cog-scored-long-allyears-standard.Rds")

# sample restrictions #####

## remove 99(UH) forms

sum(long$BOOKID==99)
long <- filter(long, BOOKID!=99)

## student-by-subject with at least 5 responses #####

# a "response" could be an administered but skipped/not reached item

# total obs
nrow(long)

# > nrow(long)
# [1] 38780628

# unique countries
length(unique(long$CNT))

# > length(unique(long$CNT))
# [1] 80

# unique students
long %>%
  group_by(YEAR, CNT, SCHOOLID, STUID) %>%
  summarise() %>%
  nrow(.)
# [1] 1073331

# resp per student
long <- long %>%
  group_by(YEAR, CNT, SCHOOLID, STUID, SUB) %>%
  mutate(stu_nresp = n()) %>%
  ungroup()

table(long$stu_nresp)

gc()

# filter to n>=5
long <- long[long$stu_nresp>=5, ]

# unique countries
length(unique(long$CNT))

# unique students
long %>%
  group_by(YEAR, CNT, SCHOOLID, STUID) %>%
  summarise() %>%
  nrow(.)
# [1] 1073064

# check books
with(long, table(BOOKID, YEAR, SUB, useNA="a"))

## country-language-subject-years with at least 2,000 students #####

length(unique(long$CNT))

long <- long %>%
  group_by(YEAR, CNT, SUB, TESTLANG) %>%
  mutate(nstu = length(unique(STUID))) %>%
  ungroup() %>%
  filter(nstu>=2000)

length(unique(long$CNT))

# next, keep only country-years where both subjects still in data

length(unique(long$CNT))

long <- long %>%
  group_by(CNT, YEAR, TESTLANG) %>%
  mutate(nsub=length(unique(SUB))) %>%
  ungroup() %>%
  filter(nsub==2)

length(unique(long$CNT))

gc()

# next, keep only countries where all three years still in data

length(unique(long$CNT))

long <- long %>%
  group_by(CNT) %>%
  mutate(nyear=length(unique(YEAR))) %>%
  ungroup() %>%
  filter(nyear==3)

length(unique(long$CNT))

gc()

# check # of unique students
long %>%
  group_by(YEAR, CNT, SCHOOLID, STUID) %>%
  summarise() %>%
  nrow(.)

#[1] 694315

long %>%
  group_by(YEAR, CNT, SUB, STUID) %>%
  summarise(n=n()) %>%
  summary()

gc()

# coding of languages changed from 2009 to 2012/2015
# challenge because even in US there are two languages over time
# these are "ENG" and "English"

long %>%
  group_by(CNT, YEAR) %>%
  summarise(nlang=length(unique(TESTLANG))) %>%
  filter(nlang>1)

long %>%
  group_by(CNT) %>%
  mutate(nlang = length(unique(TESTLANG))) %>%
  ungroup() %>%
  filter(nlang>2) %>%
  group_by(CNT, TESTLANG, YEAR) %>%
  summarise(n=length(unique(STUID))) %>%
  print(n=Inf)

long %>%
  filter(CNT=="CAN") %>%
  group_by(CNT, SUB, YEAR, TESTLANG) %>%
  summarise(nstu=length(unique(STUID)))

long %>%
  filter(CNT=="ESP") %>%
  group_by(CNT, SUB, YEAR, TESTLANG) %>%
  summarise(nstu=length(unique(STUID))) %>%
  print(n=Inf)

long %>%
  filter(CNT=="USA") %>%
  group_by(CNT, SUB, YEAR, TESTLANG) %>%
  summarise(nstu=length(unique(STUID)))

long %>%
  group_by(SUB, YEAR) %>%
  summarise(NI=length(unique(item_id)))

gc()

## Remove some second language country-year observations

long %>%
  filter(
    !(CNT=="BEL" & TESTLANG %in% c("FRE","French")),
    !(CNT=="CAN" & TESTLANG %in% c("FRE","French")),
    !(CNT=="ESP" & TESTLANG %in% c("CAT","Catalan"))
  ) %>%
  group_by(CNT,SUB,YEAR) %>%
  summarise() %>%
  group_by(CNT) %>%
  summarise(n=n()) %>%
  summary()

long <- long %>%
  filter(
    !(CNT=="BEL" & TESTLANG %in% c("FRE","French")),
    !(CNT=="CAN" & TESTLANG %in% c("FRE","French")),
    !(CNT=="ESP" & TESTLANG %in% c("CAT","Catalan"))
  )

long %>%
  group_by(YEAR, CNT, SUB, TESTLANG, item_id) %>%
  summarise(item_nresp = n()) %>%
  summary()

long %>%
  group_by(YEAR, CNT, SUB, TESTLANG) %>%
  summarise(N=n_distinct(STUID)) %>%
  summary()

long %>%
  group_by(YEAR, CNT, SUB, STUID) %>%
  summarise() %>%
  with(., table(CNT, YEAR, SUB))

long %>%
  group_by(CNT, SUB, YEAR, TESTLANG) %>%
  summarise() %>%
  pivot_wider(names_from = c("SUB","YEAR"), values_from="TESTLANG")

# unique countries
length(unique(long$CNT))

# unique students
long %>%
  group_by(YEAR, CNT, SCHOOLID, STUID) %>%
  summarise() %>%
  nrow(.)
# [1] 680165

gc()

## drop items with p<0.01 based on ypart scoring #####

long <- long %>%
  group_by(CNT, YEAR, SUB, item_id) %>%
  mutate(Ppart=mean(ypart)) %>%
  ungroup()

long %>%
  group_by(CNT, YEAR, SUB, item_id) %>%
  summarise() %>%
  nrow()
# [1] 14695

long %>%
  filter(Ppart>0.01) %>%
  group_by(CNT, YEAR, SUB, item_id) %>%
  summarise() %>%
  nrow()
#[1] 14686

14695-14686

# drops 9 item-subject-year-country observations in math

long %>%
  filter(Ppart<=0.01) %>%
  with(., table(item_id, YEAR, SUB))

# , , SUB = M
# 
# YEAR
# item_id     2012 2015
# CM00GQ01S    0  684
# CM943Q02S    0 2899
# DM406Q02C    0  725
# PM943Q02  1452    0
# PM995Q02  2973    0

long <- filter(long, Ppart>0.01)

long %>%
  group_by(CNT, SUB, YEAR) %>%
  summarise(item=length(unique(item_id))) %>%
  pivot_wider(names_from = c("SUB","YEAR"), values_from="item") %>%
  kable() %>% kable_styling()

nrow(long)
#[1] 24551599

gc()

# drop not needed variables #####

glimpse(long)
long <- select(long, -stu_nresp, -nstu, -nsub, -nyear, -Ppart)
gc()

# add item covariates #####

items <- read.csv(file = "1-item_formats_09_12_15.csv")
items <- select(items, year, subject, item_id, item_format,
                item_id_short, int_pval, is_mc, is_poly,
                process_short, subscale, text_format)

items <- rename(items,
                SUB=subject,
                YEAR=year)

long <- left_join(long, items, by = c("YEAR","SUB","item_id"))

glimpse(long)

# Rows: 24,551,599
# Columns: 31
# $ YEAR          <dbl> 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2…
# $ CNT           <chr> "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS",…
# $ SCHOOLID      <chr> "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "…
# $ STUID         <chr> "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "00001", "…
# $ BOOKID        <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,…
# $ OECD          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
# $ GRADE         <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 1…
# $ AGE           <dbl> 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42, 15.42,…
# $ TESTLANG      <chr> "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "ENG",…
# $ GENDER        <chr> "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE", "MALE…
# $ ADMINMODE     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ Option_Read   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ Option_Math   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ Admin_Std     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ item_id       <chr> "M192Q01T", "M406Q01", "M406Q02", "M423Q01", "M496Q01T", "M496Q02", "M564Q01", "M564Q02", "M571Q01", "M603Q01T", "M603Q02T", "R404Q03", "R404Q06", "R404Q07T", "R404Q10A", "R404Q10B"…
# $ resp          <dbl> 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 2, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1…
# $ SUB           <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "R", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "…
# $ raw_resp      <fct> Full credit, Missing, Missing, Full credit, Full credit, Missing, No credit, No credit, No credit, No credit, Invalid, Full credit, No credit, No credit, No credit, No credit, Full …
# $ resp_type     <chr> "answer", "skip", "skip", "answer", "answer", "skip", "answer", "answer", "answer", "answer", "skip", "answer", "answer", "answer", "answer", "answer", "answer", "answer", "answer",…
# $ y             <dbl> 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 2, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1…
# $ maxy          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
# $ yfull         <dbl> 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1…
# $ ypart         <dbl> 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1…
# $ item_format   <chr> "CMC", "OCR", "OCR", "MC", "CMC", "SR", "MC", "MC", "MC", "CMC", "SR", "MC", "MC", "CMC", "OCR", "OCR", "OCR", "OCR", "OCR", "CMC", "MC", "MC", "OCR", "SR", "MC", "CMC", "MC", "CCR"…
# $ item_id_short <chr> "M192Q01", "M406Q01", "M406Q02", "M423Q01", "M496Q01", "M496Q02", "M564Q01", "M564Q02", "M571Q01", "M603Q01", "M603Q02", "R404Q03", "R404Q06", "R404Q07", "R404Q10A", "R404Q10B", "R4…
# $ int_pval      <dbl> 41.10, 26.70, 16.70, 79.10, 51.50, 65.70, 46.40, 45.80, 46.60, 43.50, 34.80, 73.03, 48.89, 33.95, 43.32, 37.72, 66.55, 32.47, 73.44, 42.41, 66.59, 75.95, 35.55, 78.30, 64.44, 25.92,…
# $ is_mc         <int> 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0…
# $ is_poly       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ process_short <chr> "Formulate", "Employ", "Formulate", "Interpret", "Formulate", "Employ", "Formulate", "Formulate", "Interpret", "Employ", NA, "Integrate and interpret", "Integrate and interpret", "I…
# $ subscale      <chr> "Change and Relationships", "Space and Shape", "Space and Shape", "Uncertainty and data", "Quantity", "Quantity", "Quantity", "Uncertainty and data", "Change and Relationships", "Qu…
# $ text_format   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Continuous", "Non-continuous", "Non-continuous", "Non-continuous", "Non-continuous", "Continuous", "Continuous", "Continuous", "Non-cont…

gc()

with(long, sum(is.na(item_id)))

## drop math item missing process data #####

with(long, table(is.na(process_short), item_id=="M603Q02T"))

#         FALSE     TRUE
# FALSE 38673613        0
# TRUE         0   127298

long <- filter(long, !(item_id=="M603Q02T" & SUB=="M" & YEAR==2009))
nrow(long)

gc()

# save file #####

nrow(long)
#[1] 24476032
saveRDS(long, outfilename)



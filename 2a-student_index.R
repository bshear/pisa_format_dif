
# create file of student IDs that will potentially be included in later analyses
# also include student age, grade, and gender

# depends on: 
# INT_STQ09_DEC11.sav
# INT_STU12_DEC03.sav
# CY6_MS_CMB_STU_QQQ.sav

# creates:
# 2a-student_index_standard.Rds

library(tidyverse)
library(haven)
library(labelled)
library(kableExtra)

rm(list=ls())
gc()

outfilename <- "2a-student_index_standard.Rds"

q09 <- read_spss(file = "pisa2009/data/INT_STQ09_DEC11.sav", user_na=TRUE)

q12 <- read_spss(file = "pisa2012/data/INT_STU12_DEC03.sav", user_na=TRUE)

q15 <- read_spss(file = "pisa2015/data/CY6_MS_CMB_STU_QQQ.sav", user_na=TRUE)

var_label(q15$ADMINMODE)
q15labels <- var_label(q15)
q15labels[21:30]
var_label(q15$ST004D01T)
val_labels(q15$ST004D01T)
val_labels(q15$ADMINMODE)
val_labels(q15$Option_Math)
val_labels(q15$Option_Read)
var_label(q15$LANGTEST_COG)
var_label(q15$LANGTEST_QQQ)
var_label(q15$LANGTEST_PAQ)

# filter to countries within years where
# - use regular books (BOOKID<=13) in 2009 and 2012
# - use Standard Cluster and computerized test in 2015

val_labels(q09$ST04Q01)
d09 <- q09 %>%
  mutate(
    GENDER = ifelse(ST04Q01==1, "FEMALE",
                    ifelse(ST04Q01==2,"MALE",NA)),
    OECD = ifelse(as_factor(q09$OECD)=="OECD",1,0),
    GRADE=ST01Q01,
    YEAR=2009
  ) %>%
  rename(
    STUID=StIDStd
  ) %>%
  select(
    YEAR, OECD, CNT, SCHOOLID, STUID,
    GRADE, AGE, BOOKID, TESTLANG, GENDER
  )
d09 <- zap_labels(d09)

table(as_factor(q12$TestLANG), useNA="a")
d12 <- q12 %>%
  mutate(
    GENDER = ifelse(ST04Q01==1, "FEMALE",
                    ifelse(ST04Q01==2,"MALE",NA)),
    GRADE=ST01Q01,
    YEAR=2012,
    TestLANG=as_factor(TestLANG)
  ) %>%
  rename(
    STUID=StIDStd,
    BOOKID=Bookid,
    TESTLANG=TestLANG
  ) %>%
  select(
    YEAR, OECD, CNT, SCHOOLID, STUID,
    GRADE, AGE, BOOKID, TESTLANG, GENDER
  )  

d12 <- zap_labels(d12)

d15 <- q15 %>%
  mutate(
    GENDER = ifelse(ST004D01T==1, "FEMALE",
                    ifelse(ST004D01T==2,"MALE",NA)),
    LANGTEST_COG=as.character(as_factor(LANGTEST_COG)),
    YEAR=2015,
    Option_Read=as.character(as_factor(Option_Read)),
    Option_Math=as.character(as_factor(Option_Math)),
    CNTSCHID=as.character(CNTSCHID),
    CNTSTUID=as.character(CNTSTUID),
    Admin_Std = ifelse(Option_Math=="Standard Cluster" & 
                         Option_Read=="Standard Cluster" & 
                         ADMINMODE==2, 1, 0),
    GRADE=ST001D01T
  ) %>%
  rename(
    SCHOOLID=CNTSCHID,
    STUID=CNTSTUID,
    TESTLANG=LANGTEST_COG
  ) %>%
  select(YEAR, OECD, CNT, SCHOOLID, STUID, BOOKID, 
         AGE, GRADE,
         ADMINMODE, Option_Read, Option_Math, Admin_Std,
         TESTLANG, GENDER)
d15 <- zap_labels(d15)

# bind rows

d <- bind_rows(d09, d12)

d <- bind_rows(d, d15)

d <- arrange(d, CNT, YEAR, SCHOOLID, STUID)

# limit to "standard" booklets #####

dsub <- d %>%
  filter((BOOKID<=13 & YEAR %in% c(2009,2012)) | (YEAR==2015 & Admin_Std==1))

# show all administered languages by year

d %>%
  group_by(CNT, YEAR, TESTLANG) %>%
  summarise() %>%
  group_by(CNT, YEAR) %>%
  summarise(LANGS=paste0(unique(TESTLANG), collapse=",")) %>%
  pivot_wider(names_from = c("YEAR"), values_from="LANGS") %>%
  kable() %>% kable_styling()

# save data #####

saveRDS(dsub, file = outfilename)




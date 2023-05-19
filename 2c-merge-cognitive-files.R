
# merge cognitive files across years together
# create y/ypart/yfull scores

# depends on (deleted after run):
# 2b-temp-2009-long.Rds
# 2b-temp-2012-long.Rds
# 2b-temp-2015-long.Rds

# produces:
# 2c-cog-scored-long-allyears-standard.Rds

library(tidyverse)

rm(list=ls())
gc()

outfilename <- "2c-cog-scored-long-allyears-standard.Rds"

# load files #####

long09 <- readRDS(file = "2b-temp-2009-long.Rds")
long12 <- readRDS(file = "2b-temp-2012-long.Rds")
long15 <- readRDS(file = "2b-temp-2015-long.Rds")

# stack files #####

long <- bind_rows(long09, long12, long15)

rm(long09, long12, long15)

gc()

# create binary response variables #####

# y: 0/1/2 poly scoring
# ypart: 0=no credit; 1=part/full credit
# yfull: 0=no/part credit; 1=full credit

long <- long %>%
  mutate(y=resp,
         y=ifelse(y==8,0,y))

table(long$y, useNA="a")

long <- long %>%
  group_by(SUB, YEAR, item_id) %>%
  mutate(maxy=max(y)) %>%
  ungroup() %>%
  mutate(
    yfull = ifelse(y==maxy,1,0),
    ypart = ifelse(y>0,1,0)
  )

with(long, table(y, yfull))
with(long, table(y, ypart))
with(long, table(resp, ypart, useNA="a"))

# save full long file #####

gc()
saveRDS(long, outfilename)


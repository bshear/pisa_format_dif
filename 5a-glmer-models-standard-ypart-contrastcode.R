# estimate LLTM DFF models
# depends on: 2d-cog-scored-long-allyears-standard-subset.Rds
# produces: 5a-glmer-models-standard-ypart-contrastcode.Rds

library(tidyverse)
library(lme4)
library(sjPlot)
library(broom.mixed)
library(insight)
library(vtable)

sessionInfo()

rm(list=ls())

d <- readRDS(file = "2d-cog-scored-long-allyears-standard-subset.Rds") %>%
  arrange(CNT, YEAR, SUB, STUID, item_id)

## Create indicator variables
## set yfull versus ypart scoring

with(d, table(text_format, SUB))

d <- d %>%
  mutate(
    isCR = ifelse(is_mc==1, 0, 1),
    FEMALE = ifelse(GENDER=="FEMALE",1,-1),
    yvar=ypart
  )

dd <- d

nrow(dd)

rm(d)

gc()

table(dd$CNT)

select(dd,
       FEMALE, SUB, YEAR, GRADE, AGE,
       ypart, yvar,
       isCR) %>%
  sumtable(., out="return")

# Variable        N     Mean Std. Dev.   Min Pctl. 25 Pctl. 75   Max
# 1    FEMALE 24476032   -0.003         1    -1       -1        1     1
# 2       SUB 24476032                                                 
# 3     ... M 10997560    44.9%                                        
# 4     ... R 13478472    55.1%                                        
# 5      YEAR 24476032 2011.323     2.228  2009     2009     2012  2015
# 6     GRADE 24450112    9.831     3.537     7        9       10    96
# 7       AGE 24476032   15.771     0.291 15.17     15.5       16 16.42
# 8     ypart 24476032    0.537     0.499     0        0        1     1
# 9      yvar 24476032    0.537     0.499     0        0        1     1
# 10     isCR 24476032    0.554     0.497     0        0        1     1

gc()

starttime <- Sys.time()

glmer_results <- list()

for (cnt in unique(dd$CNT)) {
  
  gc()
  
  for (sub in c("R","M")) {
    for (year in c(2009,2012,2015)) {
      
      print(paste0(cnt, sub, year))
      
      dsub <- filter(dd, CNT==cnt, SUB==sub, YEAR==year)
      
      m0 <- glmer(yvar ~ 1 + FEMALE + (1 | STUID) + (1 | item_id),
                  data = dsub,
                  family="binomial",
                  control = glmerControl(optimizer = "bobyqa"))
      
      m1 <- glmer(yvar ~ 1 + FEMALE + isCR + FEMALE:isCR + (1 | STUID) + (1 | item_id),
                  data = dsub,
                  family="binomial",
                  control = glmerControl(optimizer = "bobyqa"))
      
      if(!is_converged(m1)[1]) {
        ss <- getME(m1,c("theta","fixef"))
        m1 <- update(m1,start=ss,
                     control = glmerControl(optCtrl=list(maxfun=20000)))
      }
      
      m0_tibble <- tidy(m0) %>% mutate(model="M0", CNT=cnt, SUB=sub, YEAR=year,
                                       converge = is_converged(m0)[1])
      
      m1_tibble <- tidy(m1) %>% mutate(model="M1", CNT=cnt, SUB=sub, YEAR=year,
                                       converge = is_converged(m1)[1])
      
      get_resids <- function(model,modelname) {
        ranef(model)$item_id %>%
          rownames_to_column("item_id") %>%
          rename(resid=`(Intercept)`) %>%
          mutate(model=modelname,
                 CNT=cnt,
                 SUB=sub,
                 YEAR=year
          )
      }
      
      item_resids <- bind_rows(
        get_resids(m0,"M0"),
        get_resids(m1,"M1")
      )
      
      glmer_results[[paste0(cnt, sub, year)]] <- list(
        M0=m0_tibble,
        M1=m1_tibble,
        fit_stats=bind_rows(glance(m0), glance(m1)),
        itemresids=item_resids
      )
    }
  }
}

saveRDS(glmer_results, file = "5a-glmer-models-standard-ypart-contrastcode.Rds")

endtime <- Sys.time()
endtime-starttime

table(unlist(lapply(glmer_results, function(x) x$M1$converge[1])))




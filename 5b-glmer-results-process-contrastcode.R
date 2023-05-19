# process LLTM DFF models

# depends on: 5a-glmer-models-standard-ypart-contrastcode.Rds"

# produces: 
# 5b-dff-estimates-contrastcode.csv
# 5b-fig-s1-boxplot1.png

library(tidyverse)
library(kableExtra)
library(metafor)
library(psych)

rm(list=ls())

# load model estimates #####

glmer_results <- readRDS(file = "5a-glmer-models-standard-ypart-contrastcode.Rds")

# all model results #####

dif_tibble <- do.call("rbind", lapply(glmer_results,
                                      function(x){
                                        bind_rows(x$M0, x$M1, x$M2)
                                      })) %>%
  group_by(YEAR, SUB, CNT)

# subset of estimates of interest #####

ests <- do.call("rbind", lapply(glmer_results, function(x) bind_rows(x$M1))) %>%
  mutate(SUB=ifelse(SUB=="M","Math","Reading"),
         term=ifelse(is.na(group), term, paste0(term,group))) %>%
  filter(term %in% c("(Intercept)", "FEMALE","isCR","FEMALE:isCR",
                     "sd__(Intercept)STUID","sd__(Intercept)item_id")) %>%
  mutate(ylo=estimate-1.96*std.error,
         yhi=estimate+1.96*std.error,
         term=factor(term,
                     levels = c("(Intercept)", "FEMALE", "isCR", "FEMALE:isCR", "sd__(Intercept)STUID","sd__(Intercept)item_id"),
                     labels = c("Intercept", "Female", "isCR", "Female X isCR", "sd(student)", "sd(item)"),
                     ordered = TRUE)) %>%
  arrange(SUB, YEAR, term, estimate) %>%
  group_by(SUB, YEAR, term) %>%
  mutate(rank=row_number()) %>%
  ungroup()

dff_ests <- ests %>%
  arrange(CNT, SUB, YEAR, term) %>%
  group_by(SUB, YEAR, CNT) %>%
  mutate(
    B3=estimate[term=="Female X isCR"],
    B3_se=std.error[term=="Female X isCR"],
    B3_p=p.value[term=="Female X isCR"],
    sdtheta=estimate[term=="sd(student)"],
    sditem=estimate[term=="sd(item)"]) %>%
  mutate(
    d_format=(2*B3)/sdtheta,
    d_format_se=(2*B3_se)/sdtheta
  )

# save DFF estimates #####

write.csv(dff_ests, file = "5b-dff-estimates-contrastcode.csv", row.names=FALSE)

# model diagnostics #####

## check convergence #####

table(dif_tibble$CNT)
table(dif_tibble$converge)
with(dif_tibble, table(converge, model))

## EB residuals #####

do.call("rbind", lapply(glmer_results, function(x){x$itemresids})) %>%
  filter(model=="M1") %>%
  ggplot(aes(x=resid)) +
  geom_histogram(color="black",fill="grey",bins = 15) +
  facet_grid(CNT ~ SUB+YEAR) +
  theme_bw()

do.call("rbind", lapply(glmer_results, function(x){x$itemresids})) %>%
  filter(model=="M1") %>%
  ggplot(aes(sample=resid)) +
  stat_qq(size=0.5) + stat_qq_line() +
  facet_grid(SUB+YEAR ~ CNT) +
  theme_bw()

## check correlations among estimates #####

# gender difference by format main effect

ests %>%
  select(CNT, SUB, YEAR, term, estimate) %>%
  filter(term %in% c("Female","isCR")) %>%
  pivot_wider(names_from="term", values_from="estimate") %>%
  ggplot(aes(x=Female, y=isCR)) +
  geom_point() +
  facet_wrap(SUB~YEAR)

# gender difference by intercept

ests %>%
  select(CNT, SUB, YEAR, term, estimate) %>%
  filter(term %in% c("Female","Intercept")) %>%
  pivot_wider(names_from="term", values_from="estimate") %>%
  ggplot(aes(x=Female, y=Intercept)) +
  geom_point() +
  facet_wrap(SUB~YEAR)

# gender difference and item main effect by format interaction

ests %>%
  select(CNT, SUB, YEAR, term, estimate) %>%
  filter(term %in% c("Female","Female X isCR")) %>%
  pivot_wider(names_from="term", values_from="estimate") %>%
  ggplot(aes(x=Female, y=`Female X isCR`)) +
  geom_point() +
  facet_wrap(SUB~YEAR)

ests %>%
  select(CNT, SUB, YEAR, term, estimate) %>%
  filter(term %in% c("isCR","Female X isCR")) %>%
  pivot_wider(names_from="term", values_from="estimate") %>%
  ggplot(aes(x=isCR, y=`Female X isCR`)) +
  geom_point() +
  facet_wrap(SUB~YEAR)

# all parameters

ests %>%
  filter(SUB=="Math") %>%
  select(CNT, YEAR, term, estimate) %>%
  pivot_wider(names_from = "term", values_from="estimate") %>%
  pairs.panels()

ests %>%
  filter(SUB=="Reading") %>%
  select(CNT, YEAR, term, estimate) %>%
  pivot_wider(names_from = "term", values_from="estimate") %>%
  pairs.panels()


# summarize DFF estimates #####

# print all standardized format estimates

dff_ests %>%
  filter(term=="Female X isCR") %>%
  arrange(SUB,YEAR,CNT) %>%
  mutate(
    x = round(d_format,2)
  ) %>%
  select(YEAR, SUB, CNT, x) %>%
  pivot_wider(names_from = c("SUB","YEAR"), values_from="x") %>%
  kable() %>% kable_styling()

# print all standardized format estimates, with significance stars

dff_ests %>%
  filter(term=="Female X isCR") %>%
  arrange(SUB,YEAR,CNT) %>%
  mutate(
    x = paste0(sprintf("%.2f",round(d_format, 2)), ifelse(B3_p<0.01, "*",""))
  ) %>%
  select(YEAR, SUB, CNT, x) %>%
  pivot_wider(names_from = c("SUB","YEAR"), values_from="x") %>%
  kable() %>% kable_styling()

# summary statistics

dff_ests %>%
  filter(term=="Female X isCR") %>%
  group_by(SUB,YEAR) %>%
  arrange(SUB,YEAR) %>%
  summarise(
    N=n(),
    mean=mean(d_format),
    median=median(d_format),
    min=min(d_format),
    max=max(d_format),
    nsig=sum(B3_p<0.01)
  ) %>%
  pivot_longer(cols = c("mean","median","min","max","nsig")) %>%
  pivot_wider(names_from = c("SUB","YEAR"), values_from="value") %>%
  kable(digits=2) %>% kable_styling()

# Figure S1. boxplot of all model estimates #####

ests %>%
  ggplot(aes(x=factor(YEAR), y=estimate)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(width = 0.05, pch=1) +
  ylab("Estimate") +
  xlab("Year") +
  facet_grid(term~SUB, scales="free_y") +
  theme_bw(base_size=14) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave(filename="5b-fig-s1-boxplot1.png", height=10, width=6.5)





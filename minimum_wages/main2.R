rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/TEA") 
library("haven"); library("plm"); library("dplyr"); library("ggplot2")
library("expss"); library("ggthemes")
data <- read_dta('2.Card&Krueger(1994).dta')

data <- data %>% group_by(state) %>% 
  mutate(fte_before = empft + nmgrs + emppt*0.5,
         fte_after = empft2 + nmgrs2 + emppt2*0.5, 
         wage_seg = case_when(wage_st == 4.25 ~ "low", 
                              wage_st >= 5 ~ "hight",
                              between(wage_st,4.26, 4.99 ) ~ "midrange"), 
         diference = fte_after - fte_before) %>% 
  ungroup()

#-----------------------STORES IN NEW JERSEY-----------------------------------#

test <- data %>% 
  filter(state == 1) %>% 
  filter(is.na(wage_seg) == FALSE) %>% 
  group_by(wage_seg) %>% 
  summarise(mean_before = mean(fte_before, na.rm=TRUE),
            mean_after = mean(fte_after, na.rm=TRUE),
            var_before = var(fte_before, na.rm=TRUE),
            var_after = var(fte_after, na.rm=TRUE),
            var_diference = var(diference, na.rm = TRUE),
            n_before = sum(!is.na(fte_before)),
            n_after = sum(!is.na(fte_after))) %>%
  mutate(se_mean_before = sqrt(var_before/n_before), 
         se_mean_after = sqrt(var_after/n_after), 
         change_mean_fte = mean_after - mean_before) %>% 
  select(wage_seg, 
         mean_before,
         se_mean_before,
         mean_after,
         se_mean_after, 
         change_mean_fte) %>% 
  ungroup()


test2 <- data %>% 
  filter(is.na(wage_seg) == FALSE) %>% 
  filter(complete.cases(fte_before, fte_after)) %>% 
  filter(state == 1) %>%
  group_by(wage_seg) %>% 
  summarise(mean_before_balanced = mean(fte_before),
            mean_after_balanced = mean(fte_after)) %>% 
  mutate(change_mean_fte_balanced = mean_after_balanced - mean_before_balanced) %>% 
  select(wage_seg, change_mean_fte_balanced) %>% 
  ungroup()

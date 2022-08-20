rm(list = ls())
setwd("C:/Users/JGMUNOZD/Desktop/TEA") 
library("haven"); library("plm"); library("dplyr"); library("ggplot2")
library("expss"); library("ggthemes")
data <- read_dta('2.Card&Krueger(1994).dta')

#NJ es el grupo de tratamiento y PN es el de control. 
#El resultado principal es que no hay cambiios en el empleo por el aumento del 
#salario mínimo en casi un dolar/hora

#El DF no tiene el sample completo, solo los locales que contestaron en las 
#dos entrevistas. 
nrow(data)

#Tabla 1.1: distribución el porcentajes para cada cadena de comida rápida 
#1.BK - 2. KFC - 3. Roy Rogers - 4. Wendy´s 

NJ <- data %>% filter(state == 1)
PA <- data %>% filter(state == 0)

as.data.frame(table(NJ$chain)) %>%
  rename(Cadena = 1, Frecuencia =2) %>% 
  mutate(Porcentaje = 100*Frecuencia/sum(Frecuencia))

as.data.frame(table(PA$chain)) %>%
  rename(Cadena = 1, Frecuencia =2) %>% 
  mutate(Porcentaje = 100*Frecuencia/sum(Frecuencia))

data <- data %>% group_by(state) %>% 
  mutate(fte_before = empft + nmgrs + emppt*0.5,
         fte_after = empft2 + nmgrs2 + emppt2*0.5, 
         wage_seg = case_when(wage_st == 4.25 ~ "low", 
                              wage_st >= 5 ~ "hight",
                              TRUE ~ "midrange"))
#-----------------------STORES IN NEW JERSEY-----------------------------------#

data %>% 
  filter(state == 1) %>% 
  group_by(wage_seg) %>% 
  summarise(mean_before = mean(fte_before, na.rm=TRUE),
            mean_after = mean(fte_after, na.rm=TRUE),
            var_before = var(fte_before, na.rm=TRUE),
            var_after = var(fte_after, na.rm=TRUE),
            n_before = sum(!is.na(fte_before)),
            n_after = sum(!is.na(fte_after))) %>%
  mutate(se_mean_before = sqrt(var_before/n_before), 
         se_mean_after = sqrt(var_after/n_after)) %>% 
  select(wage_seg, 
         mean_before,
         mean_after,
         se_mean_before,
         se_mean_after)


#------------------------------------------------------------------------------#

first <- data %>% 
  group_by(state) %>% 
  summarise(mean_before = mean(fte_before, na.rm=TRUE),
            mean_after = mean(fte_after, na.rm=TRUE),
            var_before = var(fte_before, na.rm=TRUE),
            var_after = var(fte_after, na.rm=TRUE),
            n_before = sum(!is.na(fte_before)),
            n_after = sum(!is.na(fte_after))) %>%
  mutate(se_mean_before = sqrt(var_before/n_before), 
         se_mean_after = sqrt(var_after/n_after), 
         state = dplyr::recode(state, '0' = "PA", '1' = "NJ"), 
         change_mean_fte = mean_after - mean_before) %>%
  select(state,
         mean_before,
         mean_after,
         change_mean_fte,
         se_mean_before,
         se_mean_after)

second <- data %>%
  filter(complete.cases(fte_before, fte_after)) %>% #Esto balancea el panel
  group_by(state) %>% 
  summarise(mean_before_balanced = mean(fte_before),
            mean_after_balanced = mean(fte_after))  %>% 
  mutate(change_mean_fte_balanced = mean_after_balanced - mean_before_balanced,
         state = dplyr::recode(state, '0' = "PA", '1' = "NJ")) %>% 
  select(state, change_mean_fte_balanced)

full_table <- left_join(first, second, by = c("state" = "state")) %>% 
  select(state,
         mean_before,
         mean_after,
         se_mean_before,
         se_mean_after,
         change_mean_fte,
         change_mean_fte_balanced) 

transposed <- as.data.frame(t(as.matrix(full_table))) %>% 
  rename("PA" = "V1", "NJ" = "V2") %>% 
  mutate(variable = c("state", 
                      "mean_before",
                      "mean_after",
                      "se_mean_before",
                      "se_mean_after",
                      "change_mean_fte",
                      "change_mean_fte_balanced")) %>% 
  filter(variable != "state") %>% 
  select(variable, everything()) %>% 
  mutate(PA = as.numeric(as.character(PA)),
         NJ = as.numeric(as.character(NJ)),
         Diff_NJ_NA = NJ - PA)

transposed

#------------------------------------------------------------------------------#


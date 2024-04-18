###################################################################################################################################################

## Author:        Hannah Pitzer
## Date Created:  2/29/2024
## Date Edited:   4/16/2024

###################################################################################################################################################

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, 
               withr, fixest, modelsummary, did, xtable)
remotes::install_github ("asheshrambachan/HonestDiD", force = TRUE)
install.packages("devtools")
devtools::install_github("bcallaway11/BMisc", dependencies = TRUE)
library(HonestDiD)

# import data

dataset_list <- c("HCRIS_Data", "medicaid_expansion", "pos_data_combined")

for (i in dataset_list){
  assign( paste0(i, ".df"),
          read.table(paste0("data/output/", i ,".txt"),
                     header = TRUE, fill = TRUE, 
                     colClasses=c("character"),
                     check.names = FALSE,
                     sep = '\t', quote = "")
  )
}

state_abbr.df <- read.csv('Crosswalks/state_abbr_xw.csv')


# Some final cleaning

medicaid_expansion.df <- medicaid_expansion.df %>%
  mutate(year = format(as.Date(medicaid_expansion.df$date_adopted), format="%Y")) %>%
  left_join(state_abbr.df, by = 'State') %>%
  rename(expand_year = year, state = Abbreviation, expand_ever = expanded) %>%
  transform(expand_ever=as.logical(expand_ever), expand_year=as.numeric(expand_year))%>%
  select(state, expand_ever, expand_year)

pos_data_combined.df <- pos_data_combined.df %>%
  rename(provider_number = provider) %>%
  filter(category == "Hospital") %>%
  select(provider_number, state, own_type, year)%>%
  mutate(private = ifelse( own_type == "Non-profit Private" | own_type == "Profit" , 1, 0),
         non_profit_private = ifelse( own_type == "Non-profit Private" , 1, 0), 
         profit_status = 
           case_when(
             own_type == 'Non-profit Private' ~ 'Non Profit', 
             own_type %in% c('Physician Owned', 'Profit') ~ 'For Profit'
           ))

HCRIS_Data.df <- HCRIS_Data.df %>%
  filter(year>=2003 & year <=2019) %>%
  transform(uncomp_care = as.numeric(uncomp_care), 
            cost_to_charge = as.numeric(cost_to_charge), 
            tot_pat_rev = as.numeric(tot_pat_rev)) %>%
  select(provider_number, uncomp_care, tot_pat_rev, year, cost_to_charge)


# Merge

full.data <- HCRIS_Data.df %>%
  left_join(pos_data_combined.df,
            by = c("provider_number", "year")) %>%
  left_join(medicaid_expansion.df, 
            by = "state") %>%
  mutate(uncomp_care = uncomp_care*cost_to_charge) %>%
  filter(year>=2003, year <=2019, uncomp_care>0, tot_pat_rev>0)


trim.data <- full.data %>%
  group_by(year) %>%
  mutate(ptile_uncomp=ntile(uncomp_care,100)) %>%
  filter(ptile_uncomp>1 & ptile_uncomp<99) %>%
  transform(uncomp_care = uncomp_care/1000000, 
            tot_pat_rev = tot_pat_rev/1000000, 
            year = as.numeric(year)) %>%
  filter(state != 'PR', state != 'VI', !is.na(state))


# Analysis --------------------------------------------------------------------

## 1 Summary statistics

  # Total revenues

trim.data.rev <- trim.data %>%
  group_by(year) %>% 
  summarize(
    mean = mean(tot_pat_rev, na.rm = TRUE), 
    std_dev = sd(tot_pat_rev, na.rm = TRUE),
    min = min(tot_pat_rev, na.rm = TRUE),
    max = max(tot_pat_rev, na.rm = TRUE)
  )

 
  # Uncompensated care

trim.data.uncomp <- trim.data %>%
  group_by(year) %>% 
  summarize(
    mean = mean(uncomp_care, na.rm = TRUE), 
    std_dev = sd(uncomp_care, na.rm = TRUE),
    min = min(uncomp_care, na.rm = TRUE),
    max = max(uncomp_care, na.rm = TRUE)
  )


## 2 Mean uncompensated care over time

  # Total

tot_uncomp_graph <- ggplot(trim.data.uncomp, aes(x = year, y = mean))+
  theme(plot.title = element_text(size=12)) +
  geom_line(linewidth=1) +
  labs(title = "Average Hospital Uncompensated Care from 2013 to 2019",
       x = "Year",
       y = "Dollars (millions)")


  # By profit status 

org.type.uncomp <- trim.data %>%
  filter(private == 1) %>% 
  group_by(year, non_profit_private) %>%
  mutate(non_profit_private = factor(non_profit_private)) %>%
  summarize(mean = mean(uncomp_care))

org_type_uncomp_graph <- ggplot(org.type.uncomp, aes(x = year, y = mean, col=non_profit_private))+
  geom_line(linewidth=1) +
  theme(plot.title = element_text(size=12)) +
  labs(title = "Average Hospital Uncompensated Care from 2013 to 2019",
       subtitle = "By organization type, private hospitals only",
       x = "Year",
       y = "Million dollars")+
  scale_color_discrete(name="Orginization Type",
                       labels=c("Private Non-profit ","Private For Profit"))


## 3 TWFE

  # Full
  reg.data.full <- trim.data %>%
    mutate(treat = 
             case_when(
               year >= expand_year & !is.na(expand_year) ~ 1, 
               year < expand_year & !is.na(expand_year) ~ 0, 
               is.na(expand_year) ~ 0
             )
           )

  dd.1 <- feols(uncomp_care ~ treat | provider_number + year, data= reg.data.full)

  # 2014 treatment vs never treated
  reg.data.2014 <- trim.data %>%
    filter(expand_year == 2014 | is.na(expand_year)) %>%
    mutate(post = (year>=2014), 
           treat = expand_ever*post)
  
  dd.2 <- feols(uncomp_care ~ treat | provider_number + year, data= reg.data.2014)

  # 2015 treatment vs never treated
  reg.data.2015 <- trim.data %>%
    filter(expand_year == 2015 | is.na(expand_year)) %>%
    mutate(post = (year>=2015), 
           treat = expand_ever*post)
  
  dd.3 <- feols(uncomp_care ~ treat | provider_number + year, data= reg.data.2015)

  # 2016 treatment vs never treated
  reg.data.2016 <- trim.data %>%
    filter(expand_year == 2016 | is.na(expand_year)) %>%
    mutate(post = (year>=2016), 
           treat = expand_ever*post)
  
  dd.4 <- feols(uncomp_care ~ treat | provider_number + year, data= reg.data.2016)
  
  # Summarize
  sum.fmt <- function(x) formatC(x, digits = 2, big.mark = ",", format = "f")
  dd.summary <- msummary(list("Full Sample"=dd.1, "Expand 2014"=dd.2, 
                              "Expand 2015"=dd.3, "Expand 2016"=dd.4),
                         shape=term + statistic ~ model, 
                         gof_map=NA,
                         coef_omit='Intercept',
                         coef_rename=c("treat"="Expansion"),
                         fmt= sum.fmt,
                         vcov = ~provider_number,
                         output="markdown",
                         caption="TWFE Estimates for Different Treatment Groups",
                         label="ddmodels")
  

## 4 Event studies

  # Full
  event.data.full <- reg.data.full %>%
    mutate(event_time=
             case_when(
               !is.na(expand_year) ~ year-expand_year,
               is.na(expand_year) ~ -1
             )
    ) 
  
  es.1 <- feols(uncomp_care ~ i(as.factor(event_time), expand_ever, ref=-1) | provider_number + year, 
                      cluster=~provider_number, data=event.data.full)
  
  es1.plot <- iplot(es.1, xlab = "Time to Treatment", main="Event Study - Full Sample")
  
  # 2014 treatment vs never treated
  es.2 <- feols(uncomp_care ~ i(as.factor(year), expand_ever, ref=2013) | provider_number + year, 
                cluster=~provider_number, data=reg.data.2014)
  
  es2.plot <- iplot(es.2, xlab = "Year", main="Event Study - 2014 Expansion")
  
  
## 5 Sun and Abraham
sa.data <- event.data.full %>%
  mutate(expand_year = ifelse(expand_ever==FALSE, 10000, expand_year), 
         time_to_treat = ifelse(expand_ever==FALSE, -1, year-expand_year), 
         time_to_treat = ifelse(time_to_treat < -15, -15, time_to_treat))

sa.1 <- feols(uncomp_care ~ sunab(expand_year, time_to_treat) | provider_number + year, 
              cluster=~provider_number, data=sa.data)

sa.tab <- tidy(summary(sa.1, agg=FALSE)) %>%
  filter(str_detect(term, 'cohort::2014|cohort::2015|cohort::2016')) %>%
  mutate(term = str_replace(term, 'time_to_treat::', ''), 
         term = str_replace(term, ':cohort::', ':')) %>%
  separate(term, c('period', 'cohort'), ':') %>%
  mutate(period = as.numeric(period)) %>%
  select(period, cohort, estimate, p.value) %>%
  rename(p_value=p.value)


## 6 SA event study
sa.plot <- iplot(sa.1, xlab = "Time to Treatment", main = "Sun and Abraham Event Study")


## 7 CS event study
cs.data <- reg.data.full %>%
  mutate(expand_year = ifelse(is.na(expand_year), 0, expand_year)) %>%
  group_by(provider_number) %>%
  mutate(hospital_id = cur_group_id()) %>%
  ungroup()

cs <- att_gt(yname='uncomp_care', tname='year', idname='hospital_id', gname='expand_year', 
                 data=cs.data, panel=TRUE, allow_unbalanced_panel = TRUE, est_method = 'dr')
cs.event <- aggte(cs, type='dynamic')

cs.coef <- tidy(cs.event) %>%
  select(rel_year=event.time, estimate, ci_lower=conf.low, ci_upper=conf.high) %>%
  mutate(rel_year=as.numeric(rel_year))
cs.coef <- as_tibble(cs.coef)

cs.plot <- ggplot(cs.coef, aes(x=rel_year, y=estimate)) +
  geom_point(size=2) +
  geom_linerange(aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_vline(xintercept=-0.5, linetype='dashed') +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks= -15:5, minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x='Relative Time', y='Estimated Effect (millions)', color=NULL, title = NULL) +
  theme_bw()


## 8 Honest DiD
cs.hdd <- att_gt(yname="uncomp_care", tname="year", idname="hospital_id",
                gname="expand_year",
                data=cs.data, panel=TRUE, est_method="dr",
                allow_unbalanced_panel=TRUE,
                base_period="universal")
cs.hdd.event <- aggte(cs.hdd, type="dynamic", min_e=-10, max_e=5)

hdd.cs <- honest_did(cs.hdd.event, type="smoothness", Mvec=seq(from=0, to=2, by=0.5))
hdd.cs.graph <- createSensitivityPlot(hdd.cs$robust_ci,
                                     hdd.cs$orig_ci)

coef.cs.hdd <- hdd.cs$robust_ci %>% bind_rows(hdd.cs$orig_ci) %>%
  mutate(type=case_when(
    M==2 ~ "M = +2",
    M==1.5 ~ "M = +1.5",
    M==1 ~ "M = +1",
    M==0.5 ~ "M = +0.5",
    M==0 ~ "M = 0",
    is.na(M) ~ "Original"
  ))

cs.hdd.plot <- ggplot(coef.cs.hdd, aes(x=factor(type, level=c('Original', 'M = 0', 'M = +0.5', 'M = +1', 'M = +1.5', 'M = +2')))) + 
  geom_linerange(aes(ymin = lb, ymax = ub)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position="none") +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = "Violation in Parallel Trends", y = "Estimated Effect (at t=0)", color = NULL, title = NULL) +
  theme_bw()


# Save workspace
save.image(file='analysis/did.RData')

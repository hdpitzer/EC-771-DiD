# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  5/30/2019
## Date Edited:   9/8/2022
## Notes:         -- v1996 of the HCRIS forms run through 2011 due to lags 
##                   in processing and hospital fiscal years


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)


# Import data -------------------------------------------------------------

## create v1996 and v2010 data
source('data-code/HCRIS-ian/H1_HCRISv1996.R')
source('data-code/HCRIS-ian/H2_HCRISv2010.R')

##### convert var types in v2010 --- added by Hannah Pitzer 3/22/24
final.hcris.v2010['tot_uncomp_care_charges'] <- as.numeric(unlist(final.hcris.v2010['tot_uncomp_care_charges']))
final.hcris.v2010['tot_uncomp_care_partial_pmts'] <- as.numeric(unlist(final.hcris.v2010['tot_uncomp_care_partial_pmts']))
final.hcris.v2010['bad_debt'] <- as.numeric(unlist(final.hcris.v2010['bad_debt']))
final.hcris.v2010['tot_discounts'] <- as.numeric(unlist(final.hcris.v2010['tot_discounts']))
final.hcris.v2010['hrrp_payment'] <- as.numeric(unlist(final.hcris.v2010['hrrp_payment']))
final.hcris.v2010['hvbp_payment'] <- as.numeric(unlist(final.hcris.v2010['hvbp_payment']))
final.hcris.v2010['tot_charges'] <- as.numeric(unlist(final.hcris.v2010['tot_charges']))
final.hcris.v2010['tot_operating_exp'] <- as.numeric(unlist(final.hcris.v2010['tot_operating_exp']))
final.hcris.v2010['ip_charges'] <- as.numeric(unlist(final.hcris.v2010['ip_charges']))
final.hcris.v2010['icu_charges'] <- as.numeric(unlist(final.hcris.v2010['icu_charges']))
final.hcris.v2010['ancillary_charges'] <- as.numeric(unlist(final.hcris.v2010['ancillary_charges']))
final.hcris.v2010['tot_discharges'] <- as.numeric(unlist(final.hcris.v2010['tot_discharges']))
final.hcris.v2010['mcare_discharges'] <- as.numeric(unlist(final.hcris.v2010['mcare_discharges']))
final.hcris.v2010['mcaid_discharges'] <- as.numeric(unlist(final.hcris.v2010['mcaid_discharges']))
final.hcris.v2010['tot_mcare_payment'] <- as.numeric(unlist(final.hcris.v2010['tot_mcare_payment']))
final.hcris.v2010['secondary_mcare_payment'] <- as.numeric(unlist(final.hcris.v2010['secondary_mcare_payment']))
final.hcris.v2010['new_cap_ass'] <- as.numeric(unlist(final.hcris.v2010['new_cap_ass']))
final.hcris.v2010['cash'] <- as.numeric(unlist(final.hcris.v2010['cash']))
final.hcris.v2010['tot_pat_rev'] <- as.numeric(unlist(final.hcris.v2010['tot_pat_rev']))
final.hcris.v2010['allowance'] <- as.numeric(unlist(final.hcris.v2010['allowance']))
final.hcris.v2010['net_pat_rev'] <- as.numeric(unlist(final.hcris.v2010['net_pat_rev']))



## create missing variables for columns introduced in v2010 of hcris forms
final.hcris.v1996 = final.hcris.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA, tot_uncomp_care_charges=NA, tot_uncomp_care_partial_pmts=NA, bad_debt=NA)

## create missing variables for columns in v1996 that we have to compute in v2010
final.hcris.v2010 = final.hcris.v2010 %>%
  mutate(uncomp_care=tot_uncomp_care_charges - tot_uncomp_care_partial_pmts + bad_debt)

############ fixing dates (Hannah)

## combine v1996 and v2010 hcris forms, and sort by provider_number/year
final.hcris <- rbind(final.hcris.v1996,final.hcris.v2010) %>%
  filter(report != 'rpt_rec_num') %>%
  mutate(fy_end = parse_date_time(fy_end, c("mdy", "ymd")), fy_start = parse_date_time(fy_start, c('mdy', 'ymd')), 
         date_processed = parse_date_time(date_processed, c('mdy', 'ymd')), date_created = parse_date_time(date_created, c('mdy', 'ymd'))) %>%
  mutate(tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) %>%
  select(-year)

## count of hospitals/provider_number by year
final.hcris %>% group_by(fyear) %>% count()


# Clean data --------------------------------------------------------------

## create count of reports by hospital fiscal year
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

## create running total of reports
final.hcris =
  final.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(report_number=row_number())

## identify hospitals with only one report per fiscal year 
## this will be the first set of hospitals in the final dataset
unique.hcris1 =
  final.hcris %>%
  filter(total_reports==1) %>%
  select(-report, -total_reports, -report_number, -npi, -status) %>%
  mutate(source='unique reports')


## identify hospitals with multiple reports per fiscal year
duplicate.hcris = 
  final.hcris %>%
  filter(total_reports>1) %>%
  mutate(time_diff=fy_end-fy_start)

## calculate elapsed time between fy start and fy end for hospitals with multiple reports
duplicate.hcris = 
  duplicate.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(total_days=sum(time_diff))

## if the elapsed time within a fy sums to around 365, then just take the total of the two
## this will be the second set of hospitals in the final dataset
unique.hcris2 = 
  duplicate.hcris %>%
  filter(total_days<370) %>%
  group_by(provider_number, fyear) %>%
  mutate(hrrp_payment=if_else(is.na(hrrp_payment),0,hrrp_payment),
         hvbp_payment=if_else(is.na(hvbp_payment),0,hvbp_payment)) %>%
  summarize(beds=max(beds), tot_charges=sum(tot_charges), tot_discounts=sum(tot_discounts),
            tot_operating_exp=sum(tot_operating_exp), ip_charges=sum(ip_charges),
            icu_charges=sum(icu_charges), ancillary_charges=sum(ancillary_charges),
            tot_discharges=sum(tot_discharges), mcare_discharges=sum(mcare_discharges),
            mcaid_discharges=sum(mcaid_discharges), tot_mcare_payment=sum(tot_mcare_payment),
            secondary_mcare_payment=sum(secondary_mcare_payment), hvbp_payment=sum(hvbp_payment),
            hrrp_payment=sum(hrrp_payment), fy_start=min(fy_start), fy_end=max(fy_end),
            date_processed=max(date_processed), date_created=min(date_created), 
            street=first(street), city=first(city), state=first(state),
            zip=first(zip), county=first(county),
            uncomp_care = sum(uncomp_care), tot_uncomp_care_charges=sum(tot_uncomp_care_charges),
            tot_uncomp_care_partial_pmts=sum(tot_uncomp_care_partial_pmts), 
            bad_debt=sum(bad_debt), cost_to_charge=first(cost_to_charge),
            new_cap_ass=sum(new_cap_ass), 
            cash=sum(cash),
            tot_pat_rev=sum(tot_pat_rev), 
            allowance=sum(allowance),
            net_pat_rev=sum(net_pat_rev)) %>%
  mutate(source='total for year')

## identify hospitals with more than one report and with elapsed time exceeding 370 days
duplicate.hcris2 =
  duplicate.hcris %>%
  filter(total_days>=370) %>%
  mutate(max_days=max(time_diff), max_date=max(fy_end))

## identify hospitals with one report (out of multiple) that appears to cover the full year
## this will be the third set of hospitals in the final dataset
unique.hcris3 = 
  duplicate.hcris2 %>%
  filter(max_days==time_diff, time_diff>360, max_date==fy_end) %>%
  select(-report, -total_reports, -report_number, -npi, -status, -max_days, -time_diff, -total_days, -max_date) %>%
  mutate(source='primary report')

## identify remaining hospitals (those with more than one report that cover more than one full year and that do
##   not appear to have one report that takes up the full year)
## these hospitals appear to have changed their fiscal years
duplicate.hcris3=anti_join(duplicate.hcris2, unique.hcris3, by=c("provider_number", "fyear"))
duplicate.hcris3 =
  duplicate.hcris3 %>%
  mutate(total_days=as.integer(total_days), time_diff=as.integer(time_diff)) %>%
  mutate_at(c("tot_charges","tot_discounts", "tot_operating_exp", "ip_charges",
              "icu_charges", "ancillary_charges", "tot_discharges", "mcare_discharges",
              "mcaid_discharges", "tot_mcare_payment", "secondary_mcare_payment",
              "hvbp_payment", "hrrp_payment", "uncomp_care", "tot_uncomp_care_charges", "bad_debt",
              "tot_uncomp_care_partial_pmts", "new_cap_ass", "cash", "tot_pat_rev", "allowance", "net_pat_rev"),
            list(~ .*(time_diff/total_days)))

## form weighted average of values for each fiscal year
unique.hcris4 = 
  duplicate.hcris3 %>%
  group_by(provider_number, fyear) %>%
  mutate(hrrp_payment=if_else(is.na(hrrp_payment),0,hrrp_payment),
         hvbp_payment=if_else(is.na(hvbp_payment),0,hvbp_payment)) %>%
  summarize(beds=max(beds), tot_charges=sum(tot_charges), tot_discounts=sum(tot_discounts),
            tot_operating_exp=sum(tot_operating_exp), ip_charges=sum(ip_charges),
            icu_charges=sum(icu_charges), ancillary_charges=sum(ancillary_charges),
            tot_discharges=sum(tot_discharges), mcare_discharges=sum(mcare_discharges),
            mcaid_discharges=sum(mcaid_discharges), tot_mcare_payment=sum(tot_mcare_payment),
            secondary_mcare_payment=sum(secondary_mcare_payment), hvbp_payment=sum(hvbp_payment),
            hrrp_payment=sum(hrrp_payment), fy_start=min(fy_start), fy_end=max(fy_end),
            date_processed=max(date_processed), date_created=min(date_created), 
            street=first(street), city=first(city), state=first(state),
            zip=first(zip), county=first(county),
            uncomp_care = sum(uncomp_care), tot_uncomp_care_charges=sum(tot_uncomp_care_charges),
            tot_uncomp_care_partial_pmts=sum(tot_uncomp_care_partial_pmts), 
            bad_debt=sum(bad_debt), cost_to_charge=first(cost_to_charge),
            new_cap_ass=sum(new_cap_ass),
            cash=sum(cash),
            tot_pat_rev=sum(tot_pat_rev), 
            allowance=sum(allowance),
            net_pat_rev=sum(net_pat_rev)) %>%
  mutate(source='weighted_average')

  


# Save final data ---------------------------------------------------------

final.hcris.data=rbind(unique.hcris1, unique.hcris2, unique.hcris3, unique.hcris4)
final.hcris.data =
  final.hcris.data %>%
  rename(year=fyear) %>%
  arrange(provider_number, year)

write_tsv(final.hcris.data,'data/output/HCRIS_Data.txt')
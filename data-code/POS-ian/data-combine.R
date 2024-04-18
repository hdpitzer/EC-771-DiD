# Meta --------------------------------------------------------------------

## Date Created:  7/22/2020
## Date Edited:   8/25/2022
## Description:   Code file to import and tidy CMS POS data from 1984-2021


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)


# Import raw data ---------------------------------------------------------
for (y in 1984:1990) {
  pos.data <- read_csv(paste0("data/input/POS/pos",y,".csv/pos",y,".csv"))

  pos.data <- pos.data %>%
    rename(provider=prov1680,
           own_change=prov0100,
           beds_cert=prov0755,
           beds_tot=prov0740,
           name=prov0475,
           street=prov2720,
           city_state=prov0115,
           zip=prov2905,
           fac_type=prov2890,
           own_type=prov2885,
           term_date=prov2810) %>%
    mutate(term_date=as.Date(as.character(term_date), format='%y%m%d'),
           own_change=as.Date(as.character(term_date), format='%y%m%d'),
           state=str_sub(city_state, start=-2),
           city=str_sub(city_state, start=1, end=-2),
           year=y) %>%
    select(provider, category, own_change, beds_cert, beds_tot, name,
           street, city, state, zip, term_date, fac_type, own_type, year) %>%
    filter(provider!='000000')

  assign(paste0("pos.",y),pos.data)
}


y=1991
pos.data <- read_csv(paste0("data/input/POS/pos",y,".csv/pos",y,".csv"))
pos.data <- pos.data %>%
  rename(category=prvdr_ctgry_cd,
         category_sub=prvdr_ctgry_sbtyp_cd,
         provider=prvdr_num,
         own_change=chow_dt,
         own_change_count=chow_cnt,
         beds_cert=crtfd_bed_cnt,
         beds_tot=bed_cnt,
         name=fac_name,
         street=st_adr,
         city_state=prov0115,
         zip=zip_cd,
         fac_type=gnrl_fac_type_cd,
         own_type=gnrl_cntl_type_cd,
         term_date=prov2810) %>%
  mutate(term_date=as.Date(as.character(term_date), format='%y%m%d'),
         own_change=as.Date(as.character(own_change), format='%y%m%d'),
         own_change_count=as.numeric(own_change_count),
         state=str_sub(city_state, start=-2),
         city=str_sub(city_state, start=1, end=-2),
         year=y) %>%
  select(provider, category, category_sub, own_change, own_change_count, beds_cert, beds_tot, name,
         street, city, state, zip, term_date, fac_type, own_type, year) %>%
  filter(provider!='000000')

assign(paste0("pos.",y),pos.data)


for (y in 1992:1993) {
  pos.data <- read_csv(paste0("data/input/POS/pos",y,".csv/pos",y,".csv"))

  pos.data <- pos.data %>%
    rename(category=prvdr_ctgry_cd,
           category_sub=prvdr_ctgry_sbtyp_cd,
           provider=prvdr_num,
           own_change=chow_dt,
           own_change_count=chow_cnt,
           beds_cert=crtfd_bed_cnt,
           beds_tot=bed_cnt,
           name=fac_name,
           street=st_adr,
           zip=zip_cd,
           own_type=gnrl_cntl_type_cd,
           term_date=prov2810,
           state=state_cd) %>%
    mutate(term_date=as.Date(as.character(term_date), format='%y%m%d'),
           own_change=as.Date(as.character(own_change), format='%y%m%d'),
           own_change_count=as.numeric(own_change_count),
           year=y) %>%
    select(provider, category, category_sub, own_change, own_change_count, beds_cert, beds_tot, name,
           street, city, state, zip, term_date, own_type, year)

  assign(paste0("pos.",y),pos.data)
}

y=1994
pos.data <- read_csv(paste0("data/input/POS/pos",y,".csv/pos",y,".csv"))
pos.data <- pos.data %>%
  rename(category=prvdr_ctgry_cd,
         category_sub=prvdr_ctgry_sbtyp_cd,
         provider=prvdr_num,
         own_change=chow_dt,
         own_change_count=chow_cnt,
         beds_cert=crtfd_bed_cnt,
         beds_tot=bed_cnt,
         name=fac_name,
         street=st_adr,
         zip=zip_cd,
         own_type=gnrl_cntl_type_cd,
         term_date=trmntn_exprtn_dt,
         state=state_cd) %>%
  mutate(term_date=as.Date(as.character(term_date), format='%y%m%d'),
         own_change=as.Date(as.character(own_change), format='%y%m%d'),
         own_change_count=as.numeric(own_change_count),
         year=y) %>%
  select(provider, category, category_sub, own_change, own_change_count, beds_cert, beds_tot, name,
         street, city, state, zip, term_date, own_type, year)
assign(paste0("pos.",y),pos.data)


for (y in 1995:2010) {
  pos.data <- read_csv(paste0("data/input/POS/pos",y,".csv/pos",y,".csv"))
  pos.data <- pos.data %>%
    rename(category=prvdr_ctgry_cd,
           category_sub=prvdr_ctgry_sbtyp_cd,           
           provider=prvdr_num,
           own_change=chow_dt,
           own_change_count=chow_cnt,           
           beds_cert=crtfd_bed_cnt,
           beds_tot=bed_cnt,
           name=fac_name,
           street=st_adr,
           zip=zip_cd,
           own_type=gnrl_cntl_type_cd,
           term_date=trmntn_exprtn_dt,
           state=state_cd) %>%
    mutate(term_date=as.Date(as.character(term_date), format='%Y%m%d'),
           own_change=as.Date(as.character(own_change), format='%Y%m%d'),
           own_change_count=as.numeric(own_change_count),
           year=y) %>%
    select(provider, category, category_sub, own_change, own_change_count, beds_cert, beds_tot, name,
           street, city, state, zip, term_date, own_type, year)
  
  assign(paste0("pos.",y),pos.data)  
}


for (y in 2011:2021) {
  pos.data <- read_csv(paste0("data/input/POS/pos",y,".csv/pos",y,".csv"))
  colnames(pos.data) <- tolower(colnames(pos.data))
  
  pos.data <- pos.data %>%
    rename(category=prvdr_ctgry_cd,
           category_sub=prvdr_ctgry_sbtyp_cd,           
           provider=prvdr_num,
           own_change=chow_dt,
           own_change_count=chow_cnt,
           beds_cert=crtfd_bed_cnt,
           beds_tot=bed_cnt,
           name=fac_name,
           street=st_adr,
           zip=zip_cd,
           city=city_name,
           own_type=gnrl_cntl_type_cd,
           term_date=trmntn_exprtn_dt,
           state=state_cd) %>%
    mutate(term_date=as.Date(as.character(term_date), format='%Y%m%d'),
           own_change=as.Date(as.character(own_change), format='%Y%m%d'),
           year=y,
           own_change_count=as.numeric(own_change_count)) %>%
    select(provider, category, category_sub, own_change, own_change_count, beds_cert, beds_tot, name,
           street, city, state, zip, term_date, own_type, year)
  
  assign(paste0("pos.",y),pos.data)  
}


# Final data --------------------------------------------------------------

## append yearly data
final.pos.data <- pos.1984
for (y in 1984:2021) {
  final.pos.data <- bind_rows(final.pos.data, get(paste0("pos.",y)))
}


## recode relevant variables...note, sub categories and ownership values change
## depending on the category type:
final.pos.data <- final.pos.data %>%
  mutate(category=str_pad(category, width=2, side="left", pad="0")) %>%
  mutate(category_sub=str_pad(category_sub, width=2, side="left", pad="0")) %>%
  mutate(own_type=str_pad(own_type, width=2, side="left", pad="0")) %>%
  mutate(category = 
           case_when(
             category=="01" ~ "Hospital",
             category %in% c("02","03","04") ~ "SNF",
             category=="05" ~ "Home Health",
             category=="06" ~ "Psychiatric",
             category=="07" ~ "Portable X-Ray",
             category=="08" ~ "Physical Therapy",
             category=="09" ~ "ESRD",
             category=="10" ~ "Nursing",
             category=="11" ~ "Intermediate Care",
             category=="12" ~ "Rural Health Clinic",
             category=="14" ~ "Outpatient Rehab",
             category=="15" ~ "ASC",
             category=="16" ~ "Hospice",
             category=="17" ~ "Organ Procurement",
             category %in% c("18","22") ~ "CLIA Lab",
             category=="19" ~ "Community Health Center",
             category=="20" ~ "Screening Mammography",
             category=="21" ~ "Federally Qualified Health Center"))

## hospital data
final.hospital <- final.pos.data %>% filter(category=="Hospital") %>%
  mutate(category_sub =
           case_when(
             category_sub=="01" ~ "Short Term",
             category_sub=="02" ~ "Long Term",
             category_sub=="03" ~ "Religious Nonmedical",
             category_sub=="04" ~ "Psychiatric",
             category_sub=="05" ~ "Rehabilitation",
             category_sub=="06" ~ "Children",
             category_sub=="07" ~ "Partial Psychiatric",
             category_sub=="11" ~ "Critical Access",
             category_sub=="20" ~ "Transplant",
             category_sub=="22" ~ "Medicaid-only Non-psychiatric",
             category_sub=="23" ~ "Medicaid-only Psyciatric"),
         own_type =
           case_when(
             own_type=="01" ~ "Non-profit Church",
             own_type=="02" ~ "Non-profit Private",
             own_type=="03" ~ "Non-profit Other",
             own_type=="04" ~ "Profit",
             own_type=="05" ~ "Govt Federal",
             own_type=="06" ~ "Govt State",
             own_type=="07" ~ "Govt Local",
             own_type=="08" ~ "Govt Hospital District",
             own_type=="09" ~ "Physician Owned",
             own_type=="10" ~ "Tribal"),
         profit_status = 
           case_when(
             own_type %in% c("Non-profit Church", "Non-profit Private", "Non-profit Other") ~ "Non Profit",
             own_type %in% c("Physician Owned","Profit") ~ "For Profit",
             own_type %in% c("Govt Federal", "Govt State", "Govt Local", "Govt Hospital District") ~ "Government",
             own_type=="Tribal" ~ "Tribal")
  )


## home health data
final.hha <- final.pos.data %>% filter(category=="Home Health") %>%
  mutate(own_type =
           case_when(
             own_type=="01" ~ "Non-profit Church",
             own_type=="02" ~ "Non-profit Private",
             own_type=="03" ~ "Non-profit Other",
             own_type=="04" ~ "Profit",
             own_type=="05" ~ "Govt State-County",
             own_type=="06" ~ "Govt Voluntary",
             own_type=="07" ~ "Govt Local"),
         profit_status = 
           case_when(
             own_type %in% c("Non-profit Church", "Non-profit Private", "Non-profit Other") ~ "Non Profit",
             own_type == "Profit" ~ "For Profit",
             own_type %in% c("Govt State-County", "Govt Local", "Govt Voluntary") ~ "Government")
  )


## SNF data
final.snf <- final.pos.data %>% filter(category=="SNF") %>%
  mutate(own_type =
           case_when(
             own_type=="01" ~ "Profit Individual",
             own_type=="02" ~ "Profit Partnership",
             own_type=="03" ~ "Profit Corporation",
             own_type=="04" ~ "Non-profit Church",
             own_type=="05" ~ "Non-profit Corporation",
             own_type=="06" ~ "Non-profit Other",
             own_type=="07" ~ "Govt State",
             own_type=="08" ~ "Govt County",
             own_type=="09" ~ "Govt City",
             own_type=="10" ~ "Govt City-County",
             own_type=="11" ~ "Govt Hospital District",
             own_type=="12" ~ "Govt Federal",
             own_type=="13" ~ "Profit LLC"),
         profit_status = 
           case_when(
             own_type %in% c("Non-profit Church", "Non-profit Corporation", "Non-profit Other") ~ "Non Profit",
             own_type %in% c("Profit Individual", "Profit Partnership", "Profit Corporation", "Profit LLC") ~ "For Profit",
             own_type %in% c("Govt State", "Govt County", "Govt City", "Govt City-County", "Govt Hospital District", "Govt Federal") ~ "Government")
  )


## Hospice data
final.hospice <- final.pos.data %>% filter(category=="Hospice") %>%
  mutate(own_type =
           case_when(
             own_type=="01" ~ "Non-profit Church",
             own_type=="02" ~ "Non-profit Corporation",
             own_type=="03" ~ "Non-profit Other",
             own_type=="04" ~ "Profit Individual",
             own_type=="05" ~ "Profit Partnership",
             own_type=="06" ~ "Profit Corporation",
             own_type=="07" ~ "Profit Other",
             own_type=="08" ~ "Govt State",
             own_type=="09" ~ "Govt County",
             own_type=="10" ~ "Govt City",
             own_type=="11" ~ "Govt City-County",
             own_type=="12" ~ "Govt and Non-profit",
             own_type=="13" ~ "Other"),
         profit_status = 
           case_when(
             own_type %in% c("Non-profit Church", "Non-profit Corporation", "Non-profit Other") ~ "Non Profit",
             own_type %in% c("Profit Individual", "Profit Partnership", "Profit Corporation", "Profit Other") ~ "For Profit",
             own_type %in% c("Govt State", "Govt County", "Govt City", "Govt City-County", "Govt and Non-profit") ~ "Government")
  )


pos.combined <- bind_rows(final.hospital, final.hha, final.snf, final.hospice)

write_tsv(pos.combined,'data/output/pos_data_combined.txt',append=FALSE,col_names=TRUE)






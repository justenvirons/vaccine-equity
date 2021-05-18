
library(tidyverse)
library(dplyr) 
library(sf)
library(clipr)
library(xlsx)
library(leaflet)
library(scales)
library(ggmap)
library(ggplot2)
library(forcats)
library(lubridate)
library(zoo)
library(readxl)

# City of Chicago COVID-19 Vacccination Data ---------------------------------------
Chicago_COVID19Vaccinations_ByZCTA <- read_csv(file="https://data.cityofchicago.org/api/views/553k-3xzc/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

# Rename columns, reformat date, create week field
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% 
  rename("zcta"="Zip Code", 
         "date"="Date", 
         "doses_daily"="Total Doses - Daily", 
         "doses_cum"="Total Doses - Cumulative", 
         "fdose_daily"="1st Dose - Daily", 
         "fdose_cum"="1st Dose - Cumulative", 
         "fdose_pctpop"="1st Dose - Percent Population", 
         "com_daily"="Vaccine Series Completed - Daily", 
         "com_cum"="Vaccine Series Completed - Cumulative", 
         "com_pctpop"="Vaccine Series Completed  - Percent Population", 
         "pop"="Population", 
         "geometry"="ZIP Code Location") %>%
  select(-geometry) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         ymw = ceiling_date(date, "week"))

write_csv(Chicago_COVID19Vaccinations_ByZCTA, file="data/Chicago_COVID19Vaccinations_ByZCTA.csv")

# Summarize by date
Chicago_COVID19Vaccinations_ByDate_Sum <- Chicago_COVID19Vaccinations_ByZCTA %>%
  filter(pop>0) %>% 
  group_by(date) %>% 
  arrange(date) %>% 
  summarise(doses = sum(doses_daily), 
            dosescm = sum(doses_cum), 
            fdoses = sum(fdose_daily),
            fdosescm = sum(fdose_cum), 
            sdoses = sum(com_daily), 
            sdosescm = sum(com_cum))

write_csv(Chicago_COVID19Vaccinations_ByDate_Sum, file="data/Chicago_COVID19Vaccinations_ByDate_Sum.csv")

# Summarize by ZCTA
Chicago_COVID19Vaccinations_ByZCTA_Sum <- Chicago_COVID19Vaccinations_ByZCTA %>%
  filter(pop>0) %>% 
  group_by(zcta) %>% 
  summarise(doses = sum(doses_daily), 
            dosescm = sum(doses_cum), 
            fdoses = sum(fdose_daily),
            fdosescm = sum(fdose_cum), 
            sdoses = sum(com_daily), 
            sdosescm = sum(com_cum)) %>%
  mutate(ZCTA5=as.character(zcta))

write_csv(Chicago_COVID19Vaccinations_ByZCTA_Sum, file="data/Chicago_COVID19Vaccinations_ByZCTA_Sum.csv")

# City of Chicago COVID-19 Case, Death, Testing Data -----------------------------------------------

Chicago_COVID19_ByZCTA <- read_csv("https://data.cityofchicago.org/api/views/yhhz-zm2v/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19_ByZCTA <- Chicago_COVID19_ByZCTA %>% rename("ZCTA5"="ZIP Code", 
                                                            "WeekNo"="Week Number",
                                                            "StartDate"="Week Start",
                                                            "EndDate"="Week End",
                                                            "CasesWk"="Cases - Weekly",
                                                            "CasesCm"="Cases - Cumulative",
                                                            "CasesWkRt"="Case Rate - Weekly",
                                                            "CasesCmRt"="Case Rate - Cumulative",
                                                            "TestsWk"="Tests - Weekly",
                                                            "TestsCm"="Tests - Cumulative",
                                                            "TestsWkRt"="Test Rate - Weekly",
                                                            "TestsCmRt"="Test Rate - Cumulative",
                                                            "DeathsWk"="Deaths - Weekly",
                                                            "DeathsCm"="Deaths - Cumulative",
                                                            "DeathsWkRt"="Death Rate - Weekly",
                                                            "DeathsCmRt"="Death Rate - Cumulative",
                                                            "PctPosWk"="Percent Tested Positive - Weekly",
                                                            "PctPosCum"="Percent Tested Positive - Cumulative",
                                                            "TotPop"="Population",
                                                            "RowID"="Row ID",
                                                            "GEOM"="ZIP Code Location")
Chicago_COVID19_ByZCTA$ZCTA5 <- as.character(Chicago_COVID19_ByZCTA$ZCTA5)
Chicago_COVID19_ByZCTA$GEOM <- NULL
Chicago_COVID19_ByZCTA$StartDate <- as.Date(Chicago_COVID19_ByZCTA$StartDate, "%m/%d/%Y")+1 # reformat date text to date data type
Chicago_COVID19_ByZCTA$EndDate <- as.Date(Chicago_COVID19_ByZCTA$EndDate, "%m/%d/%Y")+1 # reformat date text to date data type
Chicago_COVID19_ByZCTA <- Chicago_COVID19_ByZCTA %>% 
  mutate(WeekNo = epiweek(EndDate),
         WeekNo = if_else(epiyear(EndDate)==2021,WeekNo+52,WeekNo)-1) %>%
  filter(EndDate == max(EndDate))

write_csv(Chicago_COVID19_ByZCTA, file="data/Chicago_COVID19CaseDeathsTests_ByZCTA.csv")

# City of Chicago CCVI by Zip Code -----------------------------------------------

Chicago_CCVI_ByZCTA <- read_csv("https://data.cityofchicago.org/api/views/2ns9-phjk/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
ZCTAs_ACS_Chicago <- read_csv("data/ZCTAs_ACS_Chicago.csv")

Chicago_CCVI_ByZCTA <- Chicago_CCVI_ByZCTA %>%
  select(zcta = `Community Area or ZIP Code`,
         ccvi_score = `CCVI Score`,
         ccvi_category = `CCVI Category`) %>%
  mutate(zcta = as.numeric(zcta))

ZCTAs_ACS_Chicago_mod <- ZCTAs_ACS_Chicago %>%
  left_join(Chicago_CCVI_ByZCTA,by=c("ZCTA5"="zcta")) %>%
  mutate(pcp=if_else(ccvi_category=="HIGH","YES","NO"))

write_csv(Chicago_CCVI_ByZCTA, file="data/Chicago_CCVI_ByZCTA.csv")

# Background maps ---------------------------------------------------------

Chicago_COVID19TestingLocations <- read_csv(file="https://data.cityofchicago.org/api/views/thdn-3grx/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19VaccinationLocations <- read_csv(file="https://data.cityofchicago.org/api/views/6q3z-9maq/rows.csv?accessType=DOWNLOAD")

# Tables ------------------------------------------------------------------

ZCTAs_ACS_Vac_geom %>%
  ungroup() %>%
  filter(date == max(date)) %>%
  select(RMAXCAT,
         fdose_cum,
         Pop18Over) %>%
  group_by("Dominant Group"=RMAXCAT) %>%
  summarize("First Doses" = format(sum(fdose_cum),scientific=FALSE,big.mark=","),
            "Pop 18+" = format(sum(Pop18Over),scientific=FALSE,big.mark=","),
            "First Dose Rate (%)" = signif(sum(fdose_cum)/sum(Pop18Over)*100,3))

ZCTAs_ACS_Vac_geom %>%
  ungroup() %>%
  filter(date == max(date)) %>%
  select(PCTBPOV,
         fdose_cum,
         Pop18Over) %>%
  arrange(PCTBPOV) %>%
  mutate(PCTBPOV_q=ntile(PCTBPOV,5)) %>%
  group_by("Poverty Quintile"=PCTBPOV_q) %>%
  summarize("Mean Poverty Rate" = signif(mean(PCTBPOV),3),
            "First Doses" = format(sum(fdose_cum),scientific=FALSE,big.mark=","),
            "Pop 18+" = format(sum(Pop18Over),scientific=FALSE,big.mark=","),
            "First Dose Rate (%)" = signif(sum(fdose_cum)/sum(Pop18Over)*100,3))

ZCTAs_ACS_Vac_geom %>%
  ungroup() %>%
  filter(date == max(date)) %>%
  select(pcp,
         fdose_cum,
         Pop18Over) %>%
  group_by("PCP Neighborhood"=pcp) %>%
  summarize("First Doses" = format(sum(fdose_cum),scientific=FALSE,big.mark=","),
            "Pop 18+" = format(sum(Pop18Over),scientific=FALSE,big.mark=","),
            "First Dose Rate (%)" = signif(sum(fdose_cum)/sum(Pop18Over)*100,3))
#


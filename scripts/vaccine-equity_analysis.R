## ---------------------------
##
## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-04-07
## Date Last Updated: 2021-04-07
## Email: c.scott.smith@depaul.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# activate packages
library(censusapi)
library(tigris) 
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

# Import demo data, covid vulnerability, other variables etc. -------------
ZCTA_select = st_read("layers/ZCTA_select.shp")

# Flag zip codes participating in Protect Chicago Plus program ------------
# https://www.accessliving.org/newsroom/action-alerts/updates-from-access-living-protect-chicago-plus-vaccine-program-al-town-hall-recordings-posted/

ZCTA_select <- ZCTA_select %>% 
  mutate(pcp = ifelse(
    ZCTA5=="60636" | 
      ZCTA5=="60609" | 
      ZCTA5=="60632" | 
      ZCTA5=="60623" |
      ZCTA5=="60629" |
      ZCTA5=="60621" |
      ZCTA5=="60628" |
      ZCTA5=="60632" |
      ZCTA5=="60620" |
      ZCTA5=="60644" |
      ZCTA5=="60639" |
      ZCTA5=="60617", 
    "YES", "NO")) %>%
  select(ZCTA5,pcp)

## Download data from Chicago data portal ----------------
### Vaccinations by day and zip code ----------------------------------------
Chicago_COVID19Vaccinations_ByZCTA <- read_csv(file="https://data.cityofchicago.org/api/views/553k-3xzc/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
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

# Summarize vaccinations by date
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

# Import previously downloaded 2019 demographic data by ZCTA
ZCTAs_ACS <- read_csv("data/demodatabyzcta_acs2019.csv") %>%
  mutate(ZCTA=as.character(ZCTA))

# Join with geometries, subset to Chicago ZCTAs
ZCTAs_ACS_geom <- ZCTA_select %>%
  st_drop_geometry() %>% 
  left_join(ZCTAs_ACS, by=c("ZCTA5"="ZCTA")) %>%
  rowwise() %>% 
  mutate(Pop18Over = sum(c_across(Pop18to29:Pop85Over)))

  

# Join with geometries, subset to Chicago ZCTAs
ZCTAs_ACS_Vac_geom <- Chicago_COVID19Vaccinations_ByZCTA %>%
  mutate(zcta = as.character(zcta)) %>%
  replace(., is.numeric(is.na(.)), "") %>%
  left_join(ZCTAs_ACS_geom, by=c("zcta"="ZCTA5")) %>%
  select(-pop)

ZCTAs_ACS_Vac_geom$NotVac_totpop <- ZCTAs_ACS_Vac_geom$Total-ZCTAs_ACS_Vac_geom$fdose_cum
ZCTAs_ACS_Vac_geom$NotVac_18over <- ZCTAs_ACS_Vac_geom$Pop18Over-ZCTAs_ACS_Vac_geom$fdose_cum
ZCTAs_ACS_Vac_geom$NotVac_16over <- ZCTAs_ACS_Vac_geom$Pop16Over-ZCTAs_ACS_Vac_geom$fdose_cum
ZCTAs_ACS_Vac_geom$NotVac_essen <- ZCTAs_ACS_Vac_geom$WESSEN-ZCTAs_ACS_Vac_geom$fdose_cum

load("data/Data_ByZCTA_20200604.RData")

# get date list from 2/1/2021 onward
aEndDateList <- ZCTAs_ACS_Vac_geom %>% 
  group_by(date) %>% 
  summarise() %>%
  drop_na()
aEndDateList <- aEndDateList[49:113,] # return subset of list
aEndDateList # view date list

# Run chi-square for all dates
for (i in 1:nrow(aEndDateList)) {
  ct_CountsbyCategory <- ZCTAs_ACS_Vac_geom %>%
    filter(date == aEndDateList$date[i]) %>%
    group_by(pcp) %>%
    drop_na() %>%
    summarize(Vaccinated = sum(fdose_cum),
              NotVaccinated = sum(NotVac_totpop))

  ct_CountsbyCategory[is.na(ct_CountsbyCategory)] <- 0
  ct_CountsbyCategory_t <- t(ct_CountsbyCategory[,-1])
  colnames(ct_CountsbyCategory_t) <- ct_CountsbyCategory$pcp
  
  chisq_Results <- chisq.test(ct_CountsbyCategory_t)
  achisqtable_obs <-  chisq_Results$observed
  achisqtable_exp <- round(chisq_Results$expected,2)
  achisqtable_exp <- achisqtable_exp[1,]
  achisqtable_obs <- achisqtable_obs[1,]
  achisqtable_cbind <- cbind(achisqtable_exp,achisqtable_obs)
  achisqtable_cbind <- as.data.frame.matrix(achisqtable_cbind)
  achisqtable_cbind$date <- aEndDateList$date[i]
  achisqtable_cbind$PValue <- chisq_Results$p.value
  achisqtable_cbind$XSqrd <- chisq_Results$statistic
  achisqtable_cbind$df <- chisq_Results$parameter
  achisqtable_cbind <- cbind(rownames(achisqtable_cbind), data.frame(achisqtable_cbind, row.names=NULL))
  achisqtable_All <- rbind(achisqtable_cbind,achisqtable_All)
}

achisqtable_All <- achisqtable_All %>% 
  rename("Vaccinated"=`rownames(achisqtable_cbind)`,
         "Expected"='achisqtable_exp',
         "Observed"='achisqtable_obs')
achisqtable_All$Residual <- achisqtable_All$Observed - achisqtable_All$Expected
write_clip(achisqtable_All)

# create residual plot
ggplot(achisqtable_All) + geom_point(aes(x=EndDate,y=Residual, color = Race), shape=1, size = 3, stroke=2) + geom_line(aes(x=EndDate,y=Residual, color = Race), size=1.5) + scale_color_manual(values=c('#A6758D','#8DB1D5','#FFC000','#FBA2A2')) + theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank(), legend.key=element_blank(), axis.ticks=element_line(size=1), text = element_text(family="arial", face="bold", size=18), axis.text.x = element_text(angle = 90)) 

# run these lines before running subsequent chi-square analyses
achisqtable_All <- achisqtable_cbind
achisqtable_All <- achisqtable_All[c(), ]

table(Vacs_Totpop$pcp, Vacs_Totpop$pcp)

ggplot(Vacs_Totpop) +
  aes(x = Total, fill = pcp) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

head(delete)

write_clip(delete)

# Join with 


ZCTAs_ACS_Vac_Sum_geom <- ZCTAs_ACS_Vac_geom %>% 
  mutate(doses7d = rollmean(x=doses_daily, k=7, fill=0, align="right"), 
         dosert_totalpop = doses_cum/Total*100,
         dosert_65over = doses_cum/Pop65Over*100,
         dosert_18over = doses_cum/Pop18Over*100,
         dosert_16over = doses_cum/Pop16Over*100,
         dosert_essen = doses_cum/WESSEN*100,
         fdoses7d = rollmean(x=fdose_daily, k=7, fill=0, align="right"), 
         fdosert_totalpop = fdose_cum/Total*100,
         fdosert_65over = fdose_cum/Pop65Over*100,
         fdosert_18over = fdose_cum/Pop18Over*100,
         fdosert_16over = fdose_cum/Pop16Over*100,
         fdosert_essen = fdose_cum/WESSEN*100,
         sdoses7d = rollmean(x=com_daily, k=7, fill=0, align="right"), 
         sdosert_totalpop = com_cum/Total*100,
         sdosert_65over = com_cum/Pop65Over*100,
         sdosert_18over = com_cum/Pop18Over*100,
         sdosert_16over = com_cum/Pop16Over*100,
         sdosert_essen = com_cum/WESSEN*100)

# Create latest vaccinations csv and shapefile
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% 
  mutate(zcta_char = as.character(zcta))
Chicago_COVID19Vaccinations_ByZCTA_GEOM <- Chicago_COVID19Vaccinations_ByZCTA %>% 
  select(-geometry) %>% 
  left_join(ZCTA_select, by=c("zcta_char"="ZCTA5"))
Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest <- Chicago_COVID19Vaccinations_ByZCTA_GEOM %>% 
  filter(date==max(date)) %>%
  drop_na(ZCTA5No)
st_write(Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest,"../layers/Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest.shp", append=FALSE)
write.csv(Chicago_COVID19Vaccinations_ByZCTA_Sum,"../data/vaccinationssummary_chicago.csv")


### Vaccine locations in the city -------------------------------------------
Chicago_COVID19VaccinationLocations <- read_csv(file="https://data.cityofchicago.org/api/views/6q3z-9maq/rows.csv?accessType=DOWNLOAD&bom=true&format=true")





plot(ZCTA_select["pcp"])
```




``` {r summary statistics, bivariate correlations}
# Create a bivariate correlation matrix for all factors.
# If you changed factors, be sure to change names in select function.
# Otherwise no editing required.

attach(VulnerabilityData_ByZCTA_geom)
res <- cor(VulnerabilityData_ByZCTA_sub, use = "complete.obs")
round(res, 2)
# corrplot.mixed(res, lower.col = "black") # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(res, type="upper",order = "hclust", addrect=6,method="square",tl.col = "black")
rm(res)

vulnerability.hclust <- hcut(VulnerabilityData_ByZCTA_naomit[,c(96:115,118)],4,stand=TRUE)
fviz_dend(vulnerability.hclust, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))

vulnerability.kmeans <- kmeans(VulnerabilityData_ByZCTA_naomit[,c(96:115,118)],4,nstart=25)
fviz_cluster(vulnerability.kmeans, data = VulnerabilityData_ByZCTA_naomit,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

```

```{r figures}

delete <- Chicago_COVID19Vaccinations_ByZCTA_GEOM_All %>% 
  select(-geometry)
write_clip(delete)

delete <- Chicago_COVID19Vaccinations_ByZCTA_GEOM_All %>%
  select(date, zcta, pcp, doses_daily) %>%
  group_by(date, zcta, pcp) %>%
  arrange(date) %>%
  summarise(sum(doses_daily))

delete <- Chicago_COVID19Vaccinations_ByZCTA_GEOM_All %>%
  select(pcp) %>%
  group_by(pcp) %>%
  summarise(n())

test <- Chicago_COVID19Vaccinations_ByZCTA_GEOM_All %>% 
  st_as_sf() %>%
  st_drop_geometry() %>%
  drop_na(Total) %>%
  # group_by(pcp) %>%
  mutate(dose_total=sum(doses_daily), 
         rate_daily=sum(doses_daily)/Total,
         rate_cum = doses_cum/Total) %>%
  select(date, zcta,Total, dose_total,rate_daily, rate_cum, pcp) %>%
  ggplot(aes(x = as.Date(ymw), y=rate, color=pcp)) + 
  geom_line(size=1.25) +
  geom_point(size=2) +
  # scale_y_continuous(labels = function(y) format(y, big.mark = ",")) + 
  # scale_x_date(date_breaks = "weeks" , date_labels = "%b %d") +
  # xlab('week ending') +
  # ylab('shots') +
  # scale_color_grey() + 
  theme_bw()


```


```{r archive covid-19 testing and vaccination locations}

adate <- as.character(Sys.Date())

Chicago_COVID19TestingLocations <- read_csv(file="https://data.cityofchicago.org/api/views/thdn-3grx/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19VaccinationLocations <- read_csv(file="https://data.cityofchicago.org/api/views/6q3z-9maq/rows.csv?accessType=DOWNLOAD")
write.csv(Chicago_COVID19VaccinationLocations, paste0("../archive/Chicago_COVID19VaccinationLocations_",adate,".csv"))
write.csv(Chicago_COVID19TestingLocations, paste0("../archive/Chicago_COVID19TestingLocations_",adate,".csv"))
```
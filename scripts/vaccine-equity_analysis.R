## ---------------------------
##
## Script name: vaccine-equity_analysis.R
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-03-24
## Date Last Updated: 2021-03-24
## Email: c.scott.smith@depaul.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

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

icare_p_shots_wk <- icare_p %>% 
  mutate(ymw = ceiling_date(date, "week"),
         dataset="provider") %>%
  select(ymw, dataset, shots = shot_count) %>%
  group_by(ymw, dataset) %>% 
  summarise(shots = sum(shots)) %>%
  arrange(ymw)


# activate installed packages
library(dplyr) # https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
library(tidyverse) # https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf
library(sf) # https://cran.r-project.org/web/packages/sf/sf.pdf
library(lubridate)
library(zoo)

# Download census geographies
ZCTA_select = st_read("layers/ZCTA_Select.shp")

# Download vacciations by zip code data for Chicago
Chicago_COVID19Vaccinations_ByZCTA <- read_csv(file="https://data.cityofchicago.org/api/views/553k-3xzc/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% rename("zcta"="Zip Code", "date"="Date", "doses_daily"="Total Doses - Daily", "doses_cum"="Total Doses - Cumulative", "fdose_daily"="1st Dose - Daily", "fdose_cum"="1st Dose - Cumulative", "fdose_pctpop"="1st Dose - Percent Population", "com_daily"="Vaccine Series Completed - Daily", "com_cum"="Vaccine Series Completed - Cumulative", "com_pctpop"="Vaccine Series Completed  - Percent Population", "pop"="Population", "geometry"="ZIP Code Location")
Chicago_COVID19Vaccinations_ByZCTA$date <- as.Date(Chicago_COVID19Vaccinations_ByZCTA$date, "%m/%d/%Y") # reformat date text to date data type
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% 
  mutate(ymw = ceiling_date(date, "week"),
         startdate = floor_date(date),
         enddate = ceiling_date(date))

# Create summaries for welcome page daily time series
Chicago_COVID19Vaccinations_ByZCTA_Sum <- Chicago_COVID19Vaccinations_ByZCTA %>%
  filter(pop>0) %>%group_by(date) %>% arrange(date) %>% summarise(doses = sum(doses_daily), dosescm = sum(doses_cum), fdoses = sum(fdose_daily),fdosescm = sum(fdose_cum), sdoses = sum(com_daily), sdosescm = sum(com_cum),pop=max(pop))
Chicago_COVID19Vaccinations_ByZCTA_Sum[is.na(Chicago_COVID19Vaccinations_ByZCTA_Sum)] <- 0

Chicago_COVID19Vaccinations_ByZCTA_Sum <- Chicago_COVID19Vaccinations_ByZCTA_Sum %>% 
  mutate(doses7d = rollmean(x=doses, k=7, fill=0, align="right"), 
         dosert = dosescm/pop*100, 
         fdoses7d = rollmean(x=fdoses, k=7, fill=0, align="right"), 
         fdosert = fdosescm/pop*100, sdoses7d = rollmean(x=sdoses, k=7, fill=0, align="right"), 
         sdosert = sdosescm/pop*100)

# Create latest vaccinations csv and shapefile
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% 
  mutate(zcta_char = as.character(zcta))
Chicago_COVID19Vaccinations_ByZCTA_GEOM <- Chicago_COVID19Vaccinations_ByZCTA %>% 
  select(-geometry) %>% 
  left_join(ZCTA_select, by=c("zcta_char"="ZCTA5"))
Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest <- Chicago_COVID19Vaccinations_ByZCTA_GEOM %>% 
  filter(date==max(date)) %>%
  drop_na(ZCTA5No)
st_write(Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest,"layers/Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest.shp", append=FALSE)
write.csv(Chicago_COVID19Vaccinations_ByZCTA_Sum,"layers/Vaccinations_Summary.csv")

adate <- as.character(Sys.Date())

# archive testing and vaccine locations
Chicago_COVID19TestingLocations <- read_csv(file="https://data.cityofchicago.org/api/views/thdn-3grx/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19VaccinationLocations <- read_csv(file="https://data.cityofchicago.org/api/views/6q3z-9maq/rows.csv?accessType=DOWNLOAD")
write.csv(Chicago_COVID19VaccinationLocations, paste0("archive/Chicago_COVID19VaccinationLocations_",adate,".csv"))
write.csv(Chicago_COVID19TestingLocations, paste0("archive/Chicago_COVID19TestingLocations_",adate,".csv"))

# plot vaccinations by ZCTA
icare_tp_shots_wk %>%
  ggplot(aes(x = as.Date(ymw), y=shots, color=dataset)) + 
  geom_line(size=1.25) +
  geom_point(size=2) +
  scale_y_continuous(labels = function(y) format(y, big.mark = ",")) + 
  scale_x_date(date_breaks = "weeks" , date_labels = "%b %d") +
  xlab('week ending') +
  ylab('shots') +
  scale_color_grey() + 
  theme_bw()

# https://www.accessliving.org/newsroom/action-alerts/updates-from-access-living-protect-chicago-plus-vaccine-program-al-town-hall-recordings-posted/
# flag zip codes participating in Protect Chicago Plus program


ZCTA_select %>% 
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
  select(ZCTA5,PCP) 

# Create a bivariate correlation matrix for all factors.
# If you changed factors, be sure to change names in select function.
# Otherwise no editing required.

attach(VulnerabilityData_ByZCTA_geom)
res <- cor(VulnerabilityData_ByZCTA_sub, use = "complete.obs")
round(res, 2)
# corrplot.mixed(res, lower.col = "black") # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(res, type="upper",order = "hclust", addrect=6,method="square",tl.col = "black")
rm(res)


VulnerabilityData_ByZCTA <- st_drop_geometry(VulnerabilityData_ByZCTA_geom)
VulnerabilityData_ByZCTA_naomit <- na.omit(VulnerabilityData_ByZCTA)
vulnerability.pca <- prcomp(VulnerabilityData_ByZCTA_naomit[,c(96:115,118)], center = TRUE,scale. = TRUE)
vulnerability.pca.factorscores <- get_pca_ind(vulnerability.pca)
vulnerability.pca.factorscores$coord

summary(vulnerability.pca)
# fviz_eig(vulnerability.pca)
fviz_screeplot(vulnerability.pca, addlabels = TRUE, ylim = c(0, 35))
vulnerability.pca.var <- get_pca_var(vulnerability.pca)
head(vulnerability.pca.var)
fviz_pca_var(vulnerability.pca, col.var = "black")
fviz_pca_var(vulnerability.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
fviz_contrib(vulnerability.pca, choice = "var", axes = 3, top = 10)

which( colnames(VulnerabilityData_ByZCTA)=="D1_AGE" )
which( colnames(VulnerabilityData_ByZCTA_geom)=="D5_PLM" )

vulnerability.pca.factorscores$coord[,1:10]


vulnerability.hclust <- hcut(VulnerabilityData_ByZCTA_naomit[,c(96:115,118)],4,stand=TRUE)
fviz_dend(vulnerability.hclust, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))

vulnerability.kmeans <- kmeans(VulnerabilityData_ByZCTA_naomit[,c(96:115,118)],4,nstart=25)
fviz_cluster(vulnerability.kmeans, data = VulnerabilityData_ByZCTA_naomit,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

# Confirmatory factor analysis
Vulnerability.model <- '
    f5_housing =~ D5_PLM + D5_OVR + D5_RNT + D5_HSB
    f4_isolation =~ D4_LEN + D4_DIS + D4_NVH + D4_DNS
    f3_socioeconomic =~ D3_CMP + D3_INT + D3_PRT + D3_POV
    f2_transmission =~ D2_SNP + D2_MFH + D2_TRA + D2_GRP + D2_ESS
    f1_severity =~ D1_MAL+D1_INS+D1_CLR+D1_AGE
'
cfa_fit <- cfa(Vulnerability.model,data=VulnerabilityData_ByZCTA_naomit)
summary(cfa_fit, fit.measures=TRUE)


#Add IDPH field to census tract table
IL_Counties <- st_drop_geometry(IL_Counties_geom) %>% select(COUNTYFP,IDPH)
VulnerabilityData_ByZCTA_geom <- left_join(VulnerabilityData_ByZCTA_geom, IL_Counties, by = "COUNTYFP")
VulnerabilityData_ByZCTA_geom <- VulnerabilityData_ByZCTA_geom %>% select(-contains(".y"))
rm(IL_Counties)

# Add GiZscores to census tract table
colnames(Vulnerability_GiScores)[1]<- "GEOID"
Vulnerability_GiScores$GEOID <- as.character(GEOID)
VulnerabilityData_ByZCTA_geom <- left_join(VulnerabilityData_ByZCTA_geom, Vulnerability_GiScores, by = "GEOID")

# Export simple geometry Illinois census tract and Illinois county files as
# shapefiles to be used in either QGIS or ArcGIS if desired.

st_write(VulnerabilityData_ByZCTA_geom, "VulnerabilityData_ByZCTA_geom_Gi.shp")
st_write(IL_Counties_geom, "IL_Counties_geom.shp")
st_write(IL_State_geom, "IL_State_geom.shp")
st_write(IL_Regions_geom, "IL_Regions_geom.shp")

# Export vulnerability data to CSV for Excel processing
VulnerabilityData_ByZCTA <- st_drop_geometry(VulnerabilityData_ByZCTA_geom)
write.csv(VulnerabilityData_ByZCTA,"VulnerabilityData.csv", row.names = FALSE)

attach(VulnerabilityData_ByZCTA)
VulnerabilityData_ByZCTA %>% group_by(IDPH) %>% summarize(Total = sum(TOTPOP,na.rm=TRUE), D1_median = median(D4,na.rm=TRUE),older=sum(POP60PL),older/Total*100, minority=sum(POPCLR),minority/Total*100) %>% arrange(-D1_median)

pal_fun <- colorQuantile("RdBu", NULL, n = 5, reverse = TRUE) # creates color pattern for all maps

ggplot() +
  geom_sf(data = IL_State_geom) +
  geom_sf(data = IL_State_geom, fill = NA, size = 2) +
  geom_sf(
    data = IL_Counties_geom,
    mapping = aes(fill = IDPH),
    size = 0.1,
    show.legend = TRUE
  ) +
  geom_sf(data = IL_Regions_geom, fill = NA, size = 1) +
  theme_map()


# Dimension 1 map with descriptive statistics pop-up
tract_popup <- paste("<strong>Tract #: </strong>", GEOID,
                     "<br>Population: ", round(TOTPOP,digits=2),"<br>",
                     "<br><strong>D1 Score: </strong>", format(D1,digits=2),
                     "<br>% Older: ", round(PCT60PL,digits=2),
                     "<br>% Minority: ", round(PCTCLR,digits=2),
                     "<br>% Not Insured: ", round(PCTNOHI,digits=2),
                     "<br>% Male: ", round(PCTMALE,digits=2),"<br>",
                     "<br>D1 (sev): ", round(D1,digits=2),
                     "<br>D2 (exp): ", round(D2,digits=2),
                     "<br>D3 (ses): ", round(D3,digits=2),
                     "<br>D4 (iso): ", round(D4,digits=2),
                     "<br>D5 (hsg): ", round(D5,digits=2),
                     sep="")

leaflet(VulnerabilityData_ByZCTA_geom) %>% addPolygons(stroke = FALSE, fillColor = ~pal_fun(DAll), fillOpacity = 0.5, smoothFactor = 0.5, popup = tract_popup) %>% addTiles() %>% addLegend(pal = pal_fun, values = DAll, opacity = 0.7, title = NULL, position = "bottomright") %>% addProviderTiles(providers$CartoDB.Positron)

# Step 9: Create a bivariate correlation matrix for all factors.
# If you changed factors, be sure to change names in select function.
# Otherwise no editing required.

attach(VulnerabilityData_ByZCTA_geom)
VulnerabilityData_ByZCTA %>% select (older = PCT60PL,minority = PCTCLR,"no insurance" = PCTNOHI,male = PCTMALE,essential = PCTWESSEN, group = PCTGQ,transit = PCTTRANS,multifamily = PCT10PL,snap = PCTSNAP,poverty = PCTBPOV,"part-time" = PCTWPART,"no internet" = PCTNOINT,"no computer" = PCTNOCMP,"no vehicles" = PCTNOVEH,disability = PCTDISAB,"no english" = PCTLTDENG,rural = D4_DNS, "housing burdened" = PCTOVER30,renter = PCTRNTOC,overcrowded = PCTOVER,"no plumbing" = PCTNOPLM)

VulnerabilityData_ByZCTA_sub2 <- st_drop_geometry(VulnerabilityData_ByZCTA_geom)

res <- cor(VulnerabilityData_ByZCTA_sub, use = "complete.obs")
round(res, 2)
# corrplot.mixed(res, lower.col = "black") # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(res, type="upper",order = "hclust", addrect=6,method="square",tl.col = "black")
rm(res)

# How to export data from R to CSV or clipboard
# write.csv(DataFrameName,"FileName.csv", row.names = FALSE)
# write_clip(DataFrameName)


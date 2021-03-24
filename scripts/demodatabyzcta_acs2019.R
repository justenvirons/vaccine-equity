## ---------------------------
##
## Script name: 
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


# Activate/install libraries
library(censusapi) 
library(dplyr) 
library(tidyverse) 
library(tigris) 
library(leaflet) 
library(sf) 
library(scales) 
library(corrplot) 
library(clipr)
library(ggmap)

# Download census geographies
# cb = cartographic boundary
IL_Places_cb <- places(state = "IL", cb=TRUE, year=2019, class="sf")
IL_Places_Chicago_cb <- IL_Places_cb %>% filter(NAME=="Chicago")
IL_Places_Chicago_cb <- st_transform(IL_Places_Chicago_cb, crs = 26916)
US_ZCTAs_cb <- zctas(cb=TRUE, year=2019, class="sf")
US_ZCTAs_cb <- st_transform(US_ZCTAs_cb, crs = 26916)
ZCTAs_Chicago_cb <- US_ZCTAs_cb %>% 
  filter(st_intersects(st_centroid(geometry), IL_Places_Chicago_cb, sparse = FALSE))

# Year for all subsequent ACS queries
ayear <- "2019"

# Download list of ACS 5-year tables
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5",
  # name = "cbp",
  vintage = ayear, 
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste("grouptable_",ayear,sep=""),acs_groups_tables) # change name of dataframe

# Download list of ACS 5-year subject tables
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5/subject",
  # name = "cbp",
  vintage = ayear, 
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste("subjectgrouptable_",ayear,sep=""),acs_groups_tables) # change name of dataframe

# Input list of variables for selected census table
agroup <- "B25024"
acs_groups_vars <- listCensusMetadata(
  name = "acs/acs5", 
  vintage = ayear,
  group = agroup,
  type = "variables")
acs_groups_vars$year<-ayear
acs_groups_vars <- acs_groups_vars %>% filter(!str_detect(name,"EA"),!str_detect(name,"M"))
assign(paste(ayear,"_groupvars_",agroup, sep=""),acs_groups_vars)
rm(acs_groups_vars)

# Input list of variables for selected census table
# https://cran.r-project.org/web/packages/censusapi/vignettes/example-masterlist.html
agroup <- "S0101"
acs_groups_vars <- listCensusMetadata(
  name = "acs/acs5/subject", 
  vintage = ayear,
  group = agroup,
  type = "variables")
acs_groups_vars$year<-ayear
# acs_groups_vars <- acs_groups_vars %>% filter(!str_detect(name,"EA"),!str_detect(name,"M"))
assign(paste("groupvars_",agroup,"_",ayear, sep=""),acs_groups_vars)
rm(acs_groups_vars)

# Download data for selected census tables
# Below is a list of census tables used for the covid dimensions analysis

# B01001: SEX BY AGE
# B15002: EDUCATIONAL ATTAINMENT
# B17001: POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
# B22010: RECEIPT OF FOOD STAMPS/SNAP BY DISABILITY STATUS
# B27010: TYPES OF HEALTH INSURANCE COVERAGE BY AGE (Civilian noninstitutionalized population)
# B03002: HISPANIC OR LATINO ORIGIN BY RACE
# B25044: TENURE BY VEHICLES AVAILABLE
# B08126: MEANS OF TRANSPORTATION TO WORK BY INDUSTRY
# B26001: GROUP QUARTERS
# B26101: GROUP QUARTERS TYPE BY SEX AND AGE
# B25024: UNITS IN STRUCTURE
# B25014: TENURE PER OCCUPANTS PER ROOM
# B23022: SEX BY WORK STATUS IN THE PAST 12 MONTHS BY USUAL HOURS WORKED PER WEEK
# B18101: SEX BY AGE BY DISABILITY STATUS
# B25106: HOUSING COSTS AS PERCENTAGE OF INCOME
# B25048: PLUMBING FACILITIES IN OCCUPIED HOUSING
# B28011: INTERNET SUBSCRIPTIONS
# B28001: DESKTOP OR LAPTOP COMPUTER
# C16002: HOUSEHOLD LANGUAGE BY HOUSEHOLD LIMITED ENGLISH SPEAKING STATUS
# S2401: WORKERS BY INDUSTRY ESSENTIAL WORKERS
 
grouplist <- c("B01001","B03002","B08126","B15002", "B17001","B18101","B22010","B23022","B25014","B25024","B25044","B25048","B25106","B26001","B26101","B27010","B28001","B28011","C16002")
# by ZCTA
yearlist <- c(2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "zip code tabulation area:*", # tracts
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    head(acs_group)
    attach(acs_group)
    acs_group <- acs_group %>% select(-ends_with("EA"))
    acs_group <- acs_group %>% select(-ends_with("MA"))
    acs_group <- acs_group %>% select(-contains("NAME_1"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-ends_with("M_1"))
    acs_group <- acs_group %>% select(-ends_with("M"))
    acs_group$year<-ayear 
    assign(paste(agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

grouplist <- c("S2401")
yearlist <- c(2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5/subject",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "zip code tabulation area:*", # ZCTAs
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    head(acs_group)
    attach(acs_group)
    acs_group <- acs_group %>% select(-ends_with("EA"))
    acs_group <- acs_group %>% select(-ends_with("MA"))
    acs_group <- acs_group %>% select(-contains("NAME_1"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-ends_with("M_1"))
    acs_group <- acs_group %>% select(-ends_with("M"))
    acs_group$year<-ayear 
    assign(paste(agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

# Create new variables based on the raw data in downloaded census tables. Subset
# only the essential variables into a separate table. Note: Select and run all
# of the code between comments. No editing required. Dimensions

# POPULATION BY AGE, GENDER
B01001_2019_sub <- B01001_2019 %>% 
  select(-ends_with("M")) %>%
  rowwise() %>%
  mutate(
  Total = sum(B01001_001E),
  PopUnd5 = sum(B01001_003E,B01001_027E),
  Pop5to17 = sum(c_across(B01001_004E:B01001_006E),c_across(B01001_028E:B01001_030E)),
  Pop18to29 = sum(c_across(B01001_007E:B01001_011E),c_across(B01001_031E:B01001_035E)),
  Pop30to44 = sum(c_across(B01001_012E:B01001_014E),c_across(B01001_036E:B01001_038E)),
  Pop45to54 = sum(c_across(B01001_015E:B01001_016E),c_across(B01001_039E:B01001_040E)),
  Pop55to64 = sum(c_across(B01001_017E:B01001_019E),c_across(B01001_041E:B01001_043E)),
  Pop65to74 = sum(c_across(B01001_020E:B01001_022E),c_across(B01001_044E:B01001_046E)),
  Pop75to84 = sum(c_across(B01001_023E:B01001_024E),c_across(B01001_047E:B01001_048E)),
  Pop85Over = sum(B01001_025E,B01001_049E),
  Pop16Over = sum(B01001_006E/3,B01001_030E/3, c_across(B01001_007E:B01001_025E),c_across(B01001_031E:B01001_049E)),
  Pct16Over = Pop16Over/Total*100,
  Pop65Over = sum(c_across(B01001_020E:B01001_025E), c_across(B01001_044E:B01001_049E)),
  Pct65Over = Pop65Over/Total*100
) %>%
  select(NAME, Total:Pct65Over)

attach(B03002_2019) # HISPANIC OR LATINO ORIGIN BY RACE
B03002_2019$TOTPOP <- B03002_001E
B03002_2019$POPBLK <- B03002_004E
B03002_2019$POPASN <- B03002_006E
B03002_2019$POPLAT <- B03002_012E
B03002_2019$POPCLR <- B03002_001E-B03002_003E
B03002_2019$POPWHT <- B03002_003E
B03002_2019$PCTBLK <- B03002_004E/B03002_001E*100
B03002_2019$PCTASN <- B03002_006E/B03002_001E*100
B03002_2019$PCTLAT <- B03002_012E/B03002_001E*100
B03002_2019$PCTCLR <- (B03002_001E-B03002_003E)/B03002_001E*100
B03002_2019$PCTWHT <- B03002_003E/B03002_001E*100
B03002_2019_sub <- B03002_2019 %>% select (POPBLK:PCTWHT)
detach(B03002_2019)

attach(B08126_2019) # MEANS OF TRANSPORTATION TO WORK
B08126_2019$WKTRANSTOT <- (B08126_001E)
B08126_2019$WKTRANS <- (B08126_046E)
B08126_2019$PCTTRANS <- (B08126_046E)/B08126_001E*100
B08126_2019$WKTWU <- (B08126_007E)
B08126_2019$PCTTWU <- (B08126_007E)/B08126_001E*100
B08126_2019_sub <- B08126_2019 %>% select (WKTRANSTOT:PCTTWU)
detach(B08126_2019)

attach(B15002_2019) # SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
B15002_2019$EDTOT <- B15002_001E  
B15002_2019$NOHS <- B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+B15002_020E+B15002_021E+B15002_022E+B15002_023E+B15002_024E+B15002_025E+B15002_026E+B15002_027E
B15002_2019$HSON <- B15002_011E+B15002_028E
B15002_2019$HSPL <- B15002_012E+B15002_013E+B15002_014E+B15002_015E+B15002_016E+B15002_017E+B15002_018E+B15002_029E+B15002_030E+B15002_031E+B15002_032E+B15002_033E+B15002_034E+B15002_035E
B15002_2019_sub <- B15002_2019 %>% select(EDTOT:HSPL) %>% 
  mutate(PCTNOHS=NOHS/EDTOT, 
         PCTHSON=HSON/EDTOT,
         PCTHSPL=HSPL/EDTOT)
detach(B15002_2019)

attach(B17001_2019) # POVERTY LEVEL INCOME
B17001_2019$POPBPOVTOT <- B17001_001E
B17001_2019$POPBPOV <- B17001_002E
B17001_2019$PCTBPOV <- B17001_002E/B17001_001E*100
B17001_2019_sub <- B17001_2019 %>% select (POPBPOVTOT:PCTBPOV)
detach(B17001_2019)

attach(B18101_2019) # DISABILITY STATUS
B18101_2019$POPDISABTOT <- B18101_001E
B18101_2019$POPDISAB <- (B18101_004E+B18101_007E+B18101_010E+B18101_013E+B18101_016E+B18101_019E+B18101_023E+B18101_026E+B18101_029E+B18101_032E+B18101_035E+B18101_038E) 
B18101_2019$PCTDISAB <- (B18101_004E+B18101_007E+B18101_010E+B18101_013E+B18101_016E+B18101_019E+B18101_023E+B18101_026E+B18101_029E+B18101_032E+B18101_035E+B18101_038E)/B18101_001E*100
B18101_2019_sub <- B18101_2019 %>% select (POPDISABTOT:PCTDISAB)
detach(B18101_2019)

attach(B22010_2019) # FOOD STAMPS
B22010_2019$HHSNAPTOT <- B22010_001E
B22010_2019$HHSNAP <- (B22010_002E) 
B22010_2019$PCTSNAP <- B22010_002E/B22010_001E*100
B22010_2019_sub <- B22010_2019 %>% select (HHSNAPTOT:PCTSNAP)
detach(B22010_2019)

attach(B23022_2019) # PERCENT OF WORKERS PART TIME
B23022_2019$WPARTTOT <- B23022_001E
B23022_2019$WPART <- (B23022_011E+B23022_018E+B23022_035E+B23022_042E) # < 35 HOURS
B23022_2019$PCTWPART <- (B23022_011E+B23022_018E+B23022_035E+B23022_042E)/B23022_001E*100
B23022_2019_sub <- B23022_2019 %>% select (WPARTTOT:PCTWPART)
detach(B23022_2019)

attach(B25014_2019) # TENURE BY OCCUPANTS PER ROOM
B25014_2019$HUOVERTOT <- B25014_001E
B25014_2019$HUOVER <- (B25014_006E+B25014_007E+B25014_012E+B25014_013E) # > 1.5 OCCUPANTS PER ROOM
B25014_2019$PCTOVER <- (B25014_006E+B25014_007E+B25014_012E+B25014_013E)/B25014_001E*100
B25014_2019_sub <- B25014_2019 %>% select (HUOVERTOT:PCTOVER)
detach(B25014_2019)

attach(B25044_2019) # TENURE BY VEHICLES AVAILABLE
B25044_2019$HHVEHTOT <- B25044_001E
B25044_2019$HHNOVEH <- (B25044_003E+B25044_010E)
B25044_2019$PCTNOVEH <- (B25044_003E+B25044_010E)/B25044_001E*100
B25044_2019$HHRNTOC <- (B25044_009E)
B25044_2019$PCTRNTOC <- (B25044_009E)/B25044_001E*100
B25044_2019_sub <- B25044_2019 %>% select (HHVEHTOT:PCTRNTOC)
detach(B25044_2019)

attach(B25048_2019) # PLUMBING FACILITIES IN OCCUPIED HOUSING
B25048_2019$HUPLMTOTAL <- B25048_001E
B25048_2019$HUNOPLM <- (B25048_003E) 
B25048_2019$PCTNOPLM <- (B25048_003E)/B25048_001E*100
B25048_2019_sub <- B25048_2019 %>% select (HUPLMTOTAL:PCTNOPLM)
detach(B25048_2019)

attach(B25106_2019) # HOUSING COSTS AS PERCENTAGE OF INCOME
B25106_2019$HHOVERTOTAL <- B25106_001E
B25106_2019$HHOVER30 <- (B25106_006E+B25106_010E+B25106_014E+B25106_018E+B25106_022E+B25106_028E+B25106_032E+B25106_036E+B25106_040E+B25106_044E) # > 30% of HH INCOME
B25106_2019$PCTOVER30 <- (B25106_006E+B25106_007E+B25106_012E+B25106_013E)/B25106_001E*100
B25106_2019_sub <- B25106_2019 %>% select (HHOVERTOTAL:PCTOVER30)
detach(B25106_2019)

attach(B25024_2019) # UNITS IN STRUCTURE
B25024_2019$HUDWTOTAL <- B25024_001E
B25024_2019$HU2PL <- (B25024_004E+B25024_005E+B25024_006E+B25024_007E+B25024_008E+B25024_009E+B25024_010E+B25024_011E)
B25024_2019$HU10PL <- (B25024_007E+B25024_008E+B25024_009E+B25024_010E+B25024_011E)
B25024_2019$PCT2PL <- (B25024_004E+B25024_005E+B25024_006E+B25024_007E+B25024_008E+B25024_009E+B25024_010E+B25024_011E)/B25024_001E*100
B25024_2019$PCT10PL <- (B25024_007E+B25024_008E+B25024_009E+B25024_010E+B25024_011E)/B25024_001E*100
B25024_2019_sub <- B25024_2019 %>% select (HUDWTOTAL, HU2PL, HU10PL, PCT2PL,PCT10PL)
detach(B25024_2019)

attach(B26001_2019) # GROUP QUARTERS
B26001_2019$POPGQ <- B26001_001E
B26001_2019_sub <- B26001_2019 %>% select (POPGQ)
detach(B26001_2019)

attach(B27010_2019) # PERCENT NO HEALTH INSURANCE
B27010_2019$POPHITOTAL <- B27010_001E
B27010_2019$POPNOHI <- (B27010_017E+B27010_033E+B27010_050E+B27010_066E)
B27010_2019$PCTNOHI <- (B27010_017E+B27010_033E+B27010_050E+B27010_066E)/B27010_001E*100
B27010_2019_sub <- B27010_2019 %>% select (POPHITOTAL:PCTNOHI)
detach(B27010_2019)

attach(B28001_2019) # LAPTOP OR DESKTOP COMPUTER AVAIL
B28001_2019$HHCMPTOTAL <- B28001_001E
B28001_2019$HHNOCMP <- (B28001_011E) # no computer
B28001_2019$PCTNOCMP <- (B28001_011E)/B28001_001E*100
B28001_2019$HHNOLAP <- (B28001_001E-B28001_003E) # no laptop or desktop computer
B28001_2019$PCTNOLAP <- 100-(B28001_003E)/B28001_001E*100
B28001_2019$HHPHON <- (B28001_006E) # Smartphone only
B28001_2019$PCTPHON <- (B28001_006E)/B28001_001E*100
B28001_2019_sub <- B28001_2019 %>% select (HHCMPTOTAL:PCTPHON)
detach(B28001_2019)

attach(B28011_2019) # INTERNET SUBSCRIPTIONS
B28011_2019$HUINTTOTAL <- B28011_001E
B28011_2019$HUNOINT <- (B28011_008E) 
B28011_2019$PCTNOINT <- (B28011_008E)/B28011_001E*100
B28011_2019_sub <- B28011_2019 %>% select (HUINTTOTAL:PCTNOINT) 
detach(B28011_2019)

attach(C16002_2019) # LANGUAGE AT HOME, LIMITED ENGLISH
C16002_2019$HHLTDENGTOT <- C16002_001E
C16002_2019$HHLTDENG <- (C16002_004E+C16002_007E+C16002_010E+C16002_013E) 
C16002_2019$PCTLTDENG <- (C16002_004E+C16002_007E+C16002_010E+C16002_013E)/C16002_001E*100
C16002_2019_sub <- C16002_2019 %>% select (HHLTDENGTOT:PCTLTDENG)
detach(C16002_2019)

attach(S2401_2019) # ESSENTIAL WORKERS
S2401_2019$WORKERS <- S2401_C01_001E
S2401_2019$WCONMAN <- (S2401_C01_024E+S2401_C01_031E+S2401_C01_032E+S2401_C01_034E)
S2401_2019$PCTWCONMAN <- (S2401_C01_024E+S2401_C01_031E+S2401_C01_032E+S2401_C01_034E)/S2401_C01_001E*100
S2401_2019$WFOOD <- (S2401_C01_023E+S2401_C01_030E)
S2401_2019$PCTWFOOD <- (S2401_C01_023E+S2401_C01_030E)/S2401_C01_001E*100
S2401_2019$WHEALTH <- (S2401_C01_016E+S2401_C01_017E+S2401_C01_019E)
S2401_2019$PCTWHEALTH <- (S2401_C01_016E+S2401_C01_017E+S2401_C01_019E)/S2401_C01_001E*100
S2401_2019$WPROT <- (S2401_C01_021E+S2401_C01_022E)
S2401_2019$PCTWPROT <- (S2401_C01_021E+S2401_C01_022E)/S2401_C01_001E*100
S2401_2019$WSOCIAL <- S2401_C01_011E
S2401_2019$PCTWSOCIAL <- (S2401_C01_011E)/S2401_C01_001E*100
S2401_2019$WTRANS <- (S2401_C01_035E+S2401_C01_036E)
S2401_2019$PCTWTRANS <- (S2401_C01_035E+S2401_C01_036E)/S2401_C01_001E*100
S2401_2019$WESSEN <- (S2401_C01_024E+S2401_C01_031E+S2401_C01_032E+S2401_C01_034E+S2401_C01_023E+ S2401_C01_030E+S2401_C01_016E+S2401_C01_017E+S2401_C01_019E+S2401_C01_021E+S2401_C01_022E+S2401_C01_011E+S2401_C01_035E+S2401_C01_036E)
S2401_2019$PCTWESSEN <- (S2401_C01_024E+S2401_C01_031E+S2401_C01_032E+S2401_C01_034E+S2401_C01_023E+ S2401_C01_030E+S2401_C01_016E+S2401_C01_017E+S2401_C01_019E+S2401_C01_021E+S2401_C01_022E+S2401_C01_011E+S2401_C01_035E+S2401_C01_036E)/S2401_C01_001E*100
S2401_2019_sub <- S2401_2019 %>% select (WORKERS:PCTWESSEN)
detach(S2401_2019)

# Combine all of the above subset tables into a single vulnerability
# data table. Remove unnecessary tables.

VulnerabilityData_ByZCTA <- B01001_2019_sub %>% bind_cols(B03002_2019_sub, 
                                                          B08126_2019_sub,
                                                          B15002_2019_sub, 
                                                          B17001_2019_sub,
                                                          B18101_2019_sub,
                                                          B22010_2019_sub,
                                                          B23022_2019_sub, 
                                                          B25014_2019_sub, 
                                                          B25024_2019_sub, 
                                                          B25044_2019_sub, 
                                                          B25048_2019_sub, 
                                                          B25106_2019_sub, 
                                                          B26001_2019_sub, 
                                                          B27010_2019_sub, 
                                                          B28001_2019_sub, 
                                                          B28011_2019_sub, 
                                                          C16002_2019_sub, 
                                                          S2401_2019_sub)

VulnerabilityData_ByZCTA <- VulnerabilityData_ByZCTA %>% mutate(ZCTA = str_replace(NAME,"ZCTA5 ",""))
VulnerabilityData_ByZCTA$PCTGQ <- VulnerabilityData_ByZCTA$POPGQ/VulnerabilityData_ByZCTA$Total*100
VulnerabilityData_ByZCTA_geom <- left_join(VulnerabilityData_ByZCTA,US_ZCTAs_cb,by=c("ZCTA"="ZCTA5CE10"))
VulnerabilityData_ByZCTA <- VulnerabilityData_ByZCTA_geom %>% 
  filter(st_intersects(st_centroid(geometry), IL_Places_Chicago_cb, sparse = FALSE) | ZCTA == "60827" | ZCTA == "60707")
VulnerabilityData_ByZCTA_nogeom <- VulnerabilityData_ByZCTA 
write.csv(VulnerabilityData_ByZCTA, "data/demodatabyzcta_acs2019.csv")


rm(list=ls(pattern="_sub"))
rm(list=ls(pattern="2019")) 

# remove zctas outside the city and zctas with no population




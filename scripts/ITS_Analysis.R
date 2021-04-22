library(plotly)

URL <- "https://github.com/DS4PS/pe4ps-textbook/blob/master/data/time-series-example.rds?raw=true"
dataTS <- readRDS(gzcon(url( URL )))

# summary by all ZCTAs

ZCTAs_ACS_Vac_geom %>%
  ggplot() +
  aes(x=T, y=Y, shape=zcta, color=zcta) +
  geom_point()

dataTS <- ZCTAs_ACS_Vac_geom %>%
  select(zcta,
         pcp,
         date,
         T,
         D,
         P,
         Y = dosert_18over)


aplot <- dataTS %>%
  filter(pcp=="NO" & zcta==60603) %>%
  ggplot() +
  aes(x=T, y=Y, color=zcta) +
  geom_point()

aplot

library(ggplot2)
ggplot(dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 100), xlim=c(0,120),
      xlab = "Time (days)", 
      ylab = "First Dose Vaccination Rate (%)" )

# Line marking the interruption
abline( v=53, col="firebrick", lty=2 )
text( 38, 80, "February 5, 2021\nInitial Rollout of\nProtect Chicago Plus", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=dataTS )
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )


# summary by pcp
dataTS <- ZCTAs_ACS_Vac_ByPCP_geom %>%
  select(pcp, 
         date,
         T,
         D,
         P,
         Y)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 100), xlim=c(0,120),
      xlab = "Time (days)", 
      ylab = "First Dose Vaccination Rate (%)" )

# Line marking the interruption
abline( v=53, col="firebrick", lty=2 )
text( 5, 100, "Pre-Intervention", col="firebrick", cex=1.3, pos=4 )
text( 65, 100, "Post-Intervention", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=dataTS %>% filter(pcp=="NO") )
dataTS_No <- dataTS %>% select(T) %>% filter(pcp=="NO")
lines(dataTS_No$T, ts$fitted.values, col="black", lwd=2 )

ts <- lm( Y ~ T + D + P, data=dataTS %>% filter(pcp=="YES") )
dataTS_Yes <- dataTS %>% select(T) %>% filter(pcp=="YES")
lines(dataTS_Yes$T, ts$fitted.values, col="black", lty=2 )


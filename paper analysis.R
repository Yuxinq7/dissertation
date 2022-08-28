library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(flextable)
install.packages("broom")
install.packages("car")
install.packages("stargazer")
install.packages("spatialreg")
install.packages("splm")
library(sf)
library(tidycensus)
library(tmap)
library(spdep)
library(splm)
library(tigris)
library(rmapshaper)
library(broom)
library(spatialreg)
library(knitr)
library(stargazer)
library(here)

if (!requireNamespace("devtools", quietly = TRUE)){ install.packages("devtools") }
devtools::install_github("kvittingseerup/IsoformSwitchAnalyzeR", build_vignettes = TRUE)


#Spatial weighting matrix
dinb <- read.gal(here::here("weight.gal"))
dinb_mat <- nb2listw(dinb, style="W", zero.policy=TRUE)



#2010
divorce.tracts2010 <- st_read(here::here("xin","2010.shp"))

fit.ols2010 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2010)
summary(fit.ols2010)
vif(fit.ols2010)
ols.step2010 <- step(fit.ols2010, direction = "both")
summary(ols.step2010)
as_flextable(ols.step2010)
lm.morantest(ols.step2010, dinb_mat)


#2011
divorce.tracts2011 <- st_read(here::here("xin","2011.shp"))
fit.ols2011 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2011)
summary(fit.ols2011)

ols.step2011 <- step(fit.ols2011, direction = "both")
summary(ols.step2011)
as_flextable(ols.step2011)
lm.morantest(ols.step2011, dinb_mat)



#2012
divorce.tracts2012 <- st_read(here::here("xin","2012.shp"))
fit.ols2012 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2012)
summary(fit.ols2012)

ols.step2012 <- step(fit.ols2012, direction = "both")
summary(ols.step2012)
as_flextable(ols.step2012)
lm.morantest(ols.step2012, dinb_mat)




#2013
divorce.tracts2013 <- st_read(here::here("xin","2013.shp"))

fit.ols2013 <- lm(divorce ~ unemploy+ sex+ houseprice+pergdp +phone+ housesize+ urban+crime+ edu+womenwork, data = divorce.tracts2013)
summary(fit.ols2013)

ols.step2013 <- step(fit.ols2013, direction = "both")
summary(ols.step2013)
as_flextable(ols.step2013)
lm.morantest(ols.step2013, dinb_mat)


#2014
divorce.tracts2014 <- st_read(here::here("xin","2014.shp"))
fit.ols2014 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2014)
summary(fit.ols2014)

ols.step2014 <- step(fit.ols2014, direction = "both")
summary(ols.step2014)
as_flextable(ols.step2014)
lm.morantest(ols.step2014, dinb_mat)



#2015
divorce.tracts2015 <- st_read(here::here("xin","2015.shp"))
fit.ols2015 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2015)
summary(fit.ols2015)

ols.step2015 <- step(fit.ols2015, direction = "both")
summary(ols.step2015)
as_flextable(ols.step2015)
lm.morantest(ols.step2015, dinb_mat)


#2016
divorce.tracts2016 <- st_read(here::here("xin","2016.shp"))
fit.ols2016 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2016)
summary(fit.ols2016)

ols.step2016 <- step(fit.ols2016, direction = "both")
summary(ols.step2016)
as_flextable(ols.step2016)
lm.morantest(ols.step2016, dinb_mat)


#2017
divorce.tracts2017 <- st_read(here::here("xin","2017.shp"))
fit.ols2017 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2017)
summary(fit.ols2017)

ols.step2017 <- step(fit.ols2017, direction = "both")
summary(ols.step2017)
as_flextable(ols.step2017)
lm.morantest(ols.step2017, dinb_mat)


#2018
divorce.tracts2018 <- st_read(here::here("xin","2018.shp"))
fit.ols2018 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu+crime +housesize +womenwork, data = divorce.tracts2018)
summary(fit.ols2018)

ols.step2018 <- step(fit.ols2018, direction = "both")
summary(ols.step2018)
as_flextable(ols.step2018)
lm.morantest(ols.step2018, dinb_mat)


divorce.tracts2018 <- divorce.tracts2018 %>%
  mutate(olsresid = resid(ols.step2018))

tm_shape(divorce.tracts2018, unit = "mi") +
  tm_polygons(col = "olsresid", style = "equal",palette = "Reds",
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "Residuals from linear regression in China Tracts",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE,
            attr.outside = TRUE)




#2019
divorce.tracts2019 <- st_read(here::here("xin","2019.shp"))
fit.ols2019 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu +housesize +crime+womenwork, data = divorce.tracts2019)
summary(fit.ols2019)

ols.step2019 <- step(fit.ols2019, direction = "both")
summary(ols.step2019)
as_flextable(ols.step2019)
lm.morantest(ols.step2019, dinb_mat)


#2020
divorce.tracts2020 <- st_read(here::here("xin","2020.shp"))
fit.ols2020 <- lm(divorce ~ unemploy+ sex+ houseprice+ pergdp+ phone+ urban+ edu+ covid19 +housesize +crime+womenwork, data = divorce.tracts2020)
summary(fit.ols2020)
vif(fit.ols2020)
as_flextable(fit.ols2020)
#Residuals Distribution maps
divorce.tracts2020 <- divorce.tracts2020 %>%
  mutate(olsresid2020 = resid(ols.step2020))

tm_shape(divorce.tracts2020, unit = "mi") +
  tm_polygons(col = "olsresid2020", style = "equal",palette = "Reds",
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "Residuals from linear regression in China Tracts",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE,
            attr.outside = TRUE)

ols.step2020 <- step(fit.ols2020, direction = "both")
summary(ols.step2020)
as_flextable(ols.step2020)
lm.morantest(ols.step2020, dinb_mat)


#2018 and 2020 have spatial autocorrelation
#Separate analysis of these two years with spatial autocorrelation
#Spatially lagged model 2018
fit.lag<-lagsarlm(divorce ~ houseprice + edu + housesize + womenwork,  data = divorce.tracts2018, listw = dinb_mat)
summary(fit.lag)

#Spatial Error Model 2018
fit.err<-errorsarlm(divorce ~ houseprice + edu + housesize + womenwork,  data = divorce.tracts2018, listw = dinb_mat)
summary(fit.err)

#LM test
lm.LMtests(ols.step2018, listw = dinb_mat, test = "all",  zero.policy=TRUE)

stargazer(ols.step2018, fit.lag, fit.err, type = "html",title="Title: Regression Results in 2018", out='C:/Users/yuxin/Desktop/18hao')



#Spatial lag model2020
fit.lag2020<-lagsarlm(divorce ~ houseprice + edu + housesize + unemploy,  data = divorce.tracts2020, listw = dinb_mat)
summary(fit.lag2020)

#Spatial Error Model 2020
fit.err2020<-errorsarlm(divorce ~ houseprice + edu + housesize + unemploy,  data = divorce.tracts2020, listw = dinb_mat)
summary(fit.err2020)

#LM test
lm.LMtests(ols.step2020, listw = dinb_mat, test = "all",  zero.policy=TRUE)

stargazer(ols.step2020, fit.lag2020, fit.err2020, type = "html",title="Title: Regression Results in 2020", out='C:/Users/yuxin/Desktop/20')



AIC(ols.step2020)
AIC(fit.err)
AIC(fit.lag)

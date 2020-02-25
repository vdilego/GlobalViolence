###############################################################################
# Some correlation tests to address Alyson and Ridhi´s comments on why        #
# standard deviation is more meaningful than life expectancy to describe      #
# the relationship with violence.                                             #
# Author: vanessa                                                             #
###############################################################################

library(here)
library(countrycode)
library(htmltab)
library(data.table)

GBD_est <- readRDS(here("GlobalViolence","Data","Results","GBD", "GBDmid.rds"))
GBD_est$ISO3<-countrycode(GBD_est$location, origin="country.name", destination="iso3c")

#bringing GPI into scene
GPI <- data.table(htmltab("https://en.wikipedia.org/wiki/Global_Peace_Index",2))
GPI.dt         <- melt.data.table(data = GPI,id.vars = 1)
GPI.dt$country <- ifelse(GPI.dt$Country == sort(unique(GPI.dt$Country))[1], substr(GPI.dt$Country,7,nchar(GPI.dt$Country)),
                         ifelse(GPI.dt$Country == sort(unique(GPI.dt$Country))[2],substr(GPI.dt$Country,5,nchar(GPI.dt$Country)),
                                substr(GPI.dt$Country,3,nchar(GPI.dt$Country))))
GPI.dt$year    <- as.numeric(substr(GPI.dt$variable,1,4))
GPI.dt$type    <- substr(GPI.dt$variable,6,nchar(as.character(GPI.dt$variable)))
GPI.dt$value   <- as.numeric(GPI.dt$value)
GPI.dt         <- GPI.dt[order(year,country), c('country','year','type','value')]
GPI.dt




variants <- c("low","mid","upp")
for (i in 1:length(variants)){
  
  # choose an explicit closeout file
  # don´t know why but for me "sex" was saved without capital "s"
  # also now you add and "a" before all causes of death (check sequence from DataPrep)..
  
  GBDi <- readRDS(here("GlobalViolence","Data","Closeout","GBD",
                       paste0("GBD",variants[i],"_ggompertz_65_90_65.rds")))
  GBDi[,sdx:=mx2sd(Ma),.(location,year,sex)]
  GBDi[,sdx_no_h:=mx2sd(Ma-Mh),.(location,year,sex)]
  GBDi[,sdx_no_hw:=mx2sd(Ma-Mh-Mw),.(location,year,sex)]
  GBDi[,edx:=mx2edagger(Ma),.(location,year,sex)]
  GBDi[,edx_no_h:=mx2edagger(Ma-Mh),.(location,year,sex)]
  GBDi[,edx_no_hw:=mx2edagger(Ma-Mh-Mw),.(location,year,sex)]
  GBDi[,ex:=mx2ex(Ma),.(location,year,sex)]
  GBDi[,ex_no_h:=mx2ex(Ma-Mh),.(location,year,sex)]
  GBDi[,ex_no_hw:=mx2ex(Ma-Mh-Mw),.(location,year,sex)]
  saveRDS(GBDi, file = here("GlobalViolence","Data","Results","GBD",paste0("GBD",variants[i],".rds")))
  rm(GBDi);gc()
}

# Just checking Calculating Pearson's product-moment correlation
# uncertainty (sdx) versus GPI, males, age 15
cor.test(GBD_GPI_male$sdx, GBD_GPI_male$value, method = "pearson", conf.level = 0.95)

# life expectancy at age 15, males versus GPI
cor.test(GBD_GPI_male$ex, GBD_GPI_male$value, method = "pearson", conf.level = 0.95)


# uncertainty free from homicide at age 15, males versus GPI
cor.test(GBD_GPI_male$sdx_no_h, GBD_GPI_male$value, method = "pearson", conf.level = 0.95)


# life expectancy free from homicide at age 15, males versus GPI
cor.test(GBD_GPI_male$ex_no_h, GBD_GPI_male$value, method = "pearson", conf.level = 0.95)

# is GPI normally distributed?
# Shapiro-Wilk normality test for GPI
shapiro.test(data_map_men$value)
library("ggpubr")
ggqqplot(data_map_men$value, ylab = "GPI") # not normal

# Shapiro-Wilk normality test for sdx
shapiro.test(data_map_men$sdx)
ggqqplot(data_map_men$sdx, ylab = "sdx") # not normal

# Shapiro-Wilk normality test for ex
shapiro.test(data_map_men$ex)
ggqqplot(data_map_men$ex, ylab = "ex")   # not normal


data_map_men<-within(data_map_men, continent.y[is.na(continent.y)] <- continent.x[is.na(continent.y)])

data_map_men<-within(data_map_men, continent.y[is.na(continent.y)] <- continent.x[is.na(continent.y)])

#because all indicators are not normally distributed we must use either spearman or kendall correlation test
data_map_men_cor_ex<-data_map_men %>% 
  group_by() %>%
  summarise(crr = (cor.test(ex, value, method = "kendall", conf.level = 0.95)$estimate))
data_map_men_p_ex<-data_map_men %>% 
  group_by(location) %>%
  summarise(pval = (cor.test(ex, value, method = "kendall", conf.level = 0.95)$p.value))

# I think it is better to use kendall for some reasons:
# 1. It is more robust, i.e. smaller gross error sensitivity (GES);
# 2. It is more efficient, i.e. Smaller asymptotic variance (AV);
# 3. It handles better data with ties (which is our case)
# ref on this: Croux, C. and Dehon, C. (2010). 
# Influence functions of the Spearman and Kendall correlation measures. 
# Statistical Methods and Applications, 19, 497-515. 

data_map_men_cor_sd<-data_map_men %>% 
  # group_by(continent.y) %>%
  summarise(crr = (cor.test(sdx, value, method = "kendall", conf.level = 0.95)$estimate))
data_map_men_p_sd<-data_map_men %>% 
  #group_by(continent.y) %>%
  summarise(pval = (cor.test(sdx, value, method = "kendall", conf.level = 0.95)$p.value))


data_map_men_cor_sd_no<-data_map_men %>% 
  # group_by(continent.y) %>%
  summarise(crr = (cor.test(sdx_no_h, value, method = "kendall", conf.level = 0.95)$estimate))
data_map_men_p_sd_no<-data_map_men %>% 
  #group_by(continent.y) %>%
  summarise(pval = (cor.test(sdx_no_h, value, method = "kendall", conf.level = 0.95)$p.value))


data_map_men_cor_ex_no<-data_map_men %>% 
  # group_by(continent.y) %>%
  summarise(crr = (cor.test(ex_no_h, value, method = "kendall", conf.level = 0.95)$estimate))
data_map_men_p_ex_no<-data_map_men %>% 
  #group_by(continent.y) %>%
  summarise(pval = (cor.test(ex_no_h, value, method = "kendall", conf.level = 0.95)$p.value))



X11()
library(ggplot2)
ggplot(data_map_men_cor, aes(reorder(continent.y, crr), crr)) +
  geom_col()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))


tmap_mode("view")
tm_shape(data_map_men) +
  tm_polygons(c("sdx", "value")) +
  tm_facets(sync = TRUE, ncol = 2)

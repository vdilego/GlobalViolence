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
library(ggpubr)

GBD_est <- readRDS(here("GlobalViolence","Data","Results","GBD", "GBDmid.rds"))
GBD_est$ISO3<-countrycode(GBD_est$location, origin="country.name", destination="iso3c")
View(GBD_est)

#bringing GPI into scene
GPI_ISO3 <- read_csv(here("GlobalViolence","Data","Inputs","GPI", "GPI_ISO3.csv"))
View(GPI_ISO3)

# selecting only the latest years for the GPI and the score value
GPI_2017<-GPI_ISO3 %>% filter(type=="score" & year ==2017)
summary(GPI_2017$value)


# selecting only the latest years for the GBD as well and start ate age 15

# ages 15 only
GBD_15_2017<-GBD_est %>% filter(age==15 & year ==2017)
View(GBD_15_2017)

# ages 15 only, males
GBD_15_2017_male<-GBD_15_2017 %>% filter(sex=="Male")
View(GBD_15_2017_male)

# Azerbaijan, Albania, Tunisia and Uzbekistan have NaN edx values.
# Substitute for 0 so  it is not left out later
GBD_15_2017_male[is.na(GBD_15_2017_male)] <- 0

# matching GBD file with GPI file

setnames(GPI_2017, "ISO3c","ISO3")
GBD_GPI_male<-inner_join(GBD_15_2017_male, GPI_2017, by="ISO3") 
View(GBD_GPI_male)

# kosovo is not used no GBD data for it; on the other hand,
# 34 countries have info from GBD but not from GPI. They are taken out.
# in the end we get 162 countries, while the GPI report retains 163 because they include Kosovo

# Just checking Calculating Pearson's product-moment correlation
# uncertainty (sdx) versus GPI, males, age 15

cor.test(GBD_GPI_male$sdx, GBD_GPI_male$value, method = "pearson", conf.level = 0.95)

# life expectancy at age 15, males versus GPI
cor.test(GBD_GPI_male$ex, GBD_GPI_male$value, method = "pearson", conf.level = 0.95)



##############################################################################################
# How to deal with this?
# is GPI normally distributed?
# Shapiro-Wilk normality test for GPI
##############################################################################################

shapiro.test(GBD_GPI_male$value)
ggqqplot(GBD_GPI_male$value, ylab = "GPI") # not normal

plot(GBD_GPI_male$value,GBD_GPI_male$sdx )
# Shapiro-Wilk normality test for sdx
shapiro.test(GBD_GPI_male$sdx)
ggqqplot(GBD_GPI_male$sdx, ylab = "sdx") # not normal

# Shapiro-Wilk normality test for ex
shapiro.test(GBD_GPI_male$ex)
ggqqplot(GBD_GPI_male$ex, ylab = "ex")   # not normal

# Fisher’s F test to check equal variance
var.test(GBD_GPI_male$value, GBD_GPI_male$sdx)  # ok, similar variances

# Let´s try other tests, which accomodate deviation from normality
# because all indicators are not normally distributed we must use either spearman 
# or kendall correlation test
# Because the GPI scores are actually connected to rankings, maybe spearman would be a better bet:

# uncertainty (sdx) versus GPI, males, age 15
cor.test(GBD_GPI_male$sdx, GBD_GPI_male$value, method = "spearman", conf.level = 0.95,  exact = FALSE)

# life expectancy at age 15, males versus GPI
cor.test(GBD_GPI_male$ex, GBD_GPI_male$value,  method = "spearman", conf.level = 0.95,  exact = FALSE)

# uncertainty free from homicide at age 15, males versus GPI
cor.test(GBD_GPI_male$sdx_no_h, GBD_GPI_male$value, method = "spearman", conf.level = 0.95,  exact = FALSE)

# life expectancy free from homicide at age 15, males versus GPI
cor.test(GBD_GPI_male$ex_no_h, GBD_GPI_male$value,  method = "spearman", conf.level = 0.95,  exact = FALSE)



GBD_GPI_male_cor_sd<-GBD_GPI_male %>% 
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

data_map_men<-within(data_map_men, continent.y[is.na(continent.y)] <- continent.x[is.na(continent.y)])

data_map_men<-within(data_map_men, continent.y[is.na(continent.y)] <- continent.x[is.na(continent.y)])


#



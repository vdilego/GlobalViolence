#-----------------------------------------------------------------------------
# Some correlation tests to address Alyson and Ridhi´s comments on why        
# standard deviation is meaningful to describe the relationship with violence.
# Is life expectancy also important? What about HDI?
#-----------------------------------------------------------------------------

library(here)
library(countrycode)
library(htmltab)
library(data.table)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(stargazer)

# Read in GBD data estimated in the Results folder- selecting the mid version
GBD_est <- readRDS(here("GlobalViolence","Data","Results","GBD", "GBDmid.rds"))

# Transforming ISO codes into country names for better identification
GBD_est$ISO3<-countrycode(GBD_est$location, origin="country.name", destination="iso3c")
View(GBD_est)

# Read in GPI score values 
GPI_ISO3 <- fread(here("GlobalViolence","Data","Inputs","GPI", "GPI_ISO3.csv"))
View(GPI_ISO3)

# GPI: Selecting only the matching years for the GPI and GBD and the score value
GPI_years<-GPI_ISO3 %>% 
  filter(type=="score"& year %in% c(2008:2017))
summary(GPI_years)

# Checking for NAs and which cases are they
check.na<-GPI_years%>% filter(is.na(value))

# there are missing values for the scores for South Sudan and Palestine for some years,
# as there were no estimates for them due to biding territorial issues. Palestine from 2008 until 2015 and
# South Sudan until 2010. I took  them out for doing the correlation for all years and included them for the last year of analysis

# GBD: Selecting only the matching years for the GPI and GBD and the score value and restricting to ages 10 to 30 
GBD_years<-GBD_est %>% filter(age%in% c(10,15,20,25,30) & year %in% c(2008:2017))
View(GBD_years)

# Azerbaijan, Albania, Tunisia and Uzbekistan have NaN edx values.
# Substitute for 0 so it is not left out later (it does not affect what we do since the NaN values are only for the edaggers)

GBD_years[is.na(GBD_years)] <- 0

# Joining GBD file with GPI file by ISO code and year. Keeping only the matching observations.

setnames(GPI_years, "ISO3c","ISO3")
GBD_GPI<-inner_join(GBD_years, GPI_years, by=c("ISO3","year")) 
View(GBD_GPI)

# -------------------------------------------------------------------------------------------------------------------------------
# Calculating Pearson's product-moment correlation:
# Uncertainty (sdx) versus GPI and remaining life expectancy at ages 10 to 30 versus GPI (this was also in Ridhis´comments) 
# Small comments: Kosovo is not used since there is no GBD data for it; 33 countries have info from GBD but not from GPI.
# They are taken out. In the end we get 162 countries, while the GPI report retains 163 because they include Kosovo. 
# -------------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------------
# Correlation 1. all countries and for all years available that match GBD and GPI (2008-2017), by sex and ages 10-30
# -------------------------------------------------------------------------------------------------------------------------------

# Adding the correlation and p-values of testing to the dataframe for all years, 162 countries

GBD_GPI_cor_all<-GBD_GPI %>% 
  group_by(sex, age, year) %>% 
  mutate(corr_sdx= cor.test(sdx, value, method = "pearson", conf.level = 0.95)$estimate,
         corr_ex= cor.test(ex, value, method = "pearson", conf.level = 0.95)$estimate,
         corr_sdx_test=cor.test(sdx, value, method = "pearson", conf.level = 0.95)$p.value,
         corr_ex_test=cor.test(ex, value, method = "pearson", conf.level = 0.95)$p.value)%>% 
  select(value,sex,age,year,sdx,ex,corr_sdx,corr_ex)



# Plot standard deviation versus GPI, both sexes, all years and countries, ages 10-30

labels_sdx<-GBD_GPI_cor_all %>%                             # labels for graphing
  group_by(sex, age,year, corr_sdx) %>% 
  dplyr::summarise()
labels_sdx$corr_sdx<-sprintf("italic(r) == %.3f", labels_sdx$corr_sdx)

sdx_plot_all<-ggplot(GBD_GPI_cor_all, aes(sdx,value, group=sex, color=sex))+ 
  geom_point(alpha=0.4, size=2.5)+facet_grid(year~age)+  
  geom_smooth(method=lm, se=FALSE)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Standard Deviation",limits=c(8,21))+
  geom_text(x = 10, y = 3.9, aes(label = corr_sdx),size=4, parse=T,data = labels_sdx %>% filter(sex=="Female"),show.legend = F)+
  geom_text(x = 10, y = 3.7, aes(label = corr_sdx), size=4,parse=T,data = labels_sdx %>% filter(sex=="Male"),show.legend = F)+
  scale_color_manual(values=c("brown", "blue"))+
  theme_bw(base_size = 14) +theme(legend.position = "bottom")

pdf(here("GlobalViolence","Maps","plot_corr_sdx_all_years.pdf"), width = 20, height = 24) 
sdx_plot_all
dev.off() 

# Plot remaining life expectancy versus GPI, both sexes, all years and countries.

labels_ex<-GBD_GPI_cor_all %>%                             # labels for graphing
  group_by(sex, age,year, corr_ex) %>% 
  dplyr::summarise()
labels_ex$corr_ex<-sprintf("italic(r) == %.3f", labels_ex$corr_ex)

ex_plot_all<-ggplot(GBD_GPI_cor_all, aes(ex,value,group=sex,color=sex))+ 
  geom_point(alpha=0.4, size=2.5)+facet_grid(year~age)+   
  geom_smooth(method=lm, se=FALSE)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Life expectancy",limits=c(20,80))+
  geom_text(x = 29, y = 3.9, aes(label = corr_ex),size=4, parse=T,data = labels_ex %>% filter(sex=="Female"), show.legend = FALSE)+
  geom_text(x = 29, y = 3.7, aes(label = corr_ex),size=4, parse=T,data = labels_ex %>% filter(sex=="Male"),show.legend = FALSE)+
  scale_color_manual(values=c("brown", "blue"))+
  theme_bw(base_size = 14) +theme(legend.position = "bottom")

# change size accordingly when saving

pdf(here("GlobalViolence","Maps","plot_corr_edx_all_years.pdf"), width = 20, height = 24) 
ex_plot_all
dev.off() 

# ----------------------------------------------------------------------------------------------------------------------------------
# Correlation 2. All countries highlighting only most recent year, for women and men at ages 10-30
# for women at age 10 the Pearson´s correlation is 0.4365859 and at age 15 is 0.4238340
# for men age at 10 it is 0.5157806 and at age 15 0.5023810. They are both significant. 
# for Women, the correlation between life expectancy and GPI is stronger than the standard deviation. For men, it is the opposite,
# especially when considering ages 10 and 15.
# ----------------------------------------------------------------------------------------------------------------------------------

# Selecting latest year

GBD_GPI_cor_2017<-GBD_GPI_cor_all %>% 
  filter(year==2017)

# Plot standard deviation versus GPI, both sexes, year 2017 and countries, ages 10-30

labels_sdx_2017<-GBD_GPI_cor_2017 %>%                             # labels for graphing
  group_by(sex, age,year, corr_sdx) %>% 
  dplyr::summarise()
labels_sdx_2017$corr_sdx<-sprintf("italic(r) == %.3f", labels_sdx_2017$corr_sdx)

sdx_plot_2017<-ggplot(GBD_GPI_cor_2017, aes(sdx,value, group=sex, color=sex))+
  geom_point(size = 3,alpha=0.5)+
  facet_grid(.~age)+  
  geom_smooth(method=lm, se=FALSE, size=1.3)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Standard Deviation",limits=c(8,23))+
  geom_text(x = 10, y = 4, aes(label = corr_sdx),size=7, parse=T,data = labels_sdx_2017 %>% filter(sex=="Female"),show.legend = F)+
  geom_text(x = 10, y = 3.8, aes(label = corr_sdx), size=7,parse=T,data = labels_sdx_2017 %>% filter(sex=="Male"),show.legend = F)+
  scale_color_manual(values=c("brown", "blue"))+
  theme_bw(base_size = 16) +theme(legend.position = "none", axis.title.y = element_blank(),axis.title.x = element_blank())


# Plot remaining life expectancy versus GPI, both sexes, year 2017 and countries, ages 10-30

labels_ex_2017<-GBD_GPI_cor_2017 %>%                             # labels for graphing
  group_by(sex, age,year, corr_ex) %>% 
  dplyr::summarise()
labels_ex_2017$corr_ex<-sprintf("italic(r) == %.3f", labels_ex_2017$corr_ex)

ex_plot_2017<-ggplot(GBD_GPI_cor_2017, aes(ex,value,group=sex,color=sex))+ 
  geom_point(alpha=0.5, size=3)+facet_grid(.~age)+   
  geom_smooth(method=lm, se=FALSE, size=1.3)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Life expectancy",limits=c(20,80))+
  geom_text(x = 29, y = 4, aes(label = corr_ex),size=7, parse=T,data = labels_ex_2017 %>% filter(sex=="Female"), show.legend = FALSE)+
  geom_text(x = 29, y = 3.8, aes(label = corr_ex),size=7, parse=T,data = labels_ex_2017 %>% filter(sex=="Male"),show.legend = FALSE)+
  scale_color_manual(values=c("brown", "blue"))+
  theme_bw(base_size = 16) +theme(legend.position = "bottom",strip.text.x =  element_blank(), axis.title.y = element_blank(),
                                  axis.title.x = element_blank())

# change size accordingly when saving

pdf(here("GlobalViolence","Maps","plot_corr_2017.pdf"), width = 30, height = 15) 

grid.arrange(sdx_plot_2017, ex_plot_2017, ncol=1,left = textGrob("GPI Score\n", rot = 90, vjust = 1,
            gp = gpar(fontsize = 16, fontface = 'bold')), right = textGrob("Life Expectancy                                                               Standard Deviation\n",
                              gp = gpar(fontsize = 16, fontface = 'bold'),rot = 90))

dev.off() 

# ----------------------------------------------------------------------------------------------------------------------------------
# Correlation 3. Disagreggated by Region. Here we group the countries according to the 7 regions defined by the World Bank.
# Including Human Development Index (HDI) to perform analysis.
# ----------------------------------------------------------------------------------------------------------------------------------

# Download Human Development Index at http://hdr.undp.org/en/data#

hdi<- fread(here("GlobalViolence","Maps","hdi.csv"), header = T)
View(hdi)

hdi_iso<- hdi %>% 
  mutate(ISO3= countrycode(Country, "country.name","iso3c"),
         Region=countrycode(Country, "country.name","region")) %>% 
  drop_na() %>% 
  dplyr::select(-1) %>% 
  gather(Year,HDI, 2:30) %>% 
  filter(Year%in%c(2008:2017))

hdi_iso$Year<-as.numeric(hdi_iso$Year)

GBD_GPI_hdi<-left_join(hdi_iso,GBD_GPI, by=c("ISO3", "Year"="year"))
GBD_GPI_hdi$HDI<-as.numeric(GBD_GPI_hdi$HDI)

# Checking for NAs and which cases are they: these are countries for which originally there is no HDI estimate
# I kept them in the dataset since I also used information on World Bank region
check.na_hdi<-GBD_GPI_hdi %>% filter(is.na(HDI))

# I took out the countries for which there is no GPI registered, since they will not contribute to our estimates.
# these are the countries that have info from the GBD, for example.
GBD_GPI_hdi<- GBD_GPI_hdi %>% 
  filter(!is.na(value))

GBD_GPI_hdi$Region<-as.factor(GBD_GPI_hdi$Region)

# relevel to the developed region as comparison group = Europe & Central Asia for regressions below
GBD_GPI_hdi <- within(GBD_GPI_hdi, Region <- relevel(Region, ref = 2)) 


# Overall correlation test between HDI and GPI and between HDI and lifespan inequality.
# these are for all observations
cor.test(GBD_GPI_hdi$HDI,GBD_GPI_hdi$value, use = "complete.obs")
cor.test(GBD_GPI_hdi$HDI,GBD_GPI_hdi$sdx, use = "complete.obs")

# As expected, HDI and GPI are negatively associated (-0.52). The relationship is linear and means that the higher the levels of development
# the lower are the GPI scores, which means lower levels of violence.
# The correlation between HDI and sdx is also significant and with a linear negative association of -0.656.
# As it is expected that HDI and sdx were correlated, it is actually truly remarkable that GPI has such a big correlation value
# Now let´s refine these correlations by year, sex, age and region

# Since there are only 2 observations for North America the tests cannot be performed for this region, so I just added the correlation. 
# Later we see in the regression how it is not significant probably for this very same reason - the variance is too high.

GBD_GPI_hdi_cor<-GBD_GPI_hdi%>% 
  group_by(Region, Year, sex, age) %>% 
  mutate(corr_sdx= cor(sdx, value,  use = "complete.obs"),
         corr_ex= cor(ex, value, use = "complete.obs"), 
         corr_hdi=cor(HDI,value, use = "complete.obs"),
         corr_hdi_sdx=cor(HDI,sdx, use = "complete.obs"))


# In this graph, the correlation between GPI and sdx seems negative for Sub-Saharan Africa, but looking
# into the correlation tests it is not significant, suggesting that the relationship is not linear for this 
# region. 
X11()
library(ggthemes)
library(RColorBrewer)
ggplot(GBD_GPI_hdi_cor %>% filter(Year==2017), aes(sdx,value, group=Region, color=Region))+ 
  geom_point(aes(colour=Region, fill=Region),shape = 21,colour = "black",alpha=0.4, size=4)+ 
  geom_smooth(method="lm", se=F)+
  facet_grid(sex~age)+
  theme(legend.position = "bottom")+ 
  theme_bw()+
  scale_fill_viridis_d()+
  scale_color_viridis_d()


# As a matter of effect, when we plot only the cases for this region, we can see how the variance 
# across countries in this region is so high that there is no clear relationship.

cor_location2<-GBD_GPI_hdi_cor %>%
  filter(Region=="Sub-Saharan Africa") 

ggplot(cor_location2 %>% filter(Year==2017), aes(sdx,value))+ 
  geom_point(aes(colour=Region, fill=Region),shape = 21,colour = "black",alpha=0.4, size=4)+ 
  geom_smooth()+
  facet_grid(sex~age)+
  theme(legend.position = "bottom")+ 
  theme_bw()+scale_fill_viridis_d()+scale_color_viridis_d()


# when looking at HDI, however the relationship is quite linear
ggplot(cor_location2 %>% filter(Year==2017), aes(HDI,value))+ 
  geom_point(aes(colour=Region, fill=Region),shape = 21,colour = "black",alpha=0.4, size=4)+ 
  geom_smooth(method = "lm")+
  facet_grid(sex~age)+
  theme(legend.position = "bottom")+ 
  theme_bw()+scale_fill_viridis_d()+scale_color_viridis_d()

cor_location3<- cor_location2 %>% 
  filter(Year==2017)
cor.test(cor_location3$HDI,cor_location3$value, use="complete.obs")

cor.test(cor_location3$sdx,cor_location3$value, use="complete.obs")


cor.test(GBD_GPI_hdi$sdx,GBD_GPI_hdi$Year, use="complete.obs")
# the correlation of sdx and year is negative, with recent years having lower 
# sdx. However, the strength of the correlation between sdx and GPI becomes stronger
# over time for males aged 10.

# Since all the variables of interest are overall related to GPI in a linear way, we can fit a linear regression model to check what 
# is important to explain the sdx. We do it for all data including year, region and sex as a covariable
# and then we restrict the analysis only for males, age 10, year 2017. 

# Models part 1: all data including year, region and sex

mod1<-lm(sdx ~ value, data=GBD_GPI_hdi)
summary(mod1)

mod2<-lm(sdx ~ HDI, data=GBD_GPI_hdi)
summary(mod2)

mod3<-lm(sdx ~ Region, data=GBD_GPI_hdi)
summary(mod3)

mod4<-lm(sdx ~ value+Region+sex+factor(Year), data=GBD_GPI_hdi)
summary(mod4)


mod5<-lm(sdx ~ value+HDI+Region, data=GBD_GPI_hdi)
summary(mod5)

# Set of model 2: only year 2017 and males aged 10

mod1.1<-lm(sdx ~ value, data=GBD_GPI_hdi %>% 
             filter(Year==2017 & age==10 & sex=="Male"))
summary(mod1.1)

mod2.1<-lm(sdx ~ HDI, data=GBD_GPI_hdi %>% 
           filter(Year==2017 & age==10 & sex=="Male"))
summary(mod2.1)

mod3.1<-lm(sdx ~ Region, data=GBD_GPI_hdi %>% 
             filter(Year==2017 & age==10 & sex=="Male"))
summary(mod3.1)

mod4.1<-lm(sdx ~ value+Region,data=GBD_GPI_hdi %>% 
             filter(Year==2017 & age==10 & sex=="Male"))
summary(mod4.1)

mod5<-lm(sdx ~ value+HDI+Region, data=GBD_GPI_hdi)
summary(mod5)


mena<-GBD_GPI_hdi_m_10 %>%
  filter(Region=="Middle East & North Africa") 

summary(mena$sdx)

count_location<-GBD_GPI_hdi_m_10 %>%
  group_by(Region) %>% 
  dplyr::summarize(n())


anova(mod1,mod2)
anova(mod1,mod3)
anova(mod1,mod4)

anova(mod1,mod2,mod3,mod4)



# the best model is the one that includes both GPI and Region as variables to explain lifespan inequality.



##############################################################################################
# How to deal with this?
# is GPI normally distributed?
# Shapiro-Wilk normality test for GPI
##############################################################################################



# Shapiro-Wilk normality test for sdx
shapiro.test(GBD_GPI$sdx)
ggqqplot(GBD_GPI$sdx, ylab = "sdx") # not normal

# Shapiro-Wilk normality test for ex
shapiro.test(GBD_GPI_male$ex)
ggqqplot(GBD_GPI_male$ex, ylab = "ex")   # not normal

# Fisher’s F test to check equal variance
var.test(GBD_GPI$value, GBD_GPI$sdx)  # ok, similar variances

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



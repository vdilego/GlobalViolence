###############################################################################
# Some correlation tests to address Alyson and Ridhi´s comments on why        #
# standard deviation is more meaningful than life expectancy to describe      #
# the relationship with violence.                                             #
# Vanessa di Lego                                                             #
###############################################################################

library(here)
library(countrycode)
library(htmltab)
library(data.table)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)

GBD_est <- readRDS(here("GlobalViolence","Data","Results","GBD", "GBDmid.rds"))
GBD_est$ISO3<-countrycode(GBD_est$location, origin="country.name", destination="iso3c")
View(GBD_est)

#bringing GPI 
GPI_ISO3 <- fread(here("GlobalViolence","Data","Inputs","GPI", "GPI_ISO3.csv"))
View(GPI_ISO3)

# selecting only the matching years for the GPI and GBD and the score value
GPI_years<-GPI_ISO3 %>% 
  filter(type=="score"& year %in% c(2008:2017))
summary(GPI_years)

check.na<-GPI_years%>% filter(is.na(value))
# there are missing values for the scores for South Sudan and Palestine for some years,
# as there were no estimates for them and territorial issues. Palestine from 2008 until 2015 and
# South Sudan until 2010. I took  them out for doing the correlation for all years and included them for the last year

# ages 10 and 15 only
GBD_years<-GBD_est %>% filter(age%in% c(10,15,20,25,30) & year %in% c(2008:2017))
View(GBD_years)

# Azerbaijan, Albania, Tunisia and Uzbekistan have NaN edx values.
# Substitute for 0 so  it is not left out later (it does not affect what we do since the NaN values are only for the edaggers)

GBD_years[is.na(GBD_years)] <- 0

# joining GBD file with GPI file

names(GPI_years)

setnames(GPI_years, "ISO3c","ISO3")
GBD_GPI<-inner_join(GBD_years, GPI_years, by=c("ISO3","year")) 
View(GBD_GPI)

# -------------------------------------------------------------------------------------------------------------------------------
# Calculating Pearson's product-moment correlation
# uncertainty (sdx) versus GPI and remaining life expectancy at ages 10 and 15 versus GPI (this was also in Ridhis´comments) 
# Small comments: Kosovo is not used no GBD data for it; on the other hand, 34 countries have info from GBD but not from GPI.
# They are taken out. In the end we get 162 countries, while the GPI report retains 163 because they include Kosovo. 
# -------------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------------
# Correlation 1. all countries and for all years available that match GBD and GPI (2008-2017), by sex and ages 10-30
# -------------------------------------------------------------------------------------------------------------------------------

# Adding the correlation and p-values of testing to the dataframe for all years

GBD_GPI_cor_all<-GBD_GPI %>% 
  group_by(sex, age, year) %>% 
  mutate(corr_sdx= cor.test(sdx, value, method = "pearson", conf.level = 0.95)$estimate,
         corr_ex= cor.test(ex, value, method = "pearson", conf.level = 0.95)$estimate,
         corr_sdx_test=cor.test(sdx, value, method = "pearson", conf.level = 0.95)$p.value,
         corr_ex_test=cor.test(ex, value, method = "pearson", conf.level = 0.95)$p.value)%>% 
  select(value,sex,age,year,sdx,ex,corr_sdx,corr_ex, iSO3)


# Plot standard deviation versus GPI, both sexes, all years and countries, ages 10 and 15

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
library(stringr)
pdf(here("GlobalViolence","Maps","plot_corr_2017.pdf"), width = 30, height = 15) 

grid.arrange(sdx_plot_2017, ex_plot_2017, ncol=1,left = textGrob("GPI Score\n", rot = 90, vjust = 1,
            gp = gpar(fontsize = 16, fontface = 'bold')), right = textGrob("Standard Deviation                                                               Life Expectancy\n",
                              gp = gpar(fontsize = 16, fontface = 'bold'),rot = 90))


dev.off() # Close the file

X11()


# ----------------------------------------------------------------------------------------------------------------------------------
# Correlation 3. Disagreggated by Region
# ----------------------------------------------------------------------------------------------------------------------------------

# First finding a grouping region that makes sens

world_class<- fread(here("GlobalViolence","R","country_class.csv"))
class(world_class$ISO3)

# download Human Development Index at http://hdr.undp.org/en/data#

hdi<- fread(here("GlobalViolence","Maps","hdi.csv"), header = T)
View(hdi)
library(countrycode)
library(naniar)
library(magrittr)

hdi_iso<- hdi %>% 
  mutate(ISO3= countrycode(Country, "country.name","iso3c")) %>% 
  drop_na() %>% 
  select(-1) %>% 
  gather(Year,HDI, 2:30) %>% 
  filter(Year%in%c(2017))

hdi_iso$Year<-as.numeric(hdi_iso$Year)
GBD_GPI_hdi<-left_join(hdi_iso,GBD_GPI, by=c("ISO3", "Year"="year"))
GBD_GPI_hdi<-GBD_GPI_hdi %>% 
  drop_na()


# reordering factors
GBD_GPI_region$Economy<- factor(GBD_GPI_region$Economy, 
                                  levels = c("Low income", "Lower middle income", "Upper middle income", "High income"), ordered = T)


GBD_GPI_cor_econ<-GBD_GPI_region %>% 
  group_by(Economy,year, sex, age ) %>% 
  drop_na() %>% 
  mutate(corr_sdx= cor.test(sdx, value, method = "pearson", conf.level = 0.95)$estimate,
         corr_ex= cor.test(ex, value, method = "pearson", conf.level = 0.95)$estimate,
         corr_sdx_test=cor.test(sdx, value, method = "pearson", conf.level = 0.95)$p.value,
         corr_ex_test=cor.test(ex, value, method = "pearson", conf.level = 0.95)$p.value)%>% 
  select(Economy,year, sex, age, value,sdx,ex,corr_sdx,corr_ex)


labels_sex_econ<-GBD_GPI_cor_econ %>%                             # labels for graphing
  group_by(Economy, sex, age,year, corr_sdx) %>% 
  dplyr::summarise()
labels_sex_econ$corr_sdx<-sprintf("italic(r) == %.3f",labels_sex_econ$corr_sdx)

library(cluster)
library(ggfortify)
ggplot(GBD_GPI_cor_econ %>% filter(year==2017), aes(sdx,value,group=Economy,color=Economy))+ 
  geom_jitter(aes(color=Economy),alpha=0.5, size=3)+
  facet_grid(sex~age)+   
  geom_smooth(method=lm, se=FALSE, size=1.3)+
  #scale_y_continuous(name="GPI score",limits=c(1,4))+
  #scale_x_continuous(name="Standard deviation",limits=c(20,80))+
 # geom_text(x = 29, y = 4, aes(label = corr_ex),size=7, parse=T,data = labels_ex_2017 %>% filter(sex=="Female"), show.legend = FALSE)+
#  geom_text(x = 29, y = 3.8, aes(label = corr_ex),size=7, parse=T,data = labels_ex_2017 %>% filter(sex=="Male"),show.legend = FALSE)+
 # scale_color_manual(values=c("brown", "blue"))+
  theme_bw(base_size = 16) + scale_y_log10()+scale_x_log10()+
  stat_ellipse(type = "t")
#+
  #theme(legend.position = "bottom",strip.text.x =  element_blank(), axis.title.y = element_blank(),
   #                               axis.title.x = element_blank())







##############################################################################################
# How to deal with this?
# is GPI normally distributed?
# Shapiro-Wilk normality test for GPI
##############################################################################################

shapiro.test(GBD_GPI$value)
ggqqplot(GBD_GPI$value, ylab = "GPI") # not normal

plot(GBD_GPI_male$value,GBD_GPI_male$sdx )
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



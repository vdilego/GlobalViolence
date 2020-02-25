library("readxl")
library("xlsx")
library(data.table)
library(countrycode)
library(here)
library(tidyverse)

# loading GBD data

load(here("GBDmid.Rdata"))
GPI_ISO3 <- read_csv(here("GPI_ISO3.csv"))
GPI<-GPI_ISO3 %>% filter(type=="score")
GPI_year<-GPI %>% filter(year==2017)
View(GPI_year)

# selecting only the latest years for the GPI
GBD_year<-GBDi %>% filter(year=="2017")

# ages 15 only
GBD_age_year<-GBD_year %>% filter(Age==15)
View(GBD_age_year)

# ages 15 only, males
GBD_age_year_male<-GBD_age_year %>% filter(Sex==1)
View(GBD_age_year_male)

#  Azerbaijan has NaN edx values.
# Substitute for 0 so  it is not left out later
GBD_age_year_male[is.na(GBD_age_year_male)] <- 0

# reading in other gpi dimensions
other_gpi<- fread("other_dimensions_GPI.csv",sep = ";",header=T)

# transforming country names into ISO3 codes
View(other_gpi)

GPI_ISO_other<-other_gpi %>%
  mutate(ISO3c=countrycode(other_gpi$Country, "country.name", "iso3c"))

View(GPI_ISO_other)

# changing columns names to match
colnames(GPI_year)<-c("Country","Year","GPI_domain","Score","ISO3c")
GPI_year<-GPI_year[,-6]

#reorder columns
GPI_ISO_other$Year<-2017
GPI_ISO_other<- GPI_ISO_other[, c(1,5,3,2,4)]

# combining them all
GPI_all<-rbind (GPI_ISO_other,GPI_year)
View(GPI_all)

# combining with GBD data

setnames(GPI_all, "ISO3c","ISO3")
GBD_GPI_male_all<-full_join(GBD_age_year_male, GPI_all, by="ISO3")
View(GBD_GPI_male_all)
GBD_GPI_male_all$GPI_domain<-as.factor(GBD_GPI_male_all$GPI_domain)
levels(GBD_GPI_male_all$GPI_domain) <- c("Conflict" ,      "Militarization", "Safety",  "Total"  )

# correlation of sdx with other domains
GBD_GPI_total_na <-GBD_GPI_male_all[!complete.cases(GBD_GPI_male_all), ]
View(GBD_GPI_total_na)

cor_gpi_total<-GBD_GPI_male_all %>% 
 # group_by(forcats::fct_explicit_na(GPI_domain)) %>%
  summarise(crr = (cor.test(sdx, Score, method = "kendall", conf.level = 0.95)$estimate))

data_map_men_p_ex<-data_map_men %>% 
  group_by(location) %>%
  summarise(pval = (cor.test(ex, value, method = "kendall", conf.level = 0.95)$p.value))

# grphs
ggplot(data_map_men_cor, aes(reorder(continent.y, crr), crr)) +
  geom_col()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

X11()
library(ggplot2)
ggplot(GPI_ISO_other, aes(Country, Score, color=GPI_domain)) +
  geom_point()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))



#-------------------------------------------------------------------------------#
# Looking into GPI´s other dimensions                                          
# Vanessa di Lego                                                             
#------------------------------------------------------------------------------#

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
library(xlsx)
gpi.ind<-read.xlsx(here("GlobalViolence","GPI_scores_ind.xlsx"),sheetIndex = "2017",startRow = 4)
gpi.ind<-gpi.ind[-c(1), ]

# joining data with sdx file
setnames(gpi.ind, "Country", "country")
View(gpi.ind)
gpi.ind$ISO3<-countrycode(gpi.ind$country, origin="country.name", destination="iso3c")

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
# matching GBD file with GPI file

setnames(GPI_2017, "ISO3c","ISO3")
GBD_GPI<-inner_join(GBD_15_2017, GPI_2017, by="ISO3") 
View(GBD_GPI_male)

gpi.ind_sdx<-left_join(GBD_GPI,gpi.ind,by="ISO3")
View(gpi.ind_sdx)

# testing cases where the GPI score is different from the one retrieved from Wiki and the spreadsheet
# JM sent me
gpi.ind_sdx$diff<-gpi.ind_sdx$value-gpi.ind_sdx$Overall.Score
gpi.ind_sdx_diff <- subset(gpi.ind_sdx,gpi.ind_sdx$diff!=0)
View(gpi.ind_sdx_diff) # 49 cases

# long format for summarizing data according to indicator
gpi.ind_sdx_long <- melt(gpi.ind_sdx, id.vars = c(1:20, 22:25))

View(gpi.ind_sdx_long)

gpi_stat<-gpi.ind_sdx_long %>%
  group_by(variable,sex) %>%
  summarize(correlation = cor.test(sdx, value, method = "pearson", alternative="two.sided", conf.level = 0.95)$estimate )  


View(gpi_stat)

levels(gpi_stat$variable)[levels(gpi_stat$variable)=="value"] <- "Total GPI Score"

gpi_stat$assoc<- ifelse(gpi_stat$correlation<0, "black", "blue")

ggplot(gpi_stat, 
       aes(x =variable, 
           y = correlation, fill=assoc)) +
  geom_bar(stat = "identity") +facet_grid(.~sex)+
  theme_bw(base_size = 14)+ theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none")+
  scale_fill_manual(values=c("#023FA5", "#8E063B"))+ labs(x = "GPI indicators")+
  labs(y = "Pearson correlation of sdx and GPI indicators")+ coord_flip()
  
## same for ex

gpi_stat_ex<-gpi.ind_sdx_long %>%
  group_by(variable,sex) %>%
  summarize(correlation = cor.test(ex, value, method = "pearson", alternative="two.sided", conf.level = 0.95)$estimate )  

levels(gpi_stat_ex$variable)[levels(gpi_stat_ex$variable)=="value"] <- "Total GPI Score"

gpi_stat_ex$assoc<- ifelse(gpi_stat_ex$correlation<0, "black", "blue")

ggplot(gpi_stat_ex, 
       aes(x =variable, 
           y = correlation, fill=assoc)) +
  geom_bar(stat = "identity") +facet_grid(.~sex)+
  theme_classic2(base_size = 14)+ theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none")+
  scale_fill_manual(values=c("#023FA5", "#8E063B"))+ labs(x = "GPI indicators")+
  labs(y = "Pearson correlation of ex at age 15 and GPI indicators")+ coord_flip()


library("readxl")
library("xlsx")
world_class<- fread("country_class.csv",sep = ";",header=T)
View(world_class)
class(data_map_men$ISO3)
class(world_class$ISO3)


data_male_class<-left_join(data_map_men, world_class, by="ISO3")
View(data_male_class)
class(data_male_class$Economy)

# reordering factors
data_male_class$Economy <- factor(data_male_class$Economy, 
                    levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))

#because all indicators are not normally distributed we must use either spearman or kendall correlation test
data_map_men_sdx_developed<-data_male_class %>% 
  filter(economy%in% c("1. Developed region: G7","2. Developed region: nonG7")) %>%
  summarise(crr = (cor.test(sdx, value, method = "pearson", conf.level = 0.95)$estimate))

data_map_men_sdx_developing<-data_male_class %>% 
  filter(economy=="6. Developing region") %>%
  summarise(crr = (cor.test(sdx, value, method = "pearson", conf.level = 0.95)$estimate))

data_map_men_sdx_developed.p<-data_male_class %>% 
  filter(economy%in% c("1. Developed region: G7","2. Developed region: nonG7")) %>% 
  summarise(pval = (cor.test(sdx, value, method = "pearson", conf.level = 0.95)$p.value))

data_map_men_sdx_developing.p<-data_male_class %>% 
  filter(economy=="6. Developing region") %>%
  summarise(pval = (cor.test(sdx, value, method = "pearson", conf.level = 0.95)$p.value))

data_map_men_ex_developed<-data_male_class %>% 
  filter(economy%in% c("1. Developed region: G7","2. Developed region: nonG7")) %>%
  summarise(crr = (cor.test(ex, value, method = "pearson", conf.level = 0.95)$estimate))

data_map_men_ex_developing<-data_male_class %>% 
  filter(economy=="6. Developing region") %>%
  summarise(crr = (cor.test(ex, value, method = "pearson", conf.level = 0.95)$estimate))

data_map_men_ex_developed.p<-data_male_class %>% 
  filter(economy%in% c("1. Developed region: G7","2. Developed region: nonG7")) %>% 
  summarise(pval = (cor.test(ex, value, method = "pearson", conf.level = 0.95)$p.value))

data_map_men_ex_developing.p<-data_male_class %>% 
  filter(economy=="6. Developing region") %>%
  summarise(pval = (cor.test(ex, value, method = "pearson", conf.level = 0.95)$p.value))


data_map_men_cor_la_p<-data_male_class %>% 
  filter(Region=="Latin America & Caribbean") %>%
  summarise(pval = (cor.test(ex, value, method = "pearson", conf.level = 0.95)$p.value))

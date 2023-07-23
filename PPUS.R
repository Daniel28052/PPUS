library(haven)
library(tidyLPA)
library(tidyverse)
library(ggplot2)

data <- read_sav('https://github.com/Daniel28052/PPUS/blob/main/Porn%5B22561%5D.sav') %>%
  na.omit()
data1 <- data %>%
  select(33:44) %>%
  sapply(haven::zap_labels) %>%
  as_tibble()


################################################################################
########################### Estimating models ##################################
################################################################################
set.seed(123)
Modelss <- data1%>%
  select("PPUS1","PPUS2","PPUS3","PPUS4","PPUS5","PPUS6","PPUS7","PPUS8","PPUS9",
         "PPUS10","PPUS11","PPUS12")%>%
  single_imputation()%>%
  estimate_profiles(1:6, variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("zero", "zero", "equal", "varying"))%>%
  compare_solutions(statistics = c("AIC","BIC","AWE", "CLC", "KIC"))

Modelss

################################################################################
############### Estimating classes based on selected model #####################
################################################################################
set.seed(132)
class9 <- data1%>%
  select("PPUS1","PPUS2","PPUS3","PPUS4","PPUS5","PPUS6","PPUS7","PPUS8","PPUS9",
         "PPUS10","PPUS11","PPUS12")%>%
  single_imputation()%>%
  estimate_profiles(5, variances ="equal",covariances ="zero")

################################################################################
#################### Obtaining profiles with Z scores ##########################
################################################################################

data %>%
  group_by(Class) %>%
  count(Class)%>%
  mutate(Perc = (n/1149)*100)

data%>%
  select(Class,PPUS1,PPUS2,PPUS3,PPUS4,PPUS5,PPUS6,PPUS7,PPUS8,
         PPUS9,PPUS10,PPUS11,PPUS12)%>%
  group_by(Class)%>%
  summarise(PPUS1=mean(PPUS1),PPUS2=mean(PPUS2),PPUS3=mean(PPUS3),
            PPUS4=mean(PPUS4),PPUS5=mean(PPUS5),PPUS6=mean(PPUS6),
            PPUS7=mean(PPUS7),PPUS8=mean(PPUS8),PPUS9=mean(PPUS9),
            PPUS10=mean(PPUS10),PPUS11=mean(PPUS11),PPUS12=mean(PPUS12),
  )%>%
  na.omit()

LPA_dataz <- data %>%
  select(Class,ZPPUS1,ZPPUS2,ZPPUS3,ZPPUS4,ZPPUS5,ZPPUS6,ZPPUS7,ZPPUS8,
         ZPPUS9,ZPPUS10,ZPPUS11,ZPPUS12)%>%
  group_by(Class)%>%
  summarise(ZPPUS01=mean(ZPPUS1),ZPPUS02=mean(ZPPUS2),ZPPUS03=mean(ZPPUS3),
            ZPPUS04=mean(ZPPUS4),ZPPUS05=mean(ZPPUS5),ZPPUS06=mean(ZPPUS6),
            ZPPUS07=mean(ZPPUS7),ZPPUS08=mean(ZPPUS8),ZPPUS09=mean(ZPPUS9),
            ZPPUS10=mean(ZPPUS10),ZPPUS11=mean(ZPPUS11),ZPPUS12=mean(ZPPUS12),
            )%>%
  na.omit()
LPA_dataz



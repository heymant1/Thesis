library(dplyr)
library(haven)
library(tidyverse)
library(readxl)
library(ggplot2)
library(forcats)

nlfs1 <- read_dta("D:/NLFS/nlfs.dta")



employed1 <- nlfs1 %>% 
  select(c( "b01" , "b02" , "b03" , "b05" ,"b09", "b11" ,"b12","b13" , "b15" ,"b17", "b18" , "b19" , "b20" , 
            "b21" , "b22" , "b23" , "b25" , "b26" , "d02_1digit" ,  "d02_nsco" , "d11a" , "d11b" , 
            "d12_1digit" ,"d12_nsic" , "d13" , "d14", "d17" , "e01a" , "e01b" , "e01c" , "e01d" , "f01a", "f01b" ,
            "f01c" , "f02" , "f03a" , "f03b" , "f03c" , "f03d" , "f03e" , "f04" , "f05" , 
            "f06" ,   "i01a" , "i01b" , "i01c" , "i02" , "i03_1digit", "i04" , "i05" , "i06" , "i07" ,
            "i08" , "i09" , "i10" , "i11" , "i12" ,"i15","i13", "i14", "j01" , "j02" , "j03" , "j04" , "j05" , "j06" , "urbrur753",
            "psu" , "hhld")) 
#wage 


employed1<- employed1 %>%
  filter(f01a == 1|f02 == 1) %>% 
  mutate(hourly_wage = case_when(f01b==1 ~ f01c/8, 
                               f01b==2 ~ f01c/e01a,
                               f01b==3 ~ f01c/(4* e01a))) %>%
  mutate(f04 =if_else(f03a == 2 & f03b == 2 & f03c == 2 & f03d == 2 & f03e==2, 0, f04),
           net_benifit = if_else(is.na(f06), f04, f04-f06),
           net_benifithr = net_benifit/ (52*e01a )) %>% 
  mutate(hourly_wage = case_when(f01b %in% c(4, 5) ~ net_benifithr,
                                TRUE ~ hourly_wage))

          # if_else(is.na(hour_wage) , net_benifithr , net_benifithr + hour_wage))
# importing excel sheet of classifications
ethnicity_class <- read_excel("D:/Thesis/classification/class_III.xlsx" , sheet = "caste_III")
occupation_class <- read_excel( "D:/Thesis/classification/class_III.xlsx" , sheet = "jobs_III")
education_class <- read_excel("D:/Thesis/classification/class_III.xlsx" , sheet = "education_III")
Job_class <- read_excel("D:/Thesis/classification/class_III.xlsx" , sheet = "industry_III")
ind_na <- read_excel("D:/Thesis/classification/class_III.xlsx" , sheet = "indna")
#importing the variables from excel sheet
eduv <- education_class %>% 
  select(c("value" , "education" , "yrs_schooling"))
castv<-ethnicity_class %>% 
  select(c( "value" , "caste_name" , "muluki_grp" ))
occuv<- occupation_class %>% 
  select(c( "value" , "two_digit" , "class_11" , "class_5" )) %>%  
  mutate(value = as.double(value))
jobv<-Job_class %>% 
  select(c("value" , "label" , "job_sector"))
industry_na <-ind_na %>% 
  select(c("value", "ind"))

#merging variables to employed 1

  employed1<- employed1 %>% mutate(d02_nsco = as.double(d02_nsco))

  employed1 <- employed1 %>% 
  left_join(.,eduv , by = c("b22" = "value"))
 
  employed1 <- employed1%>% 
  left_join(.,castv , by = c("b03" = "value"))

  employed1 <-employed1 %>% 
  left_join(., occuv , by = c("d02_nsco" = "value"))

  employed1 <-employed1 %>% 
  left_join(., jobv , by= c("d12_nsic" = "value"))
  
  employed1 <- employed1 %>% 
    left_join(.,industry_na , by= c("d02_nsco" = "value"))
#calculating wage 
wage_calc <- employed1 %>% 
  filter(hourly_wage< 1000) %>% 
  group_by(two_digit) %>% 
  summarise(average_wage = mean(hourly_wage,na.rm= TRUE)) %>% 
  select(c("two_digit", "average_wage"))


#merging wage calc
employed1<- employed1 %>% 
  left_join(., wage_calc , by = c("two_digit" = "two_digit"))

employed1<-employed1 %>% 
  mutate(hourly_wage= if_else(hourly_wage > 1000, average_wage, 
                              if_else(hourly_wage< 13, average_wage, hourly_wage)))
employed1<- employed1 %>% 
  mutate(hourly_wage= if_else(hourly_wage<13 , average_wage,
                              if_else(hourly_wage>500 & class_5 %in% c( "Elementary occupations" ,
       "Plant and machine operators, and assemblers") , average_wage , hourly_wage)))

employed1<- employed1 %>%
  mutate(job_sector = if_else(is.na(d12_nsic), ind, job_sector))

employed1<- employed1 %>% 
  mutate(label = if_else(is.na(label), ind, label))

#education

employed1 <- employed1 %>% 
  mutate(education = case_when(b18 == 2 & b20==2 & b21== 2 ~ "Illiterate" ,
                               b18 == 2 & is.na(b20) & b21==2 ~ "Illiterate",
                               b18 == 1 & b20 == 2 & b21 ==2 ~ "Below primary",
                               b18 == 1 & is.na(b20) & b21==2 ~ "Below primary",
                               TRUE ~ education)) %>% 
## years of schooling
mutate(yrs_schooling = case_when(b18==2 & b20==2 & b21==2 ~ 0,
                                 b18==2 & is.na(b20) & b21==2 ~ 0,
                                 b18==1 & b20==2 & b21==2 ~ 1,
                                 b18==1 & b19==1 & is.na(b20) & b21==2 ~ 1,
                                 b18==1 & b19==2 & is.na(b20) & b21==2 ~ 1,
                                 TRUE ~ yrs_schooling)) %>%
  mutate(experience = b02 - yrs_schooling - 6) %>% 
  mutate(experience= (if_else(experience < 0, 0 , experience))) %>% 
  mutate(experience_square = experience * experience)

employed1<- employed1 %>% 
  mutate(gender = if_else(b01 == 1 , 0 , 1)) %>% 
  mutate(married = if_else(b05 %in% c(1 ,3, 4 , 5) , 0 , 1 )) %>% 
  mutate(vocational_training = if_else(b23 ==1 , 1 , 0),
         vocational_training= case_when(is.na(vocational_training) ~ 0,
                                              TRUE ~ vocational_training)) %>% 
  mutate(job_sector = case_when(d11a %in% c(2,3) | is.na(job_sector) ~ "Arts, entertainment and other service activities" ,TRUE ~ job_sector)) %>% 
  mutate(firm_size = case_when(d17 %in% c(1,2) ~ "small firm",
                               d17 %in% c(3,4) ~ "Medium firm",
                               d17 == 5 ~ "Large firm",
                               d13 %in% c(1,2,5) ~ "Large firm",
                               d14 == 1 ~"Large firm",
                               d11a == 2 ~ "small firm",)) %>% 
  mutate(firm_type = case_when(d11a == 1 & d13 %in% c(1,2) ~ "Government_sector" ,
                                d11a ==1 & d13 %in% c(3,4) ~ "Private_sector" ,
                                d11a ==1 & d13 %in% c(5,6,7) ~ "other" ,
                                d11a == 2~"other",
                                d11a ==3 & d14 %in% c(1,2) ~ "Private_sector" ,
                                d11a == 3 & d14 ==3~ "other"))  %>% 
  
  mutate(i11 = as.double(i11),
         i15 = as.double(i15),
         j04 = as.double(j04)) %>% 
  rowwise() %>% 
  mutate(chores=sum(c(i07,i09, i11, i13, i15), na.rm=TRUE)) %>% 
  mutate(chores1 = sum(c(j02 , j04 , j06), na.rm= TRUE )) %>% 
  mutate(chores_hr = chores/ 30 + chores1/ 7) %>% 
  mutate(urban = if_else(urbrur753 == 2,0,1)) %>%   #urbrur753 is classified non NA
mutate(migration_fr_job = case_when(b09 == 1 & b13 ==2 & b17 %in% c(3,4,5,6,7) ~ 1,
                                    b09 == 2 & b13 == 1 & b12 %in% c(3,4,5,6,7) ~ 1,
                                    b09 == 2 & b13 == 2 & b17 %in% c(3,4,5,6,7) ~ 1,
                                    TRUE ~ 0)) %>%  
  mutate(overtime = if_else(e01a>40,1,0)) 
  
childpop <- nlfs1%>% 
    group_by(psu, hhld) %>% 
    summarise(hh_child = sum(b02 < 12)) 
employed1<-employed1 %>% 
  left_join(., childpop, by = c("psu", "hhld"))
  
 

 NLFS_dat<- employed1 %>% 
    select(c("hourly_wage" ,"education", "yrs_schooling","caste_name", "muluki_grp",         
             "two_digit" , "class_11", "class_5", "label", "job_sector", "average_wage",       
              "experience",       "experience_square" ,  "gender",             
              "married",         "vocational_training", "firm_size",          
              "firm_type"    ,       "chores"     ,         "chores1" ,           
              "chores_hr"    ,       "urban"        ,       "migration_fr_job",
             "hh_child"))

 NLFS_dat<- NLFS_dat %>% 
   mutate(muluki_grp = factor(muluki_grp,
                              levels = c("tagadhari", "matwali",  "pani nachalne"))) %>% 
   mutate(education = factor(education,
                             levels = c("Illiterate", "Below primary", "Primary", "Tenth grade",
                                        "Secondary", "Bachelor", "Masters and above")),
          job_sector = factor(job_sector,
                              levels = c("Agriculture", 
                                         "Mining and quarrying; Electricity, gas and water supply",
                                         "Construction", "Manufacturing", "Market Services",
                                         "Non-market services", 
                                         "Arts, entertainment and other service activities")),
          firm_type = factor(firm_type,
                             levels = c("Government_sector", "Private_sector", 
                                         "other" )),
          firm_size = factor(firm_size,
                           levels = c("small firm", "Medium firm", "Large firm" )),
          class_5 = factor(class_5,
                           levels = c("Elementary occupations", 
                                      "Plant and machine operators, and assemblers",
                                      "Skilled agricultural and trades workers",
                                      "Clerical, service and sales workers",
                                      "Managers, professionals, and technicians")))
#creating dummy variables----
#linking git to r ----

 
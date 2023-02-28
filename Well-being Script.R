## Mental Health Well-being
##Descriptive analysis
save.image ("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Main data/R/Nsg_St.RData")

options(scipen=999)

library(freqtables)
library (tidyverse)

length (Nsg_St$WEMWBS_Score)

Nsg_St %>%
  freq_table (BMS_cat)

Nsg_St %>%
  freq_table (Nsg_St$PHQ_cat)

Nsg_St %>%
  freq_table (Nsg_St$Age_cat)

Nsg_St %>%
  freq_table (Nsg_St$Sex.fct_new)

Nsg_St %>%
  freq_table (Nsg_St$Father_education.fct_new)

Nsg_St %>%
  freq_table (Nsg_St$Mother_education.fct_new)

Nsg_St %>%
  freq_table (Nsg_St$Family_income.fct_new)

Nsg_St %>%
  freq_table (Nsg_St$Family_type.fct)

Nsg_St %>%
  freq_table (Nsg_St$Division.fct_new)

Nsg_St %>%
  freq_table (Nsg_St$Current_residence.fct_new)

Nsg_St %>%
  freq_table (Nsg_St$Course_type.fct_new)

Nsg_St %>%
  freq_table (Nsg_St$Institution_type.fct)

Nsg_St %>%
  freq_table (Nsg_St$Academic_year.fct)

Nsg_St %>%
  freq_table (Nsg_St$Nursing_choice.fct)

Nsg_St %>%
  freq_table (Nsg_St$Have_qualified_teacher.fct)

Nsg_St %>%
  freq_table (Nsg_St$Have_subject_specific_teacher.fct)

Nsg_St %>%
  freq_table (Nsg_St$Have_subject_specific_lab)

############# un-adjusted analysis by t-test #############
t.test (Nsg_St$WEMWBS_Score ~Nsg_St$BMS_cat) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$BMS_cat), FUN=sd)

#Socio-demographic information
t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Age_cat) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Age_cat), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Sex.fct_new) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Sex.fct_new), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Father_education.fct_new) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Father_education.fct_new), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Mother_education.fct_new) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Mother_education.fct_new), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Family_type.fct) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Family_type.fct), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Current_residence.fct_new) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Current_residence.fct_new), FUN=sd)

#Academic information
t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Course_type.fct_new) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Course_type.fct_new), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Institution_type.fct) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Institution_type.fct), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Nursing_choice.fct) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Nursing_choice.fct), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Have_qualified_teacher.fct) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Have_qualified_teacher.fct), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Have_subject_specific_teacher.fct) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Have_subject_specific_teacher.fct), FUN=sd)

t.test (Nsg_St$WEMWBS_Score ~Nsg_St$Have_subject_specific_lab) 
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Have_subject_specific_lab), FUN=sd)

############# un-adjusted analysis by anova-test #############
aov_PHQ_cat=aov(WEMWBS_Score ~ PHQ_cat, data = Nsg_St) 
summary (aov_PHQ_cat)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$PHQ_cat), FUN=mean)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$PHQ_cat), FUN=sd)

##
aov_Family_income=aov(WEMWBS_Score ~ Family_income.fct_new, data = Nsg_St) 
summary (aov_Family_income)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Family_income.fct_new), FUN=mean)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Family_income.fct_new), FUN=sd)

aov_Division=aov(WEMWBS_Score ~ Division.fct_new, data = Nsg_St) 
summary (aov_Division)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Division.fct_new), FUN=mean)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Division.fct_new), FUN=sd)

aov_Current_residence=aov(WEMWBS_Score ~ Current_residence.fct_new, data = Nsg_St) 
summary (aov_Current_residence)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Current_residence.fct_new), FUN=mean)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Current_residence.fct_new), FUN=sd)

##
aov_Academic_year=aov(WEMWBS_Score ~ Academic_year.fct, data = Nsg_St) 
summary (aov_Academic_year)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Academic_year.fct), FUN=mean)
aggregate(Nsg_St$WEMWBS_Score, list(Nsg_St$Academic_year.fct), FUN=sd)


#### Adjusted model ########
### Model 1

WEMWBS_linear_model.1=lm (Nsg_St$WEMWBS_Score~
                     Nsg_St$BMS_cat+
                     Nsg_St$PHQ_cat) 
WEMWBS_linear_model.1
summary (WEMWBS_linear_model.1)
confint(WEMWBS_linear_model.1)

###Model 2

WEMWBS_linear_model.2=lm (Nsg_St$WEMWBS_Score~
                            Nsg_St$BMS_cat+
                            Nsg_St$PHQ_cat+
                            Nsg_St$Age_cat+
                            Nsg_St$Sex.fct_new+
                            
                            Nsg_St$Father_education.fct_new+
                            Nsg_St$Mother_education.fct_new+
                            Nsg_St$Family_income.fct_new+
                            Nsg_St$Family_type.fct) 
WEMWBS_linear_model.2
summary (WEMWBS_linear_model.2)
confint(WEMWBS_linear_model.2)

###Model 3

WEMWBS_linear_model.3=lm (Nsg_St$WEMWBS_Score~
                            Nsg_St$BMS_cat+
                            Nsg_St$PHQ_cat+
                            Nsg_St$Age_cat+
                            Nsg_St$Sex.fct_new+
                            
                            Nsg_St$Father_education.fct_new+
                            Nsg_St$Mother_education.fct_new+
                            Nsg_St$Family_income.fct_new+
                            Nsg_St$Family_type.fct+
                            Nsg_St$Course_type.fct_new+
                            Nsg_St$Institution_type.fct+
                            Nsg_St$Academic_year.fct+
                            Nsg_St$Nursing_choice.fct+
                            Nsg_St$Have_qualified_teacher.fct+
                            Nsg_St$Have_subject_specific_teacher.fct+
                            Nsg_St$Have_subject_specific_lab.fct) 
WEMWBS_linear_model.3
summary (WEMWBS_linear_model.3)
confint(WEMWBS_linear_model.3) 

##Reference change 
Nsg_St$Have_subject_specific_lab.fct <- relevel(Nsg_St$Have_subject_specific_lab.fct, ref = "Yes") 
Nsg_St$Have_subject_specific_lab.fct= as.factor(Nsg_St$Have_subject_specific_lab)

##Figures

#Forest plot
library (tidyverse)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

Forest_WEMWBS = plot_model(WEMWBS_linear_model.3,
                            vline.color = "violet",
                            show.values = F,
                            value.offset = 0.4,
                            cex= 0.1,
                            axis.labels = c("Have subject specific labs= No",
                                            "Have subject specific teachers = No",
                                            "Have qualified teachers = No",
                                            "Own choice of being nurse = No",
                                            "Academic year = Third year",
                                            "Academic year = Second year",
                                            "Academic year = First year",
                                            "Type of institution = Private",
                                            "Type of course = Diploma",
                                            "Type of family = Nuclear",
                                            "Family income = 15k-20k BDT",
                                            "Family income = >20k BDT",
                                            "Mother's education = Non-graduate",
                                            "Father's education = Non-graduate",
                                           
                                            "Sex = Female",
                                            "Age = >20 years", 
                                            "Depression = Severe",
                                            "Depression = Moderately severe",
                                            "Depression = Moderate",
                                            "Depression = Mild",
                                            "Burnout = Yes"),
                            title = "")#+
  theme_minimal()


Forest_WEMWBS
##
violin_BMS
library (Hmisc)

violin_BMS=ggplot (Nsg_St, 
        aes (BMS_cat.fig, WEMWBS_Score, 
             fill=BMS_cat.fig))+
  geom_violin ()+
  labs (x= "Burnout", y= "Well-being", fill= "Burnout")
  
violin_BMS

violin_BMS_last= violin_BMS + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange")
violin_BMS_last

#
violin_PHQ=ggplot (Nsg_St, 
        aes (PHQ_cat, WEMWBS_Score, 
             fill=PHQ_cat))+
  geom_violin ()+
  labs (x= "Depression", y= "Well-being", fill= "Depression")

violin_PHQ

violin_PHQ_last= violin_PHQ + stat_summary(fun.data=mean_sdl, mult=1, 
                                           geom="pointrange")
violin_PHQ_last

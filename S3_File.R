# LIBRARIES AND DATA PREP  -----
# Read libraries
library("tidyverse")
library("ggpubr")
library("rstatix")
library("remotes")
library("bluegrafir")
library("ggstance")
library("jtools")
library("rlang")
library("likert")
library("GPArotation")
library("lavaan")
library("MPsychoR")
library("corrplot")
library("magrittr")
library("tidyr")
library("plyr")
library("dplyr")
library("forcats")
library("psych")
library("Rfast")
library("reshape2")
library("ggplot2")
library("FactoMineR")
library("factoextra")
library("MASS")
library("nnet")
library("car")
library("dplyr")
library("farver")
library("DescTools")
library("epitools")
library("ggplot2")
library("oddsratio")
library("data.table")
library("PropCIs")
library("stargazer")
library("DataCombine")
library("graphics")
library("gplots")
library("vcd")
library("polycor")
library("semTable")
library("sjPlot")
library("sjmisc")
library("parameters")
library("tidySEM")
library("mediation")

# Data original = the raw df with headers, strings as factors and missing values formatted as ^

data_original <- read.csv("r_adjusted_dat.csv",
                          header = TRUE, na.strings = "^", stringsAsFactors = TRUE)

library(dplyr)
library(forcats)

 dat1 <- data_original

#Creating new variables (Tidying the raw data - collapsing, re-ordering and re-coding messy variables)

# Create a 5 point ordered variable - frequency of weight monitoring (with 1-5 as factors) 
dat1$MON1 <- fct_collapse(dat1$wgt_monitor,
                          "1" = c("Never", "Annually"),
                          "2" = "Occasionally", 
                          "3" = "Monthly",
                          "4" = "Weekly", 
                          "5" = "Daily")

#convert to ordered factor
dat1$MON1 <- ordered(dat1$MON1, levels = c("1", "2", "3", "4", "5"))


# Create a binary variable for monitoring weight - never,annually or occasionally = 0 and everything else =1

dat1$MON1.2 <- fct_collapse(dat1$MON1, 
                            "0" = c("1", "2"),
                            "1" = c("3", "4", "5")
) 


# Create a 5 point ordered variable - frequency of body condition monitoring (with 1-5 as factors) 
dat1$MON2 <- ordered (fct_collapse(dat1$bcs_monitor, 
                                   "1" = c("Never","Annually"),
                                   "2" = "Occasionally", 
                                   "3" = "Monthly",
                                   "4" = "Weekly", 
                                   "5" = "Daily"))

# convert to ordered factor
dat1$MON2 <- ordered(dat1$MON2, levels = c("1", "2", "3", "4", "5"))


# Create a binary variable for monitoring body condition - never, annually or occasionally = 0 and everything else =1

dat1$MON2.2 <- fct_collapse(dat1$MON2, 
                            "0" = c("1","2"),
                            "1" = c("3", "4", "5")
) 


# Create new variable - method of weight monitoring with 1 = a measurement based method
# and 0 = a non-measurement based method or "other"

# note- owners selected their primary method but where they selected "other" they described their own practice. If owners did not select one of the options available which indicated a 
# standardised practice- they were included in the "0" non-standardised category. Despite some of these owners stating methods which appear to have included some form of standardised practice. 
# To avoid subjective categorisation, anything repsonse provided in the "other" category was consiedered non-standardised. This is applicable for both the weight and BCS variables 

dat1$MON3.1 <- fct_collapse(dat1$wgt_how, 
                            "1" = c("Weighbridge", "Tape measure", "Estimation through calculation"),
                            "0" = "Visual assessment", "Other")
# Create new variable - method of body condition monitoring with 1 = a scale / measurement based method
# and 0 = a non-measurement based method or "other"


dat1$MON4.1 <- fct_collapse(dat1$bcs_how, 
                            "1" = c("Body condition scoring using a 1-5 scale system", 
                                    "Body condition scoring using a 1-9 scale system",
                                    "Measurement of heart-girth, belly-girth and neck",
                                    "Taking measurements and using an online body condition calculator"), 
                            "0" = c("Estimation through visual assessment and feeling the horse's body", 
                                    "Estimation through visual assessment only","Other") )



# Create new binary variable - had horse BCS assessed by professional
dat1$MON6 <- ifelse(dat1$bcs_by_prof=="Yes", 1, 0) # numeric 









# Create new binary variable - aware of pf analysis 
dat1$FOR1 <- ifelse(dat1$pres_forage_analysis_aware=="Yes",1,0) # numeric 








# Create new binary variable - had pf analysed 
dat1$FOR2<- fct_collapse(dat1$pres_forage_analysis,
                         "0" = c("No","Don't know"),
                         "1" ="Yes")





# Create a new binary response variable, feed soaked hay 
summary(dat1$pres_forage_feed)
dat1$soakhay <- fct_collapse(dat1$pres_forage_feed, 
                             "1" = "Soaked hay", 
                             "0" = c("Forage substitute (E.g. short chop fibre )", "Haylage", 
                                     "None- my horse does not receive additional forage", 
                                     "Steamed hay", "Hay", "Mixture of hay and haylage") 
)



# Create new nominal factor - most important aspect of exercise 
# Create new nominal factor - most important aspect of exercise 
# Additional textual analysis was applied to aid categorisation of owners answers where they selected "Other" in response to this question, but their response fell clearly into a relevant category for analysis 
dat1$EXER1 <- fct_collapse(dat1$exercise_imp, 
                           "Weight management" = "Managing weight" ,  #managing weight 
                           "Personal enjoyment" = "Enjoyment / hobby",#hobby 
                           "Relieve horses' boredom" = "Relieving boredom / providing stimulation",   #horses' enrichment  
                           "Horses' performance" = c("Training & development","Maintaining fitness for their discipline",    # training purposes
                                                     "In hand showing ", "Keeping them fit to hunt and endurance ride",
                                                     "Muscle building",
                                                     "To keep their training for RDA ",
                                                     "To keep them healthy and fit for riding centre customers ", 
                                                     "Maintaining health and suppleness in horse..",
                                                     "Preparation for training, maintaining finiteness and weight control"),
                           "Not applicable" = c("Not applicable retired Shetland ponies",
                                                "Not applicable",
                                                "My horses aren\x92t ridden or in work", 
                                                "My ponies are used for breeding so not exercised formally", 
                                                "Na one retired other still growing ",  
                                                "None of these are in any work, all are companions ",
                                                "not exercised", "Our ponies are breeding stock & not ridden",
                                                "Retired", 
                                                "We don't. They exercise as a herd on their nature reserve",
                                                "My ponies are retired",
                                                "Our ponies are breeding stock & not ridden", 
                                                "not exercised",
                                                "None of these are in any work, all are companions ",
                                                "Na one retired other still growing ","Don't exercise",
                                                "My horses are elderly retired and exercise in 10 acres", 
                                                "My ponies are used for breeding so not exercised formally", 
                                                "My ponies are retired", 
                                                "Don't exercise"
                           ),
                           "Other" = c("Weight, hobby and fitness",
                                       "Reduce swelling and ease arthritis ",
                                       "My horses are elderly retired and exercise in 10 acres",
                                       "Mixture of above",
                                       "Managing general health including arthritis and PPID. ",
                                       "Health, mental and physical wellbeing and bonding", 
                                       "General health - managing weight and also encouraging healthy feet, tendons, ligaments, joint health....",
                                       "Could be all or any of the above - depending on individual horse or pony ", 
                                       "Combination of weight management and me tal stimulation",
                                       "Combination of the above", 
                                       "Boredom relief and weight management also significant", 
                                       "Also maintaining fitness for their discipline, managing weight and enjoyment",
                                       "All the above. No one reason is important over others. ", 
                                       "All of these.  Tomorrow different than today",
                                       "All of the above. ",
                                       "All of the above! Fun!",
                                       "All of the above in equal measure", 
                                       "All of the above ", 
                                       "All of the above", 
                                       "All of above ", 
                                       "All ", 
                                       "All", 
                                       "The fitter they\x92re kept the less things go wrong ",
                                       "A combination of the above", 
                                       "All of the above. All of my horses enjoy getting ridden and exploring new places and get very stressed and unhappy when not ridden. One does not get ridden except the odd bareback wander because he just starts dripping with sweat from fear and anxiety under 5 minutes without doing anything bad and so I don\x92t think it is fair to ride him however he is provided with several enrichment activities daily instead. One of the others has to be excersized or her weight is uncontrollable. It depends on the individual horses. ",
                                       "A combination of all of above", 
                                       "General health - managing weight and also encouraging healthy feet, tendons, ligaments, joint health....",
                                       "Health, mental and physical wellbeing and bonding",
                                       "Maintaining health and suppleness in horse..", 
                                       "Managing general health including arthritis and PPID. ", 
                                       "Reduce swelling and ease arthritis ",
                                       "The fitter they\x92re kept the less things go wrong ", 
                                       "A combination of all of above",
                                       "A combination of the above",
                                       "All", "All ", "All of above ", "All of the above", "All of the above ", 
                                       "All of the above in equal measure", "All of the above! Fun!", 
                                       "All of the above. ", 
                                       "All of these.  Tomorrow different than today",
                                       "All the above. No one reason is important over others. ",
                                       "Also maintaining fitness for their discipline, managing weight and enjoyment",
                                       "Boredom relief and weight management also significant", 
                                       "Combination of the above", 
                                       "Combination of weight management and me tal stimulation",  
                                       "Could be all or any of the above - depending on individual horse or pony ","Mixture of above",
                                       "Preparation for training, maintaining finiteness and weight control",
                                       "Weight, hobby and fitness"
                                       
                           )
                           
)

# create binary variable of 1 = weight management most important outcome vs 0 = other priority
levels(dat1$EXER1)

dat1$EXER1 <- fct_collapse(dat1$EXER1,
                           "1" = "Weight management" , 
                           "0" = c("Other", "Not applicable", "Personal enjoyment", "Horses' performance", "Relieve horses' boredom")
) 













# Create new variable - encourage weight loss across season- AUTUMN
dat1$MET1 <- fct_collapse(dat1$wgt_autumn,
                          "0" = c("Aim to encourage weight gain","Aim to maintain weight"),
                          "1" = "Aim to encourage weight loss") 








# Create new variable - encourage weight loss across season- SUMMER
dat1$MET2 <- fct_collapse(dat1$wgt_summer,
                          "0" = c("Aim to encourage weight gain","Aim to maintain weight"),
                          "1" = "Aim to encourage weight loss") 









# Create new variable - encourage weight loss across season - WINTER

dat1$MET3 <- fct_collapse(dat1$wgt_winter,
                          "0" = c("Aim to encourage weight gain","Aim to maintain weight"),
                          "1" = "Aim to encourage weight loss") 










# Create new variable - encourage weight loss across season- SPRING
dat1$MET4 <- fct_collapse(dat1$wgt_spring,
                          "0" = c("Aim to encourage weight gain","Aim to maintain weight"),
                          "1" = "Aim to encourage weight loss") 








# Create new variable - INVOLVED IN INDUSTRY 
dat1$EXP1<-ifelse(dat1$indus=="Yes",1,0)







# Create new variable - YEARS CARING FOR HORSES 
dat1$EXP2 <- ordered (fct_collapse(dat1$yrs, 
                                   "0" = c("01-Mar", "04-Sep","Oct-19"), 
                                   "1" = c( "20-29", "?30"))) 



dat1$EXP2 <- ordered(dat1$EXP2, levels = c("0", "1"))











# Create new variable - AGE 
dat1$EXP3 <- ordered (fct_collapse(dat1$age, 
                                   "0"= c("Under 18","18-25","26-40"),
                                   "1" = c("41-60","Over 60") ))

dat1$EXP3 <- ordered(dat1$EXP3, levels = c("0", "1"))









# Create new variable - qualifications
dat1$EXP5<-ifelse(dat1$qual=="Yes",1,0)







# Create new variable - ORGANISATIONAL MEMBERSHIP
dat1$EXP6 <- ifelse(dat1$org=="Yes",1,0)







# Create new variableS - CONFIDENCE RECOGNISING OBESITY ASSOC DISEASE as an ordered factor 
# with 5 levels (ordinal)

dat1$OB1 <- ordered ( as.factor(dat1$conf_rec_obesity))
# create ob2
dat1$OB2 <- ordered( as.factor(dat1$conf_rec_lam))
# create ob3
dat1$OB3 <- ordered( as.factor(dat1$conf_rec_ems))
# create ob4
dat1$OB4 <- ordered( as.factor(dat1$conf_rec_ppid))
# create ob5
dat1$OB5 <- ordered( as.factor(dat1$conf_rec_colic))
# create ob6
dat1$OB6 <- ordered( as.factor(dat1$conf_rec_loc))












# Creating 5 point variable in confidence in management of native ponies 
# (collapsing a mistake from the original survey coding where there were two
# wordings used between not confident at all and neutral)





# Confidence management 
dat1$coman1 <- fct_collapse(dat1$conf_pasture,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor 
dat1$coman1 <- factor (dat1$coman1,ordered = TRUE, levels = c("1","2", "3", "4", "5"))




# Confidence in herd management 
dat1$coman2 <- fct_collapse(dat1$conf_herdman,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
# Order factor 
dat1$coman2 <- factor (dat1$coman2,ordered = TRUE, levels = c("1","2", "3", "4", "5"))





#Confidence in making decisions on turnout routine 
dat1$coman3 <- fct_collapse(dat1$conf_turnoutroutine,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman3 <- factor (dat1$coman3,ordered = TRUE, levels = c("1","2", "3", "4", "5"))






# Confidence in deciding on the type of preserved forage to feed the horse
dat1$coman4 <- fct_collapse(dat1$conf_typepf,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman4 <- factor (dat1$coman4,ordered = TRUE, levels = c("1","2", "3", "4", "5"))







#Confidence in deciding on the amount of preserved forage to feed 
dat1$coman5 <- fct_collapse(dat1$conf_amountpf,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor 
dat1$coman5 <- factor (dat1$coman5,ordered = TRUE, levels = c("1","2", "3", "4", "5"))







# Confidence in feeding the appropriate type of supplemtary feeding 
dat1$coman6 <- fct_collapse(dat1$conf_typef,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman6 <- factor (dat1$coman6,ordered = TRUE, levels = c("1","2", "3", "4", "5"))








# Confidence deciding on the appropriate amount of supplementary feeding 
dat1$coman7 <- fct_collapse(dat1$conf_amountf,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor 
dat1$coman7 <- factor (dat1$coman7,ordered = TRUE, levels = c("1","2", "3", "4", "5"))








# Confidence making decisions on rugging, clipping, stabling etc
dat1$coman8 <- fct_collapse(dat1$conf_rug,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman8 <- factor (dat1$coman8,ordered = TRUE, levels = c("1","2", "3", "4", "5"))









dat1$feedlam <-as.factor(dat1$pres_forage_lam)
dat1$feedems <-as.factor(dat1$pres_forage_ems)
dat1$feedweight <-as.factor(dat1$pres_forage_weight)



dat1<- dat1 %>% 
  mutate(
    met_feed = case_when(feedlam == "Yes"~ 'Yes',
                         feedems == "Yes"~ 'Yes',
                         feedweight == "Yes"~ 'Yes',
                         TRUE ~ 'No')
  ) 



names(dat1)
# Create a new subset with only the re-codedvariables included in the analysis
# Subset the new variables 
dat2 <- subset(dat1, select = c("met_feed", "soakhay", "MON1.2", "MON2.2","MON4.1", "FOR1", "FOR2",
                                "EXER1", "MET1", "MET2","MET3", "MET4", "EXP1", "EXP2", "EXP3", "EXP5", "EXP6",
                                "coman1", "coman2", "coman3", "coman4", "coman5", "coman6", "coman7","coman8",
                                "OB1", "OB2", "OB3", "OB4", "OB5", "OB6")) 



# Check the structure 
str(dat2)



# There are a variety of ordered variables,factors with 2 levels , and binary numerical variables 
# For some analysis, the factors with 2 levels are required to be numerical 
# Create two versions of the subsetted dataframe- one factors only and one numerical


#Factor version dat2



#Recode for1 
head(data_original$pres_forage_analysis_aware,10)
head(dat1$FOR1,10) # check original coding 
head(dat2$FOR1,10)# check recoding ( 1 - YES, 0 = No)
dat2$FOR1 <- as.factor(dat2$FOR1)
head(dat2$FOR1,10)

#Recode exp1 
head(data_original$indus,12)
head(dat1$EXP1,12) # check original coding 
head(dat2$EXP1,12)# check recoding ( 1 - YES, 0 = No)
dat2$EXP1 <- as.factor(dat2$EXP1)
head(dat2$EXP1,10)


#Recode EXP6
head(data_original$qual,12)
head(dat1$EXP5,12) # check original coding 
head(dat2$EXP5,12)# check recoding ( 1 - YES, 0 = No)
dat2$EXP5 <- as.factor(dat2$EXP5)
head(dat2$EXP5,10)


#Recode EXP6
head(data_original$org,12)
head(dat1$EXP6,12) # check original coding 
head(dat2$EXP6,12)# check recoding ( 1 - YES, 0 = No)
dat2$EXP6 <- as.factor(dat2$EXP6)
head(dat2$EXP6,10)

# Recode met feed 
dat2$met_feed <- as.factor(dat2$met_feed)
levels(dat2$met_feed)
levels(dat2$met_feed) <- c("0","1")


#Numerical version dat2

#Create a vector of the subset variable names 
names <- c("met_feed", "soakhay", "MON1.2", "MON2.2","MON4.1", "FOR1", "FOR2",
           "EXER1", "MET1", "MET2","MET3", "MET4", "EXP1", "EXP2", "EXP3", "EXP5", "EXP6",
           "coman1", "coman2", "coman3", "coman4", "coman5", "coman6", "coman7","coman8",
           "OB1", "OB2", "OB3", "OB4", "OB5", "OB6")



#Create a new variable 
dat2_character <- dat2
names(dat2_character)
dat2_character[names] <- sapply(dat2_character[names],as.character) #convert the factor levels to characters
dat2_numeric <- dat2_character
dat2_numeric[names] <- sapply(dat2_character[names],as.numeric)# convert the characters to numbers




semdat<-subset(dat2_numeric, select = c("met_feed", "soakhay", "MON1.2", "MON2.2","MON4.1", "FOR1", "FOR2",
                                        "EXER1", "MET1", "MET2","MET3", "MET4", "EXP1", "EXP2", "EXP3", "EXP5", "EXP6",
                                        "coman1", "coman2", "coman3", "coman4", "coman5", "coman6", "coman7","coman8",
                                        "OB1", "OB2", "OB3", "OB4", "OB5", "OB6"))

testdat  <- semdat

# The outcome variables 
testdat$MON2.2 <- as.factor(testdat$MON2.2)
testdat$MON4.1 <- as.factor(testdat$MON4.1)
testdat$MET1 <- as.factor(testdat$MET1)
testdat$MET3 <- as.factor(testdat$MET3)
testdat$FOR2 <- as.factor(testdat$FOR2)
testdat$soakhay <- as.factor(testdat$soakhay)
testdat$EXER1 <- as.factor(testdat$EXER1)

# Explanatory variables 
testdat$EXP1 <- as.factor(testdat$EXP1)
testdat$EXP2 <- as.factor(testdat$EXP2)
testdat$EXP3 <- as.factor(testdat$EXP3)
testdat$EXP5 <- as.factor(testdat$EXP5)
testdat$EXP6 <- as.factor(testdat$EXP6)


testdat$met_feed <- as.factor(testdat$met_feed)



testdat2 <- na.omit(testdat)






#### Likert scale version of confidence testing ########
# Libraries, data prep, descriptive stats ------
library(ltm)
library("lmboot") #bootstrap cis
library(e1071) # testing for skewness and kurtosis 
library(gmodels) # cross tabs
names(testdat2)
management_5point <- subset(testdat2, select=c(18:25))
summary(management_5point)
disease_5point <- subset(testdat2, select=c("OB1", "OB2", "OB3", "OB4", "OB5", "OB6"))
summary(disease_5point)

cronbach.alpha(management_5point, CI=TRUE)
cronbach.alpha(disease_5point, CI=TRUE)



management_5point_sum <- management_5point %>% 
  dplyr::mutate(conf_man = rowSums(across(coman1:coman8)))

disease_5point_sum <- disease_5point %>% 
  dplyr::mutate(conf_dis = rowSums(across(OB1:OB6)))





testdat3 <- cbind(testdat2, management_5point_sum$conf_man, disease_5point_sum$conf_dis)

str(testdat3)

testdat3$conf_man <- testdat3$`management_5point_sum$conf_man`
testdat3$conf_dis <- testdat3$`disease_5point_sum$conf_dis`

skewness(testdat3$conf_man, type=2)
kurtosis(testdat3$conf_man, type=2)
skewness(testdat3$conf_dis, type=2)
kurtosis(testdat3$conf_dis, type=2)

summary(testdat3$conf_dis)
# 2-Way Cross Tabulation


CrossTable(testdat3$EXP2, testdat3$MON2.2)
# There is a fairly low number of owners in the "less frequently than occasionally" grouping
# with 23 and 22 in the two met feed categories. ** 
CrossTable(testdat3$EXP2, testdat3$MON4.1)

CrossTable(testdat3$EXP2, testdat3$MET3)
CrossTable(testdat3$EXP2, testdat3$MET1)

CrossTable(testdat3$EXP2, testdat3$FOR2) # might be too low (count 26 in one group) , FOR1 would be a potential alternative 
CrossTable(testdat3$EXP2, testdat3$soakhay) # might be too low (count 27 in one group)

CrossTable(testdat3$EXP2, testdat3$EXER1) # might be too low (count 25 in one group)


M1 <- glm(EXP2 ~ met_feed, data=testdat3, family=binomial ("probit"))
summary(M1)
M2 <- glm(EXP6 ~ met_feed, data=testdat3, family=binomial ("probit"))
summary(M2)
M3 <- glm(EXP2 ~ EXP6, data=testdat3, family=binomial ("probit"))
summary(M1)
M4 <- glm(MON4.1 ~ MON2.2, data=testdat3, family=binomial ("probit"))
summary(M4)
confint(M4)
M6<- glm(EXP1 ~ EXP2, data=testdat3, family=binomial ("probit"))
summary(M6)
M7<- glm(EXP1 ~ EXP3, data=testdat3, family=binomial ("probit"))
summary(M7)
M8<- glm(EXP1 ~ EXP5, data=testdat3, family=binomial ("probit"))
summary(M8)
confint(M8)
M9<- glm(EXP1 ~ EXP6, data=testdat3, family=binomial ("probit"))
summary(M9)
confint(M9)
M10<- glm(EXP2 ~ EXP3, data=testdat3, family=binomial ("probit"))
summary(M10)
confint(M10)
M11<- glm(EXP2 ~ EXP5, data=testdat3, family=binomial ("probit"))
summary(M11)
M12<- glm(EXP2 ~ EXP6, data=testdat3, family=binomial ("probit"))
summary(M12)
confint(M12)
M13<- glm(EXP3 ~ EXP6, data=testdat3, family=binomial ("probit"))
summary(M13)


# 1.1 Monitoring body condition freq EXP2  ----
# Direct effect MON2.2 EXP2  -----
out.fit1TOTAL <- glm(MON2.2 ~ EXP2,
                     data = testdat3, family = binomial("probit"))

summary(out.fit1TOTAL)
confint(out.fit1TOTAL)



# conf_dis _ EXP2 (with bootstrap cis) ----- 
med.fit1.1 <- lm(conf_dis ~ EXP2, data = testdat3)
summary(med.fit1.1)
confint(med.fit1.1)

med.fit1_bay <- bayesian.boot(conf_dis ~ EXP2, B = 5000, seed = 101, data = testdat3)
med.fit1_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit1_bay$bootEstParam[,2], probs=c(.025, .975))


# Combined outcome model with confidence and exp2-----
out.fit1.1 <- glm(MON2.2 ~ EXP2 + conf_dis,
                  data = testdat3, family = binomial("probit"))
summary(out.fit1.1)
confint(out.fit1.1)
# There is no direct relationship, but according to Bollen 1989, and Hayes 2018- we can still test for mediation as confidence has a signficiant influence 

# The mediation model -----
set.seed(2023)
med.out1<- mediate(med.fit1.1, out.fit1.1, treat = "EXP2", mediator = "conf_dis",
                   sims = 5000,boot=T, boot.ci.type="bca")

summary(med.out1) 



################################

# 1.2 Monitoring body condition freq metfeed
# Direct effect MON2.2 met_feed-----
out.fit2TOT <- glm(MON2.2 ~ met_feed,
                   data = testdat3, family = binomial("probit"))
summary(out.fit2TOT)
confint(out.fit2TOT)

# Mediation model conf-dis and met_feed -----
med.fit1.2 <- lm(conf_dis ~ met_feed, data=testdat3)
summary(med.fit1.2)
confint(med.fit1.2)

med.fit2_bay <- bayesian.boot(conf_dis ~ met_feed, B = 5000, seed = 101, data = testdat3)
med.fit2_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit2_bay$bootEstParam[,2], probs=c(.025, .975))

# Combined outcome model with confidence and met_feed----
out.fit1.2 <- glm(MON2.2 ~ conf_dis + met_feed,
                  data = testdat3, family = binomial("probit"))
summary(out.fit1.2)
confint(out.fit1.2)


# The mediation model -----
set.seed(2023)
med.out1.2<- mediate(med.fit1.2, out.fit1.2, treat = "met_feed", mediator = "conf_dis",
                     sims = 5000,boot=T, boot.ci.type="bca")

summary(med.out1.2) 


################################



# 2.1 Monitoring body condition method EXP2----
# Direct effect MON4.1 ~ EXP2 -----

outfit2TOTAL <- glm(MON4.1 ~ EXP2, 
                    data=testdat3, family=binomial("probit"))

summary(outfit2TOTAL)
confint(outfit2TOTAL)

# conf_dis _ EXP2 (with bootstrap cis) ----- 


summary(med.fit2.1<- lm(conf_dis ~ EXP2, data=testdat3))
confint(med.fit2.1)

med.fit2.1bay<- bayesian.boot(conf_dis ~ EXP2, B = 5000, seed = 101, data = testdat3)
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit2.1bay$bootEstParam[,2], probs=c(.025, .975))




# Combined outcome model with conf and exp2 predictors ----
out.fit2.1 <- glm(MON4.1 ~ conf_dis + EXP2, 
                  data=testdat3, family=binomial("probit"))
summary(out.fit2.1)
confint(out.fit2.1)


# Mediation model -----
set.seed(2023)
med.out2.1 <- mediate(med.fit2.1, out.fit2.1, treat = "EXP2", mediator = "conf_dis",
                      sims = 5000, boot = T, boot.ci.type="bca")


summary(med.out2.1)   
med.out2.1$z.avg.ci  # p<0.05



################################
# 2.2 Monitoring body condition method - metfeed
# Direct effect MON4.1~ met_feed ----

out.fit4TOT <- glm(MON4.1 ~  met_feed, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit4TOT)
confint(out.fit4TOT)

# Mediation effect (NS) ----
med.fit2.2 <- lm(conf_dis ~met_feed, data=testdat3)
summary(med.fit2.2)
confint(med.fit2.2)

med.fit2.2bay <- bayesian.boot(conf_man ~ met_feed, B = 5000, seed = 101, data = testdat3)

#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit2.2bay$bootEstParam[,2], probs=c(.025, .975))


# Combined outcome model with conf and met-feed predictors ----
out.fit2.2 <- glm(MON4.1 ~ conf_dis + met_feed, 
                  data=testdat3, family=binomial("probit"))
summary(out.fit2)
confint(out.fit2)








# Mediation model -----
set.seed(2023)
med.out2.2 <- mediate(med.fit2.2, out.fit2.2, treat = "met_feed", mediator = "conf_dis",
                      sims = 5000, boot = T, boot.ci.type="bca")


summary(med.out2.2)   
med.out2.2$z.avg.ci  # p<0.05




################################
# 3.1 Seasonal management - winter EXP2----
# Direct effect between MET3 ~ EXP2 ------

out.fit1tot <- glm(MET3 ~  EXP2,
                   data = testdat3, family = binomial("probit"))
summary(out.fit1tot)
confint(out.fit1tot)

# Mediation effect exper on summed confidence in management ( with bootstrap version) -----
med.fit3.1 <- lm(conf_man ~ EXP2, data=testdat3)
summary(med.fit3.1)
confint(med.fit3.1)

med.fit3.1_bay <- bayesian.boot(conf_man ~ EXP2, B = 5000, seed = 101, data = testdat3)
med.fit3.1_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit3.1_bay$bootEstParam[,2], probs=c(.025, .975))

# Combined outcome model with confidence and exp2 (NS) ----
out.fit3.1 <- glm(MET3 ~ conf_man + EXP2,
                  data = testdat3, family = binomial("probit"))
summary(out.fit3.1)
confint(out.fit3.1)

# The mediation model -----
set.seed(2023)
med.out3.1<- mediate(med.fit3.1, out.fit3.1, treat = "EXP2", mediator = "conf_man",
                     sims = 5000,boot=T, boot.ci.type="bca")

summary(med.out3.1) 

################################
# 3.2 Seasonal management - winter METFEED----
# Direct effect between MET3 ~ met_feed -----
out.fit2TOT <- glm(MET3 ~  met_feed, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit2TOT)
confint(out.fit2TOT)

# Mediation effect met_feed on confidence with bootstrap version-----
med.fit3.2 <- lm(conf_man ~ met_feed, data=testdat3)
summary(med.fit3.2)
confint(med.fit3.2)
med.fit3.2_bay <- bayesian.boot(conf_man ~ met_feed, B = 5000, seed = 101, data = testdat3)
med.fit3.2_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit3$bootEstParam[,2], probs=c(.025, .975))




# Combined outcome model with confidence and met_feed effect on met3 ----
out.fit3.2 <- glm(MET3 ~  met_feed + conf_man, 
                  data=testdat3, family=binomial("probit"))
summary(out.fit3.2)





# 4. Seasonal management - autumn 
# The mediation model -----
set.seed(2023)
med.out3.2<- mediate(med.fit3.2, out.fit3.2, treat = "met_feed", mediator = "conf_man",
                     sims = 5000,boot=T, boot.ci.type="bca")

summary(med.out3.2) 


################################

#4.1  Seasonal management - AUTUMN EXP2---- 
# Direct effect MET1 ~ EXP2 -----
out.fit2TOT <- glm(MET1 ~  EXP2, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit2TOT)
confint(out.fit2TOT)


# Mediation effect exper on summed confidence in management ( with bootstrap version) -----
med.fit4.1 <- lm(conf_man ~ EXP2, data=testdat3)
summary(med.fit4.1)
confint(med.fit4.1)

med.fit4.1_bay <- bayesian.boot(conf_man ~ EXP2, B = 5000, seed = 101, data = testdat3)
med.fit4.1_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit4.1_bay$bootEstParam[,2], probs=c(.025, .975))


# Combined outcome model with confidence and exp2 (NS) ----
out.fit4.1 <- glm(MET1 ~ conf_man + EXP2,
                  data = testdat3, family = binomial("probit"))
summary(out.fit4.1)





# The mediation model -----
set.seed(2023)
med.out4.1<- mediate(med.fit4.1, out.fit4.1, treat = "EXP2", mediator = "conf_man",
                     sims = 5000,boot=T, boot.ci.type="bca")

summary(med.out4.1) 



################################

#4.2 Seasonal management autumn metfeed     
# Direct effect MET1 ~ met_feed -----
out.fit2TOT <- glm(MET1 ~  met_feed, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit2TOT)
confint(out.fit2TOT)

# Mediation effect met_feed on confidence with bootstrap version-----
med.fit4.2 <- lm(conf_man ~ met_feed, data=testdat3)
summary(med.fit4.2)
confint(med.fit4.2)
med.fit4.2_bay <- bayesian.boot(conf_man ~ met_feed, B = 5000, seed = 101, data = testdat3)
med.fit4.2_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit4.2_bay$bootEstParam[,2], probs=c(.025, .975))

# Combined outcome model with confidence and met_feed effect on met1 ----
out.fit4.2 <- glm(MET1 ~  met_feed + conf_man, 
                  data=testdat3, family=binomial("probit"))
summary(out.fit4.2)











# The mediation model -----
set.seed(2023)
med.out4.2<- mediate(med.fit4.2, out.fit4.2, treat = "met_feed", mediator = "conf_man",
                     sims = 5000,boot=T, boot.ci.type="bca")

summary(med.out4.2) 




################################


# 5.1 Feeding practices - soak hay exp2 ----
# Direct effect of soakhay ~ EXP2 ------

out.fit1tot <- glm(soakhay ~  EXP2,
                   data = testdat3, family = binomial("probit"))
summary(out.fit1tot)
confint(out.fit1tot)


# Mediation effect exper on summed confidence in management ( with bootstrap version) -----
med.fit5.1 <- lm(conf_man ~ EXP2, data=testdat3)
summary(med.fit5.1)
confint(med.fit5.1)

med.fit5.1_bay <- bayesian.boot(conf_man ~ EXP2, B = 5000, seed = 101, data = testdat3)
med.fit5.1
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit5.1_bay$bootEstParam[,2], probs=c(.025, .975))




# Outcome model with management and EXP2 ----

out.fit5.1 <- glm(soakhay ~ conf_man + EXP2,
                  data = testdat3, family = binomial("probit"))
summary(out.fit5.1)
confint(out.fit5.1)

# The mediation model ------
med.out5.1 <- mediate(med.fit5.1, out.fit5.1, treat = "EXP2", mediator = "conf_man",
                      sims = 5000, boot = T, boot.ci.type="bca")

summary(med.out5.1)



################################

# 5.2 Feeding practices - soak hay metfeed ----
# Direct effect soakhay ~ met_feed -----
out.fit2TOT <- glm(soakhay ~  met_feed, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit2TOT)
confint(out.fit2TOT)
# Mediation effect met_feed on confidence with bootstrap version-----
med.fit5.2 <- lm(conf_man ~ met_feed, data=testdat3)
summary(med.fit5.2)
confint(med.fit5.2)
med.fit5.2_bay <- bayesian.boot(conf_man ~ met_feed, B = 5000, seed = 101, data = testdat3)
med.fit5.2_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit5.2_bay$bootEstParam[,2], probs=c(.025, .975))


# Combined outcome model with confidence and met_feed-----
out.fit5.2 <- glm(soakhay ~  met_feed + conf_man, 
                  data=testdat3, family=binomial("probit"))
summary(out.fit5.2)





# 6. Feeding practices - analyse forage      

# The mediation model ------
med.out5.2 <- mediate(med.fit5.2, out.fit5.2, treat = "met_feed", mediator = "conf_man",
                      sims = 5000, boot = T, boot.ci.type="bca")

summary(med.out5.2)




################################

# 6.1 Feeding practices pres forage exp2
# Direct effect of FOR2 ~ EXP2 -----
out.fit2TOT <- glm(FOR2 ~  EXP2, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit2TOT)
confint(out.fit2TOT)

# Mediation effect exper on summed confidence in management ( with bootstrap version) -----
med.fit6.1 <- lm(conf_man ~ EXP2, data=testdat3)
summary(med.fit6.1)
confint(med.fit6.1)

med.fit6.1_bay <- bayesian.boot(conf_man ~ EXP2, B = 5000, seed = 101, data = testdat3)
med.fit6.1_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit6.1_bay$bootEstParam[,2], probs=c(.025, .975))





# Outcome model for exp2 and confidence -----
out.fit6.1 <- glm(FOR2 ~  EXP2 + conf_man, 
                  data=testdat3, family=binomial("probit"))
summary(out.fit6.1)



# The mediation model ------
med.out6.1 <- mediate(med.fit6.1, out.fit6.1, treat = "EXP2", mediator = "conf_man",
                      sims = 5000, boot = T, boot.ci.type="bca")

summary(med.out6.1)





################################

# 6.2 Feeding practices pres forage metfeed     
# Direct effect for FOR2 ~ met_feed -----
out.fit2TOT <- glm(FOR2 ~  met_feed, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit2TOT)
confint(out.fit2TOT)

# Mediation effect met_feed on confidence with bootstrap version-----
med.fit6.2 <- lm(conf_man ~ met_feed, data=testdat3)
summary(med.fit6.2)
confint(med.fit6.2)
med.fit6.2_bay <- bayesian.boot(conf_man ~ met_feed, B = 5000, seed = 101, data = testdat3)
med.fit6.2_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit$bootEstParam[,2], probs=c(.025, .975))


# Combined outcome model with confidence and met_feed-----
out.fit2 <- glm(FOR2 ~  met_feed + conf_man, 
                data=testdat3, family=binomial("probit"))
summary(out.fit2)




#' Although met_feed was associated with greater likelihood of undertaking WMA's, it wasn't associated with greater confidence , thus no mediation









################################

# 7.1 Exercise exp2----
# Direct effect EXER1 - EXP2-----

out.fit1TOT <- glm(EXER1 ~  EXP2,
                   data = testdat3, family = binomial("probit"))
summary(out.fit1TOT)
confint(out.fit1TOT)

# conf_dis _ EXP2 (with bootstrap cis) ----- 
med.fit7.1 <- lm(conf_dis ~ EXP2, data = testdat3)
summary(med.fit7.1)
confint(med.fit7.1)
med.fit7.1_bay <- bayesian.boot(conf_dis ~ EXP2, B = 5000, seed = 101, data = testdat3)
med.fit7.1_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit7.1_bay$bootEstParam[,2], probs=c(.025, .975))

# Combined outcome model with confidence and experience -----

out.fit7.1 <- glm(EXER1 ~ conf_dis + EXP2,
                  data = testdat3, family = binomial("probit"))
summary(out.fit7.1)
confint(out.fit7.1)

# The mediation model ------
med.out7.1 <- mediate(med.fit7.1, out.fit7.1, treat = "EXP2", mediator = "conf_dis",
                      sims = 5000, boot = T, boot.ci.type="bca")

summary(med.out7.1)







################################

# 7.2 Exercise metfeed 
# Direct effect EXER1 ~ met_feed ------
out.fit2tot <- glm(EXER1 ~ met_feed, 
                   data=testdat3, family=binomial("probit"))
summary(out.fit2tot)
confint(out.fit2tot)

# Mediation effect met_feed on confidence with bootstrap version-----
med.fit7.2 <- lm(conf_dis ~ met_feed, data=testdat3)
summary(med.fit7.2)
confint(med.fit7.2)
med.fit7.2_bay <- bayesian.boot(conf_dis ~ met_feed, B = 5000, seed = 101, data = testdat3)
med.fit7.2_bay
#residual bootstrap 95% CI for slope parameter (percentile method)
quantile(med.fit7.2_bay$bootEstParam[,2], probs=c(.025, .975))




# Combined outcome model with confidence and met_feed effect on met3 -----

out.fit7.2 <- glm(EXER1 ~ conf_dis + met_feed,
                  data = testdat3, family = binomial("probit"))
summary(out.fit7.2)
confint(out.fit7.2)




# SUMMARIES OF MEDIATION
summary(med.out1)
med.out1$d.avg.ci
med.out1$z.avg.ci
med.out1$tau.ci

summary(med.out1.2)
summary(med.out2.1)
med.out2.1$z.avg.ci
med.out2.1$d.avg.ci
med.out2.1$tau.ci


summary(med.out2.2)
summary(med.out3.1)
med.out3.1$tau.ci
med.out3.1$d.avg.ci
med.out3.1$z.avg.ci


summary(med.out3.2)
summary(med.out4.1)
med.out4.1$tau.ci
med.out4.1$d.avg.ci
med.out4.1$z.avg.ci

summary(med.out4.2)
summary(med.out5.1)
med.out5.1$tau.ci
med.out5.1$d.avg.ci
med.out5.1$z.avg.ci
summary(med.out5.2)

summary(med.out6.1)
med.out6.1$tau.ci
med.out6.1$d.avg.ci
med.out6.1$z.avg.ci
summary(med.out7.1)
med.out7.1$tau.ci
med.out7.1$d.avg.ci
med.out7.1$z.avg.ci












      
      
      
      
      

# WARD_A_2023_SURVEY_MEDIATION_ANALYSIS
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

# Data original = the raw df with headers, strings as factors and missing values accounted for

setwd("C:/Users/award/OneDrive - SRUC/FINAL YEAR/SURVEY/SmartSurvey_Data")

data_original <- read.csv("raw_survey_complete_plus_partial_responses.csv",
                          header = TRUE, na.strings = "^", stringsAsFactors = TRUE)

library(dplyr)
library(forcats)

## Data underwent filtering and removal of questions for seperate analsis in Excel. 




## Select only those who have given an answer to the first and last questions. 
## This approach is to minimize the number of missing values that were random, 
## and to retain missing values that occurred between the respondent answering the first and last 
## questions- making an assumption that those were skipped purposefully and thus contain information 

dat1_original <- data_original %>% drop_na(number_of_horses, conf_rec_lam) 
# rows with na's between 1st and last question dropped
dat1<- data_original %>% drop_na(number_of_horses, conf_rec_lam) 

#Creating new variables (Tidying the raw data - collapsing, re-ordering and re-coding messy variables)

#Create a new variable coded 1 if the respondent sources information from themselves, and 0 if they outsource 
dat1$info_feeding <- fct_collapse(dat1$info_feed2, 
                                  "1" = c("BSc in Animal nutrition and physiology","Experience", "I am always researching.",
                                          "Multiple sources - wild horse research groups (what horses evolved to eat), modern research like Carol Hughes Equine Micro Biome, Equitopia holistic education etc. NOT feed companies ",
                                          "My equine based qualification", "Own research" , "Own research and experience",
                                          "Reading ", "Research ", "specialist breed forum" , "The Laminitis Site", 
                                          "Web based feeding guidance" ),
                                  
                                  "0" =c("available (specialist) feed in shop",
                                         "Barefoot schools/groups",
                                         "As well as trained friend that owns animal feed store & follow farriers advice too.",
                                         "&also vet/nutritionistadvice asked if unsure",
                                         "A combination of all of the above ", 
                                         "Barefoot schools/groups", 
                                         "equine podiatrist", "Equine podiatrist ","don't feed", "BHS", 
                                         "Barefoot schools/groups","Family / friend advice",
                                         "Feed retailer / feed representative", "Groom" ,
                                         "Industry professional (E.g. farrier, equine dental technician)", 
                                         "Previous ownerd" , 
                                         "Professional advice (E.g. veterinarian/ nutritionist)", "Tack shop" ,
                                         "Trainer / coach", "Yard owner / manager", 
                                         "My EP (barefoot trimmer) and barefoot group on social media ")
)






#Create a new variable coded 1 if the respondent sources information from themselves, and 0 if they outsource 
summary(dat1$info_weight)

dat1$info_weight <- fct_collapse(dat1$info_weight, 
                                 "Other industry professional" = c("Industry professionals (e.g.managers / instructors )",
                                                                   "Farmer / agricultural contractor"))



#Create a new variable coded 1 if the respondent sources information from themselves, and 0 if they outsource 
summary(dat1$info_health)






#Create a new variable coded 1 if the respondent sources information from themselves, and 0 if they outsource 
dat1$info_pasture 










#Create a new variable coded 1 if the respondent sources information from themselves, and 0 if they outsource 
dat1$info_nutrition <- fct_collapse(dat1$info_nutrition, 
                                    "Other industry professional" = c("Industry professionals (e.g.managers / instructors )",
                                                                      "Farmer / agricultural contractor"))






# Create a binary variable - always vs not always controlling pasture access 
dat1$PAST <- fct_collapse(dat1$freq_turnoutroutine,
                          "1" = "Always", 
                          "0" = c("Occasionally","Never"))
# convert to numeric 
dat1$PAST <- as.numeric(dat1$PAST)












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

dat1$MON3 <- fct_collapse(dat1$wgt_how, 
                          "1" = c("Weighbridge","Scales", "And weighbridge when available ", "Weigh bridge and tape", 
                                  "Weigh bridge couple of times a year to help calibrate weigh tape",
                                  "Weighbridge annually, tapemeasure every 2 weeks, daily condition score visually",
                                  "Also used weighbridge and tape","Weighbridge","Weighbridge also monthly", 
                                  "Estimation through calculation","Tape calibrated against a weigh bridge", "Tape measure", 
                                  "Tape measure and visual assessment", "tape measure following visual assessment", "and tape",
                                  "And weigh tape weekly", "Weigh tape", "Weight tape with tb and BCS with ( year old youngster ",
                                  "Weight tape, photos and feeling ribs", "Weightape and weighbridge ", "When required weight tape",
                                  "I do use a tape but also by eye", "I use both tape & visual", "Tape and visual","Tape measure",
                                  "Both tape and visually", "Normally a tape measure and sometimes a weigh bridge when it is available",
                                  "Plus weight tape", "Tape and visual ", "Weigh tape, sometimes compared to a weight formula x"
                          ),
                          "0" = c("Observation ", "tape measure, visually, and how many hole i can get the girth up ?? ", "Visual", 
                                  "Visual and tape measure", "Visual assessment", "Visual assessment ", "And visual ", 
                                  "and visual assessment","And visually & calculation ", 
                                  "Daily visually, weekly with tape and twice yearly on weight bridge ",
                                  "visual assessment and weighbridge","And by sight/feel","Visual assessment", "And visually ",
                                  "Visual also", "Visual and feel", "Visual and hands on", "Visual assessment daily, tape measure once a month",
                                  "Visual assessment recently due to being happy with weight and lightly monitoring during winter ",
                                  "A mix of first 4 ", "also check pulses and hoof heat", "and calculation ","App",
                                  "Because not accurate unless weighbridge available. Condition scoring seems more practical",
                                  "Body weight scoring","Growing baby",
                                  "Shes been weighed twice though in her life which I much prefer", "Combination as said previously",
                                  "I generally do not monitor my horses weight")
)


dat1$MON3.1 <- fct_collapse(dat1$wgt_how, 
                            "1" = c("Weighbridge","Tape measure", "Estimation through calculation"),
                            "0" = "Visual assessment", 
                            "NA" = c("Scales", "And weighbridge when available ", "Weigh bridge and tape", 
                                     "Weigh bridge couple of times a year to help calibrate weigh tape",
                                     "Weighbridge annually, tapemeasure every 2 weeks, daily condition score visually",
                                     "Also used weighbridge and tape","Weighbridge","Weighbridge also monthly", 
                                     "Tape calibrated against a weigh bridge", "Tape measure and visual assessment", "tape measure following visual assessment", "and tape",
                                     "And weigh tape weekly", "Weigh tape", "Weight tape with tb and BCS with ( year old youngster ",
                                     "Weight tape, photos and feeling ribs", "Weightape and weighbridge ", "When required weight tape",
                                     "I do use a tape but also by eye", "I use both tape & visual", "Tape and visual","Tape measure",
                                     "Both tape and visually", "Normally a tape measure and sometimes a weigh bridge when it is available",
                                     "Plus weight tape", "Tape and visual ", "Weigh tape, sometimes compared to a weight formula x","Observation ", "tape measure, visually, and how many hole i can get the girth up ?? ", "Visual", 
                                     "Visual and tape measure", "Visual assessment ", "And visual ", 
                                     "and visual assessment","And visually & calculation ", 
                                     "Daily visually, weekly with tape and twice yearly on weight bridge ",
                                     "visual assessment and weighbridge","And by sight/feel","Visual assessment", "And visually ",
                                     "Visual also", "Visual and feel", "Visual and hands on", "Visual assessment daily, tape measure once a month",
                                     "Visual assessment recently due to being happy with weight and lightly monitoring during winter ",
                                     "A mix of first 4 ", "also check pulses and hoof heat", "and calculation ","App",
                                     "Because not accurate unless weighbridge available. Condition scoring seems more practical",
                                     "Body weight scoring","Growing baby",
                                     "Shes been weighed twice though in her life which I much prefer", "Combination as said previously",
                                     "I generally do not monitor my horses weight")
)

# Create new variable - method of body condition monitoring with 1 = a scale / measurement based method
# and 0 = a non-measurement based method or "other"


dat1$MON4 <- fct_collapse(dat1$bcs_how, 
                          "1" = c("Body condition scoring using a 1-5 scale system", 
                                  "Body condition scoring using a 1-9 scale system",
                                  "Measurement of heart-girth, belly-girth and neck",
                                  "Taking measurements and using an online body condition calculator"),
                          "0" = c("Estimation through visual assessment and feeling the horse's body",
                                  "Estimation through visual assessment only","Other") 
)

dat1$MON4.1 <- fct_collapse(dat1$bcs_how, 
                            "1" = c("Body condition scoring using a 1-5 scale system", 
                                    "Body condition scoring using a 1-9 scale system",
                                    "Measurement of heart-girth, belly-girth and neck",
                                    "Taking measurements and using an online body condition calculator"), 
                            "0" = c("Estimation through visual assessment and feeling the horse's body", 
                                    "Estimation through visual assessment only","Other") )






# Create new binary variable - shown how tobody condition score
dat1$MON5 <- ifelse(dat1$bcs_shown=="Yes",1,0) #numeric








# Create new binary variable - had horse BCS assessed by professional
dat1$MON6 <- ifelse(dat1$bcs_by_prof=="Yes", 1, 0) # numeric 









# Create new binary variable - aware of pf analysis 
dat1$FOR1 <- ifelse(dat1$pres_forage_analysis_aware=="Yes",1,0) # numeric 








# Create new binary variable - had pf analysed 
dat1$FOR2<- fct_collapse(dat1$pres_forage_analysis,
                         "0" = c("No","Don't know"),
                         "1" ="Yes")





# Create a new binary response variable, feed soaked hay 

dat1$soakhay <- fct_collapse(dat1$pres_forage_prefer, 
                             "1" = "Soaked hay", 
                             "0" = c("Forage substitute (E.g. short chop fibre )", "Haylage", 
                                     "None- my horse does not receive additional forage", 
                                     "Steamed hay", "Hay", "Mixture of hay and haylage") 
)



# Create new nominal factor - most important aspect of exercise 
# Create new nominal factor - most important aspect of exercise 
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








# Create new variable - AGE 
dat1$EXP4 <- dat1$number_of_horses
dat1$EXP4 <- as.numeric(dat1$EXP4)







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



# COnfidnece in identifying conditions collapsed into below neutral, neutral and above neutral 



#Recognising laminitis 
dat1$CONFOB2 <- ordered (fct_collapse(dat1$OB2, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))




#Recognising equine metabolic syndrome (EMS) 
dat1$CONFOB3 <- ordered (fct_collapse(dat1$OB3, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))





#Recognising pituitary pars intermedia dysfunction (PPID)
dat1$CONFOB4 <- ordered (fct_collapse(dat1$OB4, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))





#Recognising colic 
dat1$CONFOB5 <- ordered (fct_collapse(dat1$OB5, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))






#Recognising loss of condition 
dat1$CONFOB6 <- ordered (fct_collapse(dat1$OB6, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))









# Create new 3 point variable collapsed from the 5 point confidence in identifying obesity and related conditions



# Recognising obesity 
dat1$CONFOB1 <- ordered (fct_collapse(dat1$OB1, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))



#Recognising laminitis 
dat1$CONFOB2 <- ordered (fct_collapse(dat1$OB2, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))




#Recognising equine metabolic syndrome (EMS) 
dat1$CONFOB3 <- ordered (fct_collapse(dat1$OB3, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))





#Recognising pituitary pars intermedia dysfunction (PPID)
dat1$CONFOB4 <- ordered (fct_collapse(dat1$OB4, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))





#Recognising colic 
dat1$CONFOB5 <- ordered (fct_collapse(dat1$OB5, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))






#Recognising loss of condition 
dat1$CONFOB6 <- ordered (fct_collapse(dat1$OB6, 
                                      "1"= c("1", "2", "3"), 
                                      "2" = "4", 
                                      "3" = "5"))








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





#Create a 3 level version of the confidence variable
dat1$confman1 <- ordered(fct_collapse(dat1$conf_pastureman,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))








# Confidence in herd management 
dat1$coman2 <- fct_collapse(dat1$conf_herdman,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
# Order factor 
dat1$coman2 <- factor (dat1$coman2,ordered = TRUE, levels = c("1","2", "3", "4", "5"))



#Create a 3 level version of the confidence variable
dat1$confman2 <- ordered(fct_collapse(dat1$conf_herdman,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))









#Confidence in making decisions on turnout routine 
dat1$coman3 <- fct_collapse(dat1$conf_turnoutroutine,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman3 <- factor (dat1$coman3,ordered = TRUE, levels = c("1","2", "3", "4", "5"))



#Create a 3 level version of the confidence variable
dat1$confman3 <- ordered(fct_collapse(dat1$conf_turnoutroutine,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))









# Confidence in deciding on the type of preserved forage to feed the horse
dat1$coman4 <- fct_collapse(dat1$conf_typepf,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman4 <- factor (dat1$coman4,ordered = TRUE, levels = c("1","2", "3", "4", "5"))



#Create a 3 level version of the confidence variable
dat1$confman4 <- ordered(fct_collapse(dat1$conf_typepf,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))









#Confidence in deciding on the amount of preserved forage to feed 
dat1$coman5 <- fct_collapse(dat1$conf_amountpf,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor 
dat1$coman5 <- factor (dat1$coman5,ordered = TRUE, levels = c("1","2", "3", "4", "5"))





#Create a 3 level version of the confidence variable
dat1$confman5 <- ordered(fct_collapse(dat1$conf_amountpf,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))








# Confidence in feeding the appropriate type of supplemtary feeding 
dat1$coman6 <- fct_collapse(dat1$conf_typef,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman6 <- factor (dat1$coman6,ordered = TRUE, levels = c("1","2", "3", "4", "5"))



#Create a 3 level version of the confidence variable
dat1$confman6 <- ordered(fct_collapse(dat1$conf_typef,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))









# Confidence deciding on the appropriate amount of supplementary feeding 
dat1$coman7 <- fct_collapse(dat1$conf_amountf,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor 
dat1$coman7 <- factor (dat1$coman7,ordered = TRUE, levels = c("1","2", "3", "4", "5"))




#Create a 3 level version of the confidence variable
dat1$confman7 <- ordered(fct_collapse(dat1$conf_amountf,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))








# Confidence making decisions on rugging, clipping, stabling etc
dat1$coman8 <- fct_collapse(dat1$conf_rug,
                            "1" = "Not confident at all", 
                            "2" = c("Only a little bit", "A little"), 
                            "3" = "Neutral", 
                            "4" = "Fairly confident",
                            "5" = "Very confident")
#Order the factor
dat1$coman8 <- factor (dat1$coman8,ordered = TRUE, levels = c("1","2", "3", "4", "5"))



#Create a 3 level version of the confidence variable
dat1$confman8 <- ordered(fct_collapse(dat1$conf_rug,
                                      "1" = c("Not confident at all", "Only a little bit", "A little", 
                                              "Neutral"), 
                                      "2" = "Fairly confident", 
                                      "3" = "Very confident"))



### Create 3 level variables frequency of decision making 
dat1$freman1 <- ordered(fct_collapse(dat1$freq_pastureman,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))


dat1$fr1bin <-ordered(fct_collapse(dat1$freq_pastureman,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))



dat1$freman2 <- ordered(fct_collapse(dat1$freq_herdman,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))


dat1$fr2bin <-ordered(fct_collapse(dat1$freq_herdman,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))


dat1$freman3 <- ordered(fct_collapse(dat1$freq_turnoutroutine,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))


dat1$fr3bin <-ordered(fct_collapse(dat1$freq_turnoutroutine,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))



dat1$freman4 <- ordered(fct_collapse(dat1$freq_typepf,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))


dat1$fr4bin <-ordered(fct_collapse(dat1$freq_typepf,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))



dat1$freman5 <- ordered(fct_collapse(dat1$freq_amountpf,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))

dat1$fr5bin <-ordered(fct_collapse(dat1$freq_amountpf,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))



dat1$freman6 <- ordered(fct_collapse(dat1$freq_typef,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))

dat1$fr6bin <-ordered(fct_collapse(dat1$freq_typef,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))




dat1$freman7 <- ordered(fct_collapse(dat1$freq_amountf,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))

dat1$fr7bin <-ordered(fct_collapse(dat1$freq_amountf,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))


dat1$freman8 <- ordered(fct_collapse(dat1$freq_rug,
                                     "1" = "Never", 
                                     "2" = "Occasionally", 
                                     "3" = "Always"),
                        levels=c("1", "2", "3"))

dat1$fr8bin <-ordered(fct_collapse(dat1$freq_rug,
                                   "1" = c("Never", "Occasionally"),
                                   "2" = "Always"),
                      levels=c("1", "2"))



# Create new variable - total control over premises 
dat1$CO1 <- ifelse(dat1$X46_grass =="Yes",1,0)

str(dat1$CO1)


# Create new variables - frequency of decision making 
dat1$CO2 <- fct_collapse(dat1$freq_pastureman,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always")




dat1$CO3 <- fct_collapse(dat1$freq_amountf,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always")




dat1$CO4 <- fct_collapse(dat1$freq_amountpf,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always")



dat1$CO5 <- fct_collapse(dat1$freq_herdman,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always")



dat1$CO6 <- fct_collapse(dat1$freq_rug,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always")



dat1$CO7 <- fct_collapse(dat1$freq_turnoutroutine,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always") 



dat1$CO8 <- fct_collapse(dat1$freq_typef,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always")



dat1$CO9 <- fct_collapse(dat1$freq_typepf,
                         "0" = c("Never","Occasionally"),
                         "1" = "Always")




dat1$lam_ident <-ifelse(dat1$lam_ident=="Yes",1,0)


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




# Create a new subset with only the re-codedvariables included in the analysis
# Subset the new variables 
dat2 <- subset(dat1, select = c("met_feed", "soakhay","lam_ident", "PAST","MON1.2", "MON2.2", "MON3", "MON4","MON4.1", "MON5", "MON6", "FOR1", "FOR2",
                                "EXER1", "MET1", "MET2","MET3", "MET4", "EXP1", "EXP2", "EXP3","EXP4", "EXP5", "EXP6",
                                "confman1","confman2","confman3","confman4","confman5","confman6","confman7","confman8",
                                "coman1", "coman2", "coman3", "coman4", "coman5", "coman6", "coman7","coman8",
                                "CONFOB1","CONFOB2","CONFOB3","CONFOB4","CONFOB5","CONFOB6",
                                "CO1", "CO2","CO3","CO4","CO5","CO6","CO7","CO8","CO9",
                                "OB1", "OB2", "OB3", "OB4", "OB5", "OB6", 
                                "info_pasture", "info_weight", "info_nutrition", "info_health", "info_feeding",
                                "freman1", "freman2", "freman3", "freman4", "freman5", "freman6", "freman7", "freman8", 
                                "fr1bin", "fr2bin", "fr3bin", "fr4bin", "fr5bin", "fr6bin", "fr7bin", "fr8bin"
) 

) 



# Check the structure 
str(dat2)



# There are a variety of ordered variables,factors with 2 levels , and binary numerical variables 
# For some analysis, the factors with 2 levels are required to be numerical 
# Create two versions of the subsetted dataframe- one factors only and one numerical


#Factor version dat2


#Recode lam_ident 
head(data_original$lam_ident, 12)# check original coding (Yes =1 and No = 0)
head(dat2$lam_ident,10)
dat2$lam_ident <- as.factor(dat2$lam_ident)
head(dat2$lam_ident,10)



#Recode PAST 
head(dat1$freq_turnoutroutine,10) # check original coding 
head(dat2$PAST,10)# check recoding ( 1 - Always, 2 = Occasionally or never)
dat2$PAST <- as.factor(dat2$PAST)
head(dat2$PAST,10)
levels(dat2$PAST) <- c("1","0")
head(dat2$PAST,10)



#Recode mon5 
head(data_original$bcs_shown,10)
head(dat1$MON5,10) # check original coding 
head(dat2$MON5,10)# check recoding ( 1 - YES, 0 = No)
dat2$MON5 <- as.factor(dat2$MON5)
head(dat2$MON5,10)


#Recode mon6 
head(data_original$bcs_by_prof,10)
head(dat1$MON6,10) # check original coding 
head(dat2$MON6,10)# check recoding ( 1 - YES, 0 = No)
dat2$MON6 <- as.factor(dat2$MON6)
head(dat2$MON6,10)


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
names <- c("met_feed", "soakhay", "lam_ident", "PAST","MON1.2", "MON2.2", "MON3", "MON4","MON4.1", "MON5", "MON6", "FOR1", "FOR2",
           "EXER1", "MET1", "MET2","MET3", "MET4", "EXP1", "EXP2", "EXP3","EXP4", "EXP5", "EXP6",
           "confman1","confman2","confman3","confman4","confman5","confman6","confman7","confman8",
           "coman1", "coman2", "coman3", "coman4", "coman5", "coman6", "coman7","coman8",
           "CONFOB1","CONFOB2","CONFOB3","CONFOB4","CONFOB5","CONFOB6",
           "CO1", "CO2","CO3","CO4","CO5","CO6","CO7","CO8","CO9",
           "OB1", "OB2", "OB3", "OB4", "OB5", "OB6", 
           "info_pasture", "info_weight", "info_nutrition", "info_health", "info_feeding",
           "freman1", "freman2", "freman3", "freman4", "freman5", "freman6", "freman7", "freman8", 
           "fr1bin", "fr2bin", "fr3bin", "fr4bin", "fr5bin", "fr6bin", "fr7bin", "fr8bin")



#Create a new variable 
dat2_character <- dat2
names(dat2_character)
dat2_character[names] <- sapply(dat2_character[names],as.character) #convert the factor levels to characters
dat2_numeric <- dat2_character
dat2_numeric[names] <- sapply(dat2_character[names],as.numeric)# convert the characters to numbers





#check that factor and numeric data align 
head(dat2$info_feeding, 30)
head(dat2_numeric$info_feeding, 30)



##Export these data frames if you want 
write.csv(dat2,"C:/Users/award/OneDrive - SRUC/Research 2022/Project Survey/data/ Ash_dat_2.csv"
          , row.names = TRUE)

write.csv(dat2_numeric,"C:/Users/award/OneDrive - SRUC/Research 2022/Project Survey/data/ Ash_dat_2_numeric.csv"
          , row.names = TRUE)




semdat<-subset(dat2_numeric, select = c(met_feed,coman1, coman2, coman3, coman4, coman5, coman6, coman7, coman8,
                                        confman1 ,confman2 ,confman3,confman4,confman5,confman6, confman7, confman8,
                                        CONFOB1,CONFOB2,CONFOB3, CONFOB4, CONFOB5,CONFOB6,OB1, OB2, OB3, OB4, OB5, OB6, 
                                        EXP1, EXP2, EXP3, EXP5, EXP6, soakhay,
                                        PAST, MON1.2, MON2.2, MON3,MON4,MON4.1,MON5, MON6,FOR1,FOR2,EXER1,MET1, MET2, MET3, MET4, lam_ident))

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

# Mediators 
testdat$confman8 <- as.factor(testdat$confman8)
testdat$confman4 <- as.factor(testdat$confman4)
testdat$CONFOB1 <- as.factor(testdat$CONFOB1)


testdat2 <- na.omit(testdat)






#### Likert scale version of confidence testing ########
# Libraries, data prep, descriptive stats ------
library(ltm)
library("lmboot") #bootstrap cis
library(e1071) # testing for skewness and kurtosis 
library(gmodels) # cross tabs

management_5point <- subset(testdat2, select=c(2:9))
summary(management_5point)
disease_5point <- subset(testdat2, select=c("OB1", "OB2", "OB3", "OB6"))
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

  CrossTable(testdat3$met_feed, testdat3$MON2.2)
  # There is a fairly low number of owners in the "less frequently than occasionally" grouping
  # with 24 and 21 in the two met feed categories. ** 
  
  CrossTable(testdat3$met_feed, testdat3$MON4.1)

  CrossTable(testdat3$met_feed, testdat3$MET3)
  CrossTable(testdat3$met_feed, testdat3$MET1)
  
  CrossTable(testdat3$met_feed, testdat3$FOR2) # count 26 in one group
  CrossTable(testdat3$met_feed, testdat3$soakhay) # count 27 in one group
  
  CrossTable(testdat3$met_feed, testdat3$EXER1) #count 25 in one group


  
  CrossTable(testdat3$EXP2, testdat3$MON2.2)
  # There is a fairly low number of owners in the "less frequently than occasionally" grouping
  # with 23 and 22 in the two met feed categories. ** 
  
  CrossTable(testdat3$EXP2, testdat3$MON4.1)
  
  CrossTable(testdat3$EXP2, testdat3$MET3)
  CrossTable(testdat3$EXP2, testdat3$MET1)
  
  CrossTable(testdat3$EXP2, testdat3$FOR2) #count 26 in one group
  CrossTable(testdat3$EXP2, testdat3$soakhay) # count 27 in one group
  
  CrossTable(testdat3$EXP2, testdat3$EXER1) # count 25 in one group
  

  M1 <- glm(EXP2 ~ met_feed, data=testdat3, family=binomial ("probit"))
  summary(M1)
  M2 <- glm(EXP6 ~ met_feed, data=testdat3, family=binomial ("probit"))
  summary(M2)
  M3 <- glm(EXP2 ~ EXP6, data=testdat3, family=binomial ("probit"))
  summary(M1)
  M4 <- glm(MON4.1 ~ MON2.2, data=testdat3, family=binomial ("probit"))
  summary(M4)
  confint(M4)
  M5 <- glm(MON3 ~ MON1.2, data=testdat3, family=binomial ("probit"))
  summary(M5)
  confint(M5)
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
      
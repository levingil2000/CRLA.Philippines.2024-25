## This project is an exploration of the recent results of CRLA. The data available are the
## BosY (pretest), EoSY (posttest) and the previous year results.

# Import data
pretest <- read.csv("C:/Users/Owner/Downloads/_CRLA National Dashboard_BoSY 2024-25 Assessment Results_Table.csv")
posttest <- read.csv("C:/Users/Owner/Downloads/_CRLA National Dashboard_EoSY 2024-25 Assessment Results_Table (1).csv")
previousyr <- read.csv("C:/Users/Owner/Downloads/_CRLA National Dashboard_EoSY 2023-24 Assessment Results_Table.csv")

# basic libraries
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)

#fix some variable types
pretest <- pretest %>%
  mutate(across(c(Low.Emerging.Reader,
                  High.Emerging.Reader,
                  Developing.Reader,
                  Transitioning.Reader,
                  Reading.At.Grade.Level),as.numeric))
pretest <- pretest %>%
  mutate(across(c(Total.Low.Emerging,
                  G1.Lower.Emergent,
                  G2.MT.Lower.Emergent,
                  G2.Fil.Lower.Emergent,
                  G3.MT.Lower.Emergent,
                  G3.Fil.Lower.Emergent,
                  G3.Eng.Lower.Emergent), as.integer))
posttest <- posttest %>%
  mutate(across(c(Low.Emerging.Reader,
                  High.Emerging.Reader,
                  Developing.Reader,
                  Transitioning.Reader,
                  Reading.At.Grade.Level),as.numeric))
posttest <- posttest %>%
  mutate(across(c(Total.Low.Emerging,
                  G1.Lower.Emergent,
                  G2.MT.Lower.Emergent,
                  G2.Fil.Lower.Emergent,
                  G3.MT.Lower.Emergent,
                  G3.Fil.Lower.Emergent,
                  G3.Eng.Lower.Emergent), as.integer))

#Explore the top schools in terms of the number of grade level ready readers
top_schools_g1 <- posttest %>%
  mutate(G1.Grade.Level.prop = G1.Grade.Level/G1.Total.Assessed) %>%
  arrange(desc(G1.Grade.Level.prop)) %>%
  slice_head(n=10)
View(top_schools_g1) ##Shows that there seems to be an error with their data with grade 1 specially with the top schools

#Imma do grade 3 instead and check if there are iregularities like this one
top_schools_g3_mothertongue <- posttest %>%
  mutate(g3.mt.grade.level.prop= G3.MT.Grade.Level/G3.Total.Assessed) %>%
  arrange(desc(g3.mt.grade.level.prop)) %>%
  slice_head(n=10)

#Explore the top district
top_divisions <- posttest %>%
  group_by(Division, District) %>%
  summarise(mean.grade.level = mean(Reading.At.Grade.Level, na.rm=TRUE)) %>%
  ungroup()
# top division 50
top_division_50 <- top_divisions %>%
  arrange(desc(mean.grade.level)) %>%
  slice_head(n=50)
View(top_division_50)
# Which divisions are on the top districts
division_table <- top_division_50 %>%
  group_by(Division) %>%
  summarise(n=n_distinct(District)) %>%
  ungroup()
division_table

#### Top 100 schools in terms of grade level ready readers
top100 <- posttest %>%
  arrange(desc(Reading.At.Grade.Level)) %>%
  slice_head(n=100) %>%
  select(c(Region, Division, District, School.Name, Language, Reading.At.Grade.Level))
# Check their mother tongue proficiency
MT_distribution <- top100 %>%
  group_by(Language) %>%
  summarise(n=n_distinct(School.Name)) %>%
  ungroup()
MT_distribution

MTproficiencyPlot <- ggplot(MT_distribution, mapping = aes(x=Language, y=n))+
  geom_col()+
  coord_flip()
MTproficiencyPlot
### Explore the performance per particular language
### to do so, we introduce a Metric called  ReadingLevelIndex which would score the following as follows:
# Low emerging = 1, High Emerging = 2, Developing = 3, Transitioning = 4, Grade Level = 5
# After such we compute the mean score of each school

# Introduce the new variable
posttest <- posttest %>%
  mutate(Reading_Index = (Total.Low.Emerging+Total.High.Emerging*2+Total.Developing*3+Total.Transitioning*4+Total.At.Grade.Level*5)/Total.Assessed)
posttest$Reading_Index #check it
hist(posttest$Reading_Index) #graph it if there's something fuzzy about the distribution
posttestfixingna <- posttest %>%
  filter(is.na(Reading_Index))
View(posttestfixingna) # check if there are failed instances

#Turned out there are a couple of rows that has 0 total assessed which i'll be removing
#And there seems to be a couple of rows which has NA values on low emerging which can easily be supplied

#Remove the rows with 0 assessed
posttest.cleaned.for.index <- posttest %>%
  filter(Total.Assessed != 0)
posttestcheckna <- posttest.cleaned.for.index %>%
  filter(is.na(Total.Low.Emerging))
View(posttestcheckna)# Check the remaining NA and approach each value carefully
#Diome ES has inconsistent values so i'd be removing it 
posttest.cleaned.for.index <- posttest.cleaned.for.index %>%
  filter(School.ID != 110618) %>% #Diome ES has inconsistent values so i'd be removing it 
  filter(School.ID != 104498) %>% #Paraway Mangyan School has inconsistent values so i'd be removing it as well
  mutate(Total.Low.Emerging = replace_na(Total.Low.Emerging, 0))  #I'll Keep Kalauan School since the value for Low Emerging should simply be zero
#Resolve the index for this fixed dataframe
posttest.cleaned.for.index <- posttest.cleaned.for.index %>%
  mutate(Reading_Index = (Total.Low.Emerging+Total.High.Emerging*2+Total.Developing*3+Total.Transitioning*4+Total.At.Grade.Level*5)/Total.Assessed)

#Now we check the performance of each particular language group
MT.performance <- posttest.cleaned.for.index %>%
  group_by(Language) %>%
  summarise(mean_Reading_Index=mean(Reading_Index)) %>%
  arrange(desc(mean_Reading_Index)) %>%
  ungroup() %>%
  mutate(Language = reorder(Language, mean_Reading_Index))
View(MT.performance)

ggplot(MT.performance, aes(x=Language, y=mean_Reading_Index, fill=mean_Reading_Index))+
  geom_col()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()


#### Check on the effects of MT in grade 3 to other languages. Again we build the index scores
#Clean
posttest.cleaned.for.index.g3.version <- posttest.cleaned.for.index %>%
  filter(G3.Total.Assessed != 0)

Grade3MTeffects <- posttest.cleaned.for.index.g3.version %>%
  mutate(G3_MT_Index = (G3.MT.Lower.Emergent + G3.MT.Higher.Emergent*2 + G3.MT.Developing*3 + G3.MT.Transitioning*4 + G3.MT.Grade.Level*5)/G3.Total.Assessed) %>%
  mutate(G3_Fil_Index =(G3.Fil.Lower.Emergent + G3.Fil.Higher.Emergent*2 + G3.Fil.Developing*3 + G3.Fil.Transitioning*4 + G3.Fil.Grade.Level*5)/G3.Total.Assessed ) %>%
  mutate(G3_Eng_Index =(G3.Eng.Lower.Emergent + G3.Eng.Higher.Emergent*2 + G3.Eng.Developing*3 + G3.Eng.Transitioning*4 + G3.Eng.Grade.Level*5)/G3.Total.Assessed ) %>%
  select(c(Region, Division, District, School.ID, School.Name, Language, Reading_Index, G3_MT_Index, G3_Eng_Index,G3_Fil_Index))

sum(is.na(Grade3MTeffects$G3_MT_Index))# 1 na value
#Identify the error
Grade3MTeffects.na.for.fixing <- Grade3MTeffects %>%
  filter(is.na(G3_MT_Index))
posttest.cleaned.for.index %>%
  filter(School.ID == 124790)

#Turns out it's still Calauan ES where 0s are written as NA
posttest.cleaned.for.index.g3.version <- posttest.cleaned.for.index.g3.version %>%
  mutate(G3.MT.Lower.Emergent = replace_na(G3.MT.Lower.Emergent, 0)) %>%
  mutate(G3.Fil.Lower.Emergent = replace_na(G3.Fil.Lower.Emergent,0)) %>%
  mutate(G3.Eng.Lower.Emergent = replace_na(G3.Eng.Lower.Emergent,0))
#just go back to the previous code to fix everything

#Now we compare the indices
summary(Grade3MTeffects)

#Check for linear relationships in the reading indices
install.packages("GGally")
library(GGally)

ggpairs(Grade3MTeffects, 
        columns = c("G3_MT_Index", "G3_Eng_Index", "G3_Fil_Index"))
##Dang it, turns out that there a couple of values for EngIndex and FIlIndex above 5
# So we check if we can resolve it one by 

Grade3MTeffects.badvalues <- Grade3MTeffects %>%
  filter(G3_Fil_Index > 5 | G3_Eng_Index > 5)
Grade3MTeffects.badvalues # so there are 46 of them
#Most of which are division errors where the total amount of students in each category exceeds the stated total
#What i'll do here is to use an actual sum of the total number of students, rather than using the column for the total

Grade3MTeffects <- posttest.cleaned.for.index.g3.version %>%
  mutate(G3_MT_Index = (G3.MT.Lower.Emergent + G3.MT.Higher.Emergent*2 + G3.MT.Developing*3 + G3.MT.Transitioning*4 + G3.MT.Grade.Level*5)/(G3.MT.Lower.Emergent + G3.MT.Higher.Emergent + G3.MT.Developing + G3.MT.Transitioning +G3.MT.Grade.Level)) %>%
  mutate(G3_Fil_Index =(G3.Fil.Lower.Emergent + G3.Fil.Higher.Emergent*2 + G3.Fil.Developing*3 + G3.Fil.Transitioning*4 + G3.Fil.Grade.Level*5)/(G3.Fil.Lower.Emergent + G3.Fil.Higher.Emergent + G3.Fil.Developing + G3.Fil.Transitioning +G3.Fil.Grade.Level) ) %>%
  mutate(G3_Eng_Index =(G3.Eng.Lower.Emergent + G3.Eng.Higher.Emergent*2 + G3.Eng.Developing*3 + G3.Eng.Transitioning*4 + G3.Eng.Grade.Level*5)/(G3.Eng.Lower.Emergent + G3.Eng.Higher.Emergent + G3.Eng.Developing + G3.Eng.Transitioning +G3.Eng.Grade.Level) ) %>%
  select(c(Region, Division, District, School.ID, School.Name, Language, Reading_Index, G3_MT_Index, G3_Eng_Index,G3_Fil_Index))

summary(Grade3MTeffects)

View(
  Grade3MTeffects %>%
    arrange(G3_MT_Index) %>%
    slice_head(n=10)
)

#non tagalog exploration
Grade3MTeffects.NonTagalog <- Grade3MTeffects %>%
  filter(Language != "Tagalog")

ggpairs(Grade3MTeffects.NonTagalog, 
        columns = c("G3_MT_Index", "G3_Eng_Index", "G3_Fil_Index"))
#MT scores for every regional language
MT_scores_per_dialect <- Grade3MTeffects %>%
  group_by(Language) %>%
  summarise(mean_MT_scores = mean(G3_MT_Index, na.rm=TRUE)) %>%
  arrange(desc(mean_MT_scores)) %>%
  ungroup()
View(MT_scores_per_dialect)

Grade3MTeffects.cebuano <- Grade3MTeffects %>%
  filter(Language == "Sinugbuanong Binisaya")
ggpairs(Grade3MTeffects.cebuano, 
        columns = c("G3_MT_Index", "G3_Eng_Index", "G3_Fil_Index"))
mean(Grade3MTeffects.NonTagalog$G3_Fil_Index, na.rm=TRUE)
Grade3MTeffects.Tagalog <-Grade3MTeffects %>%
  filter(Language == "Tagalog")
mean(Grade3MTeffects.Tagalog$G3_Fil_Index,na.rm=TRUE)


### I want to check whether the pretest MT language affects the change
pretest.cleaning.g3 <- pretest %>%
  filter(G3.Total.Assessed !=0) %>%
  mutate(G3.MT.Lower.Emergent = replace_na(G3.MT.Lower.Emergent, 0)) %>%
  mutate(G3.Fil.Lower.Emergent = replace_na(G3.Fil.Lower.Emergent, 0)) %>%
  mutate(G3.Eng.Lower.Emergent = replace_na(G3.Eng.Lower.Emergent, 0)) %>%
  mutate(pre_G3_MT_Index = (G3.MT.Lower.Emergent+G3.MT.Higher.Emergent*2+G3.MT.Developing*3+G3.MT.Transitioning*4+G3.MT.Grade.Level*5)/(G3.MT.Lower.Emergent+G3.MT.Higher.Emergent+G3.MT.Developing+G3.MT.Transitioning+G3.MT.Grade.Level)) %>%
  mutate(pre_G3_Fil_Index = (G3.Fil.Lower.Emergent+G3.Fil.Higher.Emergent*2+G3.Fil.Developing*3+G3.Fil.Transitioning*4+G3.Fil.Grade.Level*5)/(G3.Fil.Lower.Emergent+G3.Fil.Higher.Emergent+G3.Fil.Developing+G3.Fil.Transitioning+G3.Fil.Grade.Level)) %>%
  mutate(pre_G3_Eng_Index = (G3.Eng.Lower.Emergent+G3.Eng.Higher.Emergent*2+G3.Eng.Developing*3+G3.Eng.Transitioning*4+G3.Eng.Grade.Level*5)/(G3.Eng.Lower.Emergent+G3.Eng.Higher.Emergent+G3.Eng.Developing+G3.Eng.Transitioning+G3.Eng.Grade.Level)) %>%
  select(Region, Division, District, School.ID, School.Name, Language, pre_G3_MT_Index, pre_G3_Fil_Index, pre_G3_Eng_Index)

mean(pretest.cleaning.g3$pre_G3_Eng_Index, na.rm=TRUE) 

#Combined dataset for grade3
pre.post.g3 <- inner_join(pretest.cleaning.g3, Grade3MTeffects, by = "School.ID")
ggpairs(pre.post.g3,
        columns = c("pre_G3_MT_Index","pre_G3_Fil_Index","pre_G3_Eng_Index","Reading_Index","G3_MT_Index","G3_Eng_Index","G3_Fil_Index"))
model <- lm(pre_G3_MT_Index ~  G3_MT_Index + G3_Fil_Index + G3_Eng_Index, data= pre.post.g3)

check.group.scores <- pre.post.g3 %>%
  group_by(Language.x) %>%
  summarise(preMTmean = mean(pre_G3_MT_Index, na.rm=TRUE))%>%
  arrange(desc(preMTmean)) %>%
  ungroup()
check.group.scores

pre.post.g3 <- pre.post.g3 %>%
  select(c("Region.x","Division.x","District.x","School.ID","School.Name.x","Language.x",
         "pre_G3_MT_Index","pre_G3_Fil_Index","pre_G3_Eng_Index","Reading_Index","G3_MT_Index",
         "G3_Eng_Index","G3_Fil_Index"))
grade3_cleaned <- pre.post.g3 %>%
  na.omit()
write.csv(grade3_cleaned, file ="test.csv")

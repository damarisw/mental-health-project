#Load the packages
library(readxl)
library(tidyverse)

#import and prints the file sheet names.
sheet_names <- excel_sheets("survey.xlsx")
sheet_names

#Reads all the sheets.
mental_health <-lapply(sheet_names, function(x) {as.data.frame(read_excel("survey.xlsx", sheet = x))})

#Renames the mental health elements.
names(mental_health) <- sheet_names

#Prints the head of the first and second sheet.
head(mental_health$'Sheet 2')
head(mental_health$Sheet1)

#This code combines the two sheets into one datafile.
mental_health <-do.call("rbind.data.frame",mental_health)

#Drop the time stamp column from the file.
mental_health <-mental_health [,-1]

#The age column has some negative values and values consisting of over 100. Replacing those values with NA.
mental_health <- mental_health %>% mutate(Age = replace(Age, which(Age < 0), NA),
                                                Age = replace(Age, which(Age > 100), NA))

#Summary statistics of the age column.
summary(mental_health$Age)

#Checks and prints the categories in the gender column.
categories <- unique(mental_health$Gender)
print(categories)

#converts the categories in the column to lower case.
mental_health$Gender <- tolower(mental_health$Gender)

#defines the gender column based on the categories present.
male <- c('male-ish', 'cis male', 'male (cis)', 'make', 'mail','m', 'maile', 'male ', 'msle', 'mal', 'man', 'malr', 'cis man')

female <- c('female', 'f ', 'female (cis)', 'woman', 'cis female', 'cis-female/femme', 'femail', 'femake')

gq <- c('p','a little about you', '2','nah','fluid','neuter','something kinda male','agender','1','queer','guy(ish)^_^','all',
        'queer/she/they','na','genderqueer', 'non-binary', 'androgyne','enby', 'male leaning androgynous','ostensibly male,unsure what that really means')

transgender<- c('transwoman','trans-female','female(trans)')

#Replacing the gender categories.
mental_health <- mental_health %>% mutate(Gender = replace(Gender, which(Gender %in% male), 'Male'),
                                                Gender = replace(Gender, which(Gender %in% female), 'Female'),
                                                Gender = replace(Gender, which(Gender %in% gq), 'gq'),
                                                Gender = replace(Gender, which(Gender %in% transgender), 'transgender'))


#The plot shows the age distribution of the mental health survey respondents.
library(ggplot2)
ggplot(mental_health,aes(x=Age))+
  geom_histogram(fill=" dark orange") +
  labs(title = "Histogram of the Mental Survey Respondents' Age")


#The plot compares family history and seeking help ratio
mental_health %>% ggplot(aes(x=family_history, fill = (seek_help))) +
  geom_bar(position = "fill") + ggtitle("Family_history Treatment Ratio")




















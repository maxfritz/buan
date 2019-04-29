install.packages('tidyverse')
install.packages('naniar') 
install.packages('visdat')
install.packages('plyr')
library('plyr')
library('visdat')
library("tidyverse")
library('naniar')

#import and look through data
raw <- read.csv(url("https://raw.githubusercontent.com/maxfritz/buan/master/dog_id_max_ranks-Table%201.csv"), header=TRUE, sep=",")

#object.size(raw)
dim(raw)
head(raw) 
summary(raw)

data$Breed <- factor(data$Breed)
data$Dog_Fixed <- factor(data$Dog_Fixed)
data$DNA_Tested <- factor(data$DNA_Tested)
data$Free_Start_User <- factor(data$Free_Start_User)
data$Subscribed <- factor(data$Subscribed)
data$Membership_ID <- factor(data$Membership_ID)

# check missing values
raw %>%
  miss_var_summary()%>%
  filter(n_miss > 0)


#graph some missing values
missing_values<-sapply(raw, function(x) sum(is.na(x)))
missing_values
missing_df <- data.frame(keyName=names(missing_values), value=missing_values, row.names=NULL)
missing_df %>%
  filter(value>0)
gg_miss_var(raw)+theme_bw()
vis_dat(data)+coord_flip()

# we will omit two empty variables, X and X.1
# in addition, we decided to omit some time label variables that appear to be solely administrative in purpose.
# zip is largely junk and Last_Active_At alone is not useful.
# all variables omitted are:
#           X
#           X.1
#           Mean.ITI..days.
#           Mean.ITI..minutes.
#           Median.ITI..days.
#           Median.ITI..minutes.
#           ZIP
#           Last_Active_At
#           Membership_ID
# and brings our total number of variables to 26.

drop_vars <- names(raw) %in% c('X','X.1','Mean.ITI..days.','Mean.ITI..minutes.','Median.ITI..days.','Median.ITI..minutes.', 'Zip','Last_Active_At','Membership_ID') 
data <- raw[!drop_vars]

levels(data$Membership_Type)
summary(data$Membership_Type)

# Per data description, data should only include 5 levels for the Membership factor variable.
# Therefore, we exclude those data points with membership values outside of these stated levels (1:5) as erroneous

data <- data %>%
  filter(Membership_Type %in% c(1:5))
summary(data$Membership_Type)

data$Membership_Type <- factor(data$Membership_Type)
summary(data$Membership_Type)

#last data clean
my_data <- data

my_data_filter <- my_data%>%
  filter(Breed != "I Don't Know")%>%
  filter(City != "") %>%
  filter(Country != "")%>%
  filter(State != "")%>%
  filter(City != "N/A") %>%
  filter(Country != "N/A")%>%
  filter(State != "N/A")

my_data_filter$City <- factor(my_data_filter$City)
my_data_filter$Country <- factor(my_data_filter$Country)
my_data_filter$State <- factor(my_data_filter$State)
my_data_filter$Gender <- factor(my_data_filter$Gender)
my_data_filter$Breed <- factor(my_data_filter$Breed)

# Breed has too many factors
length(unique(my_data_filter$Breed))

# Create table showing frequency of each levels occurrence.
table1 <- data.frame(table(my_data_filter$Breed))

# Orders the table in descending order of frequency.
table1 <- table1[order(-table1$Freq),]
table1

# shrink factor levels into top 25 (25 distinct + 2 mislabelled but equivalent pairs)
noChange <- table1$Var1[1:27]
noChange <- factor(noChange)

my_data_filter$Breed <- (ifelse(my_data_filter$Breed %in% noChange, my_data_filter$Breed, "Other")) 

my_data_filter$Breed = factor(my_data_filter$Breed)
length(unique(my_data_filter$Breed))

my_data_filter %>%
  group_by(Breed) %>%
  tally() %>%
  arrange(-n)

my_data_filter$Breed <- revalue(my_data_filter$Breed, 
                                c("723"="Other", 
                                  "736"="Other",
                                  "632"="Labrador Retriever",
                                  "539"="Golden Retriever",
                                  "486"="German Shepherd Dog",
                                  "105"="Australian Shepherd",
                                  "218"="Border Collie",
                                  "789"="Poodle",
                                  "538"="Golden Doodle",
                                  "631"="Labradoodle",
                                  "892"="Shih Tzu",
                                  "657"="Labrador Retriever-Golden Retriever Mix",
                                  "277"="Boxer",
                                  "715"="Miniature Schnauzer",
                                  "883"="Shetland Sheepdog",
                                  "425"="Dachshund",
                                  "145"="Beagle",
                                  "448"="Doberman Pinscher",
                                  "406"="Cockapoo",
                                  "468"="English Springer Spaniel",
                                  "1000"="American Pit Bull Terrier",
                                  "35"="Chihuahua",
                                  "361"="Yorkshire Terrier",
                                  "267"="Boston Terrier",
                                  "580"="Havanese",
                                  "Labrador Retriever-Golden Retriever Mix"="Golden Retriever-Labrador Retriever Mix",
                                  "348"="Cavalier King Charles Spaniel"
                                ))

write.csv(my_data_filter, file = "final_data.csv", row.names = FALSE)

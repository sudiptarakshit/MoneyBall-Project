#Money Ball Project

#Open the Batting.csv file and assign it to a dataframe called batting 
batting<-read.csv('/Users/sudiptarakshit/Desktop/Batting.csv')
#Checking out 'Batting' dataset head
head(batting)
#check out the structure of 'Batting' dataset
str(batting)
#Create BA column to calculate Batting Average (BA)
batting$BA <- batting$H/batting$AB
# Check the first 5 and last 5 entries of BA column
head(batting$BA,5) #first 5
tail(batting$BA,5) #last 5
#Create OBP column to calculate On Base Percentage (OBP)
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP +batting$SF)
#Create SLG column to calculate Slugging Average
#Singles (X1B)
batting$X1B<- batting$H- batting$X2B - batting$X3B - batting$HR
#Slugging Average (SLG)
batting$SLG <- ((1*batting$X1B)+(2*batting$X2B)+(3*batting$X3B)+(4*batting$HR))/batting$AB

#Check the structure of modified data frame
str(batting)

##############
#We know we don't just want the best players, we want the most undervalued players
# It means we will also need to know current salary information! We have salary information in the csv file 'Salaries.csv'
# Load the 'Salaries.csv' file into a data frame called 'sal'
sal<-read.csv('/Users/sudiptarakshit/Desktop/Salaries.csv')
#Check the sal dataset
head(sal, 5) #first 5
tail(sal, 5) #last 5
#Merging Salary Data with Batting Data
#The minimum year in the yearID column in batting data goes back to 1871! Our salary data starts at 1985 
summary(batting)
# It means we need to remove the batting data that occured before 1985
batting<-subset(x = batting, subset = yearID>=1985)
#Check if the subset reassignment is proper
summary(batting)
#Merge the batting and sal data frames by c('playerID','yearID'). 
#Call the new data frame combo
combo<-merge(x = batting,y = sal, by = c('playerID','yearID'))

#Check the combo dataset
summary(combo)

#Oakland A's lost 3 key players during the off-season. 
#We'll want to get their stats to see what we have to replace. 
#The players lost were: 
#first baseman 2000 AL MVP Jason Giambi ('giambja01') to the New York Yankees, 
#outfielder Johnny Damon ('damonjo01') to the Boston Red Sox and 
#infielder Rainer Gustavo "Ray" Olmedo ('saenzol01').

#Get a data frame called lost_players from the combo data frame consisting of those 3 players
lost_players<-subset(x = combo,subset = playerID %in% c('giambja01','damonjo01','saenzol01'))

lost_players

#Since all these players left after 2001 in the offseason
#let's only concern ourselves with the data from 2001
lost_players<-subset(lost_players,yearID == 2001)

lost_players

#Reduce the lost_players data frame to the following columns: 
#playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB
lost_players<-lost_players[,c('playerID','H','X2B','X3B','HR','OBP','BA','AB')]
lost_players

#Replacement Players
#Task - Find Replacement Players for the key three players left! 
#However, we have three constraints:
#The total combined salary of the three players can not exceed 15 million dollars.
#Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#Their mean OBP had to equal to or greater than the mean OBP of the lost players


#First only grab available players from year 2001
library(dplyr)
avail.players<-filter(.data = combo, yearID==2001)

#Plot to see where we should cut-off for salary in respect to OBP:
library(ggplot2)
ggplot(data = avail.players,mapping = aes(x = OBP,y = salary))+
  geom_point()
#We'll choose salary 8 million as a cutt off point. 
#There are also a lot of players with OBP==0. Let's get rid of them too.
avail.players<-filter(.data = avail.players,salary<8000000,OBP>0)

#Find the total AB of the lost players. 
summarise(.data = lost_players,sum(lost_players$AB))
#meaning we should probably cut off my avail.players at 1500/3= 500 AB.

avail.players<-filter(.data = avail.players,AB>=500)

#Now let's sort by OBP and see what we've got!
possible_players <- head(arrange(avail.players,desc(OBP)),10)

#Grab the relevant columns
possible_players <- possible_players[,c('playerID','OBP','AB','salary')]
possible_players

#Can't choose giambja again as he left.  
#But the other ones look good (2-4). I choose them!
possible_players[2:4,]

#Done#
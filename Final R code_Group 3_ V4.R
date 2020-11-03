
########## PROJECT CODE #########################################

# SECTION 2.0 Micromap  -- Tejasri Surapaneni

#################################################################

# dots representation #

install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("micromapST")
library(micromapST)

setwd("D:/STAT 515")

traffic <-  read.csv("D:\\STAT 515\\Traffic Data.csv", header = T)
head(traffic)
View(traffic)

paneldesc = data.frame(
  type = c('map', 'id', 'dot', 'dot'),
  lab1 = rep("", 4),
  lab2 = c('','','2016 Fatal Rates', '2017 Fatal Rates' ),
  lab3 = c('','Highest Fatal Rates','possible 0-4000', 'possible 0-4000'),
  col1 = c(NA,NA,'X2016.Fatalities', 'X2017Fatalities')
)

windows(width = 10, height = 10)
micromapST(traffic, paneldesc, 
           rowNamesCol = 'State',
           rowNames = 'full',
           plotNames = 'full',
           sortVar = 'X2017Fatalities' , ascend = FALSE,
           title =c("According to NHTSA, Total Traffic Fatal Crashes", "2016 and 2017"
           ),
           ignoreNoMatches = TRUE)

## scatterplot and dot with referance values ##

install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("micromapST")
library(micromapST)

setwd("D:/STAT 515")

traffic <-  read.csv("D:\\STAT 515\\Traffic Data.csv", header = T)

head(traffic)
View(traffic)

paneldesc = data.frame(
  type = c('map', 'id', 'dot', 'scatdot'),
  lab1 = c('','','2016 Alcohol Impaired ', 'Comparision of Rates' ),
  lab2 = c('','','Fatalities', '2016 (X-axis) and 2017 (Y-axis)' ),
  lab3 = c('','Highest Fatal Rates','Possible 0-1500', 'Fatals in 2016'),
  lab4 = c('', '','', '2017'),
  col1 = c(NA,NA,'X2016Number', 'X2017Number'),
  col2 = c(NA,NA,NA, 'X2016Number'),
  refVals = c(NA,NA,142,186)
)

windows(width = 10, height = 10)

micromapST(traffic, paneldesc, 
           rowNamesCol = 'State',
           rowNames = 'full',
           plotNames = 'full',
           sortVar = 'X2016Number' , ascend = FALSE,
           title =c("NHTSA Reports, Number of Alcohol Impaired Driving Fatalities", "2016 and 2017"
           ),
           ignoreNoMatches = TRUE)




########################################################################################

#SECTION 3.0 RELATIONSHIP COMPARISON ------ Ka Hang Kwong

########################################################################################

#Set Working Directory
setwd("C:/Users/kkwong/Desktop/Ka/Graduate School/Fall 2018/STAT 515/Project")

library(tidyverse)

#Reading the File into a DataFrame
Fatalisites_Data <- read.csv(file='Traffic Data.csv',header = T)

#Analyze the Data
head(Fatalisites_Data)

View(Fatalisites_Data)

str(Fatalisites_Data)

#Create the ScattorPlot Graph
ggplot(Fatalisites_Data, aes(x = X2017Fatalities, y = X2017Number)) +
  geom_point(color="red") +
  scale_y_continuous(breaks = seq(0, 4000, by = 500), limits = c(0,4000))+
  scale_x_continuous(breaks = seq(0, 4000, by = 500), limits = c(0,4000))+  
  stat_smooth(method=lm,formula=y~poly(x,5), color="blue",fill="cyan")+
  labs(x="2017 Total Fatalities Accidents",
       y="2017 Total Fatalities Accidents Related to Alcohol",
       title=paste('National Highway Traffic Safety Administration',
                   '2017 Total Fatalities Accidents',
                   'vs',
                   '2017 Total Fatalities Accidents Related to Alcohol', 
                   'By State',sep ='\n')) +
  theme(plot.title = element_text(hjust = 0.5))  


# Cites

citation(package = 'base')
RStudio.Version() 
citation("tidyverse")


########################################################################################

#SECTION 4.0 COMBO GRAPH   ------ Naveena Anbu

########################################################################################

# Comparing percentage change of fatalities between 2016 and 2017

# To load packages
library(ggplot2)
library(tidyverse)

# To cite R and R packages in final redesign project paper
citation("ggplot2")
citation("tidyverse")

# Reading the Traffic Data CSV file to create a data frame
traffic_data.dfm <- read_csv(file = "Traffic Data.csv")

# To identify data types of all data items in the dataset 
sapply(traffic_data.dfm, typeof)

# Defining function to remove percentage and converting it to a numeric value
remove_percentage.function <- function(data){
  data <- gsub("%", "", data)
  return (as.numeric(data))
}

# Preparing/Cleaning data to plot the graph 
traffic_data.dfm$TotalFatalitiesPercChange <- remove_percentage.function(traffic_data.dfm$TotalFatalitiesPercChange)
traffic_data.dfm$AlcoholImpFatalitiesPercChange <- remove_percentage.function(traffic_data.dfm$AlcoholImpFatalitiesPercChange)

# Abbreviating state name to represent the state concisely in graph
traffic_data.dfm$State <- state.abb[match(traffic_data.dfm$State,state.name)]

# Clean the data - Remove any missing field
traffic_data.dfm <- na.omit(traffic_data.dfm)

# Viewing the edited data frame to verify the correctness of data
# View(mydata2)

# Added below line to avoid me replacing previous team member's graph in a new window
windows(width = 15, height = 10)

# Producing combo graph:
#  i) Bar plot to visualize 'Total Fatalities'
# ii) Line graph to visualize 'Alcohol Related Fatalities'
ggplot(traffic_data.dfm, aes(x = reorder(State,-TotalFatalitiesPercChange), y = TotalFatalitiesPercChange, group=1)) + 
  geom_bar(stat="identity", aes(fill="Total Fatalities"), width = 0.5) +
  scale_fill_manual(values = c("blue")) +
  geom_line(data = traffic_data.dfm, 
            aes(x = State, y = AlcoholImpFatalitiesPercChange, linetype = "Alcohol Related Fatalities"), 
            stat="identity", size=1, color = "red") +
  geom_point(data = traffic_data.dfm, aes(x = State, y = AlcoholImpFatalitiesPercChange), color = "red") +
  labs(x="U.S. States",
       y="Percentage Change",
       title="Percentage Change of fatalities between 2016 and 2017") +
  scale_y_continuous(limits=c(-40, 80), 
                     breaks = c(-40, -20, 0, 20, 40, 60, 80), 
                     labels = c("-40%", "-20%", "0%", "20%", "40%", "60%", "80%")) +
  theme_grey() + 
  theme(plot.title=element_text(hjust = 0.5), 
        panel.border=element_blank(),
        panel.background = element_rect(fill= "grey97"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        legend.title=element_blank())

# Added below line to avoid replacing my graph with next team member's graph in the window
# Specifying the window size based on previous member's windows specification
windows(width = 10, height = 10)

# To identify R version to cite in final Redesign project paper
# sessionInfo()


########################################################################################

#SECTION 5.0 Staked Bar and Bar Plot    ------ Arsadur Rahman 

########################################################################################

#Set the directory 
setwd("C:/Users/rahma/OneDrive/Desktop")

crash = read.csv("Traffic Data.csv", header = TRUE)


#Library 
library(ggplot2)
library(tidyverse)
library(reshape2)



#Analyze the Data
head(crash)

View(crash)

str(crash)

#Create the Stack Barplot for 2016 - 2017 

fatal<- melt(crash[,c('State','X2016.Fatalities','X2017Fatalities')],id.vars = 1)
fatal <- fatal[order(-fatal$value),] 
ggplot(fatal, aes(x = State, y =value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack')+
  geom_text(aes(label=value),size = 2, position = position_stack(vjust = 0.3))+
  coord_flip()+
  labs(x = "State", 
       y = "Total No. of Fatalitties", 
       title = "2016-2017 Stacked in total Fatalities Accident by State")



#####These Barplot was created using different CSV file#####  

#Library 
library(ggplot2)
library(tidyverse)



#Set the directory 
setwd("C:/Users/rahma/OneDrive/Desktop")

crash2 = read.csv("CR_2017_c.csv", header = TRUE)

#Rearrange the Dataset Highest to Lowest 
crashdata2 <- ctash %>%
  arrange(desc(tf6))


#Create Barplot for the year of 2016  

ggplot(crash2, aes(crash2 = reorder(state, tf6), y = tf6)) + 
  geom_bar(stat = "identity", fill = c("black")) + 
  scale_y_continuous(breaks=seq(0, 4000, 250)) +
  coord_flip() +
  labs(x = "State", 
       y = "Total No. of Fatalitties", 
       title = "2016 total Fatalities Accident by State - Highest to Lowest")

#Create the Barplot 2017 
ggplot(crash2, aes(crash2 = reorder(state, tf7), y = tf7)) + 
  geom_bar(stat = "identity", fill = c("black")) + 
  scale_y_continuous(breaks=seq(0, 4000, 250)) +
  coord_flip() +
  labs(x = "State", 
       y = "Total No. of Fatalitties", 
       title = "2017 total Fatalities Accident by State - Highest to Lowest")
# Cites

citation(package = 'base')
RStudio.Version(1.1.463 windows 10)
citation("tidyverse")




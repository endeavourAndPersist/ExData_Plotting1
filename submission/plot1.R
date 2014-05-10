#Plot1.R 10/05/2014
#Script created in fulfilment of Project 1 of Coursera Exploratory Data 
#Analysis Course.

#The script loads data from the household_power_consumption.txt file assumed
#to be in the current working directory, cleans the data for use in a histogram 
#plot and then plots the graph of:
# #Global Active Power vs Frequency
# The script incorpoates 2 functions:
# getData(): This function loads and cleans the Data returning a dataframe
# plot1(): this function plots the graphs to a plot1.png image in the wd

#Note: In the final line of the script the plot1() function is called enabling
#automatic generation of the png file.

#Load the lubridate library to deal with dates in the dataset
library(lubridate)

#Load the data, assumes the household_power_consumption.txt file is in the
#current working directory
getData<-function(){
  #load the data with read.table to enable the seperator ';' to be specified
  #The data we for the dates 01-02-2007 and 02-02-2007 is within the first
  #7000 rows so only load those 7000
  x<-read.table('household_power_consumption.txt',sep=';',header=T,nrows=70000,stringsAsFactors=F)
  
  #Create a new column of type Date combining the Date and Time columns 
  x$datetime<-dmy_hms(paste(x$Date,x$Time))
  
  #Convert th Date column into a Date Type
  x$Date<- dmy(x$Date)
  
  #Extract the rows of data with dates of 01-02-2007 and 02-02-2007
  data<-x[x$Date %in% dmy(c('01-02-2007','02-02-2007')),]
  
  #Convert the Global Active Power column to numeric
  data$Global_active_power<-as.numeric(data$Global_active_power)
    
  #return the cleaned data
  data
}

#plot the histogram
plot1<-function(){
  
  #get the data
  data<-getData()
  
  #open the PNG device use the default 480x480 size and set the bg
  #to transperent to match the GitHub forked images
  png('plot1.png',bg='transparent')
  
  #plot the histogram
  with(data,hist(Global_active_power,col='red',main='Global Active Power',
                 xlab='Global Active Power (kilowatts)',ylab='Frequency'))
  
  #close the device
  dev.off()
}

#Automatically call the plot1() function
plot1()

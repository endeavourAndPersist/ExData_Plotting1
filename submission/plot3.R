#Plot3.R 10/05/2014
#Script created in fulfilment of Project 1 of Coursera Exploratory Data 
#Analysis Course.

#The script loads data from the household_power_consumption.txt file assumed
#to be in the current working directory, cleans the data for use in a scatter 
#plot and then plots the graph of:
# #DateTme vs Sub Metering (1,2 and 3)

# The script incorpoates 2 functions:
# getData(): This function loads and cleans the Data returning a dataframe
# plot3(): this function plots the graph to a plot3.png image in the wd

#Note: In the final line of the script the plot3() function is called enabling
#automatic generation of the png file.

#Load the lubridate library to deal with dates in the dataset
library(lubridate)

#Load the data, assumes the household_power_consumption.txt file is in the
#current working directory
#Load the lubridate library to deal with dates in the dataset
library(lubridate)

#Load the data 
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
  
  #Convert the sub metering columns to numeric
  data$Sub_metering_1<-as.numeric(data$Sub_metering_1)
  data$Sub_metering_2<-as.numeric(data$Sub_metering_2)
  data$Sub_metering_3<-as.numeric(data$Sub_metering_3)
  
  #return the cleaned data
  data
}

#plot the line graph
plot3<-function(){
  
  #get the data
  data<-getData()
  
  #open the PNG device
  png('plot3.png')
  
  #Set the background to transparent to match the project spec and plot 
  #a scatterplot
  par(bg='transparent')
  with(data,plot(datetime,Sub_metering_1,type='l',xlab='',
                 ylab='Energy sub metering'))
  with(data,points(datetime,Sub_metering_2,col='red',type='l'))
  with(data,points(datetime,Sub_metering_3,col='blue',type='l'))
  
  #Create a legend in the top right with lines as symbols 
  legend("topright", lty=1, col = c('black','red', 'blue'), 
         legend = c('Sub_metering_1', 'Sub_metering_2','Sub_metering_3'))
  
  #close the device
  dev.off()
}

#Automatically call the plot3() function
plot3()
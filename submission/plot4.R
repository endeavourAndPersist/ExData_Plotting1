#Plot4.R 10/05/2014
#Script created in fulfilment of Project 1 of Coursera Exploratory Data 
#Analysis Course.

#The script loads data from the household_power_consumption.txt file assumed
#to be in the current working directory, cleans the data for use in a 4 grid 
#plot and plots the graphs of:
# #DateTme vs Global Active Power
# #DateTme vs Voltage
# #DateTme vs Sub Metering (1,2 and 3)
# #DateTme vs Global Reactive Power

# The script incorpoates 2 functions:
# getData(): This function loads and cleans the Data returning a dataframe
# plot4(): this function plots the graphs to a plot4.png image in the wd

#Note: In the final line of the script the plot4() function is called enabling
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
  
  #Convert the Global Active Power,Voltage and sub metering columns to numeric
  data$Global_active_power<-as.numeric(data$Global_active_power)
  data$Sub_metering_1<-as.numeric(data$Sub_metering_1)
  data$Sub_metering_2<-as.numeric(data$Sub_metering_2)
  data$Sub_metering_3<-as.numeric(data$Sub_metering_3)
  data$Sub_Voltage<-as.numeric(data$Voltage)
  
  #return the cleaned data
  data
}

#plot the 4 graphs
plot4<-function(){
  
  #get the data
  data<-getData()
  
  #open the PNG device
  png('plot4.png')
  
  #Set grid to 2x2 and the background to transparent to match the 
  #project spec and plot
  par(mfrow=c(2,2),bg='transparent')
  
  #plot the DateTime vs Global Active Power Graph
  with(data,plot(datetime,Global_active_power,type='l',xlab='',
                 ylab='Global Active Power (kilowatts)'))
  
  #Plot the DateTime vs Voltage Graph
  with(data,plot(datetime,Voltage,type='l'))
  
  #Plot the DateTime vs Sub Metering Graph
  with(data,plot(datetime,Sub_metering_1,type='l',xlab='',
                 ylab='Energy sub metering'))
  with(data,points(datetime,Sub_metering_2,col='red',type='l'))
  with(data,points(datetime,Sub_metering_3,col='blue',type='l'))
  
  #Create a legend with lines as symbols but without a border i.e lty=1 and bty='n'
  legend("topright", lty=1, col = c('black','red', 'blue'), 
         legend = c('Sub_metering_1', 'Sub_metering_2','Sub_metering_3'),bty='n')
  
  #Plot the DateTime vs Global Reactive power graph
  with(data,plot(datetime,Global_reactive_power,type='l'))
  
  #close the device
  dev.off()
}

#Automatically call the plot4() function
plot4()
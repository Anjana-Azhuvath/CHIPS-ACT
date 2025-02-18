########################################################################
#                    Laading Libraries
########################################################################
library(tseries)
library(readr)
library(zoo)
library(xts)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(psych)
library(tibble)
########################################################################
#                    Laading Data
########################################################################
IntelClose=readxl::read_xlsx("/Users/anjanaraja/Desktop/ECON_675/Data/closing_price.xlsx",sheet = 8)
Nasdaq=read.csv("/Users/anjanaraja/Desktop/ECON_675/Data/nasdaq.csv")

########################################################################
#      Cleaning and Organizing Intel Data              
########################################################################

# Drop last three cols:
IntelClose=IntelClose[,1:(ncol(IntelClose)-4)]

#Sort Dates:
IntelClose=IntelClose[order(IntelClose$Date),]


#Calculating Returns for Intel:
IntelClose$Return=NA
for(i in 2:length(IntelClose$`Close/Last`)){
  IntelClose$Return[i]=(IntelClose$`Close/Last`[i]-IntelClose$`Close/Last`[i-1])/IntelClose$`Close/Last`[i-1]
}

#Dropping first row:
IntelClose=IntelClose[-1,]

#Checking for missing values:There are no missing values
sum(is.na(IntelClose))

#Checking for duplicates:There are no duplicates
sum(duplicated(IntelClose))

#Create an event index:The event occurred on Mar 20 2024. 
event=as.Date("2024-03-20")

#Initializing event date as 0
IntelClose$index=ifelse(IntelClose$Date==event,0,NA)

#Incremental Value
inc=1

#Indexing 264 days prior to the event
for (i in seq_len(nrow(IntelClose))) {
  if (IntelClose$Date[i] < event) {
    if (i == 1) {
      IntelClose$index[i] <- inc
    } else {
      IntelClose$index[i] <- IntelClose$index[i - 1] + inc
    }
  }
}

########################################################################
#      Statistical Test on Intel Returns              
########################################################################

#Testing for Normality: Shapiro Test
Shapiro=shapiro.test(IntelClose$Return)

#Testing for Normality: QQ Plot
qqnorm(IntelClose$Return,main = "Checking for normality of Intel Returns")
qqline(IntelClose$Return)

#Checking for Stationarity: KPSS test
ADF=adf.test(IntelClose$Return)

#Subsetting estimation window:
estimation_window=IntelClose[1:240,c("Date","Return","index")]
event_window=IntelClose[241:250,c("Date","Return","index")]


########################################################################
#      Plotting Returns              
########################################################################
#Plotting closing prices:
plot1 <- ggplot(IntelClose, aes(x = Date)) +
  geom_line(aes(y = `Close/Last`, color = "Close Price"), size = 0.5, col="blue") +  # Plot closing prices
  labs(title = "Intel Daily Closing Prices ",
       y = "Value",
       x = "Date") +
  theme_minimal() 

#Plotting Intel returns:
plot2 <- ggplot(IntelClose, aes(x = Date, y=Return)) +
  geom_line(size = 0.5, col="red") +  # Plot closing prices
  labs(title = "Intel Daily Returns ",
       y = "Value",
       x = "Date") +
  theme_minimal() 

########################################################################
#      Cleaning and Organizing NASDAQ Data              
########################################################################

#Convert from character to date:
library(anytime)
library(lubridate)
Nasdaq$Date=anydate(Nasdaq$Date)

#Sort Dates
Nasdaq=Nasdaq[order(Nasdaq$Date),]

#Drop irrelevant cols:
Nasdaq=Nasdaq[,-3]


#Calculating returns for Nasdaq:
Nasdaq$Return=NA
for(i in 2:length(Nasdaq$Close.Last)){
  Nasdaq$Return[i]=(Nasdaq$Close.Last[i]-Nasdaq$Close.Last[i-1])/Nasdaq$Close.Last[i-1]
}

#Drop the first row:
Nasdaq=Nasdaq[-1,]


#Renaming Columns:
colnames(Nasdaq)=c("Date","ClosingPrices","Rm")

#Merging market returns with Intel Returns
estimation_combined=merge(Nasdaq, estimation_window, by="Date")

#Total study window:
event_study=rbind(estimation_combined,event_combined)

#Plotting Market Return and Intel Returns:
plot3 <- ggplot(event_study, aes(x = Date)) +
  geom_line(aes(y = Rm, color = "NASDAQ Market Returns"), size = 0.5) +  # Plot CRSP Market Returns
  geom_line(aes(y = Return, color = "Intel Returns"), size = 0.5) +  # Plot Intel Returns
  labs(title = "Plotting Market Return against Intel Returns (Daily)",
       y = "Value",
       x = "Date") +
  scale_color_manual(values = c("NASDAQ Market Returns" = "black", "Intel Returns" = "orchid")) +  # Corrected color mapping
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title

########################################################################
#      OLS Market Model             
########################################################################
normal_returns=lm(estimation_combined$Return~ estimation_combined$Rm)
output=summary(normal_returns)
########################################################################
#      Calculating the expected return for the event window            
########################################################################
#Merging Market Return with event date:
event_combined=merge(Nasdaq, event_window, by="Date")

#Calculating normal expected returns:
alpha=as.numeric(normal_returns$coefficients[1])
beta=as.numeric(normal_returns$coefficients[2])

#Calculating expected normal returns:
event_combined$Re=alpha+beta*event_combined$Rm

#Abnormal Returns:Difference between the actual and expected return
event_combined$AR=event_combined$Return - event_combined$Re
  
#Calculating cumulative abnormal returns:
event_combined$CAR=cumprod(1+event_combined$AR)
event_combined$index=c("-4","-3","-2","-1","0","1","2","3","4","5")
########################################################################
#      Plotting Expected Return and Actual Return          
########################################################################
plot4 <- ggplot(event_combined, aes(x = Date)) +
  geom_line(aes(y = Re, color = "Expected Returns"), size = 0.5) +  # Plot CRSP Market Returns
  geom_line(aes(y = Return, color = "Actual Returns"), size = 0.5) +  # Plot Intel Returns
  labs(title = "Plotting Actual Returns against Expected Returns for the Event Window",
       y = "Value",
       x = "Date") +
  scale_color_manual(values = c("Expected Returns" = "turquoise", "Actual Returns" = "orange")) + 
  geom_vline(xintercept = as.Date("2024-03-20"), linetype = "dashed", color = "red", size = 0.5)+# Corrected color mapping
  theme_minimal() +
  theme(legend.title = element_blank()) 
########################################################################
#      Plotting Cumulative Abnormal Returns           
########################################################################
plot(event_combined$CAR, type = "l", main = "Cumulative Abnormal Returns",
     xlab = "Event Window", ylab = "CAR")
abline(h = 0, lty = 2)
abline(v = 5, col = "red", lty = 2)
########################################################################
#      NULL:Is the CAR statistically significant from zero.          
########################################################################
ttest=t.test(event_combined$CAR)
########################################################################
#      Plotting Cumulative Abnormal Returns           
########################################################################
plot(event_combined$AR, type = "l", main = "Abnormal Returns",
     xlab = "Event Window", ylab = "AR")
abline(h = 0, lty = 2)
abline(v = 5, col = "red", lty = 2)


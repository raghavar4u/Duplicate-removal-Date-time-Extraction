
#############################################################################################################################################
#                      Alternative sloution for creating TestTimes data from Raw data & How to use it for further analysis
#############################################################################################################################################																		    
rm(list=ls())
###INIT Library ######
library(RODBC)

### ODBC Connection  ####

ODBCChannel  <- odbcDriverConnect('driver={SQL Server};server=XXXXX ;database=XXXX;trusted_connection=true')

################################   Data tables   ########################################
pdata <-sqlQuery(ODBCChannel, "SELECT  * From DATA.[TestPersons]") ### persons data (pdata)
rdata <-sqlQuery(ODBCChannel, "SELECT  * From DATA.[TestRaw]")     ### raw data     (rdata)
tdata <-sqlQuery(ODBCChannel, "SELECT  * From DATA.[TestTimes]")   ### timesdata    (tdata)

 ############################# Duplication removal ####################################
rdata= rdata[-1]  ## Removing the Access ID beacuse it has unique value  

nrow(rdata) ## 39257
table(!duplicated(rdata))  ### It is Providing the table of duplicated and non duplicated records 
##  FALSE  TRUE 
##  24361 14896
NDdata<-rdata[!duplicated(rdata),] ; nrow(NDdata) ### 14896 --Non Duplicate Data

NDdata$Date<-as.Date(NDdata$DT)                                        #### seperating Date from the DT Column 
NDdata$Time<-substr(as.character(NDdata$DT),12,20)                     #### seperating Time  from the DT Column and converting charcter to Times class

head(NDdata)
##  Emp_Code                  DT       Date     Time
##1        13 2015-01-01 08:45:00 2015-01-01 08:45:00
##2        12 2015-01-01 08:45:00 2015-01-01 08:45:00
##3        37 2015-01-01 08:50:00 2015-01-01 08:50:00
##8        25 2015-01-01 08:57:00 2015-01-01 08:57:00
##9        36 2015-01-01 08:58:00 2015-01-01 08:58:00
##10        7 2015-01-01 08:58:00 2015-01-01 08:58:00

mergedata<-merge(pdata,NDdata)  ### Merging the Persons data with the Nonduplicated data for eleminating the unwanted EMPPLOYEE ID existed in the RAW data
nrow(mergedata) ## 13156

Checkin<-subset(mergedata,Time<="08:00:00"|Time<="14:30:00",select=c("Emp_Code","Date","Time"))  ### Seperating Checkin time 
CheckOut<-subset(mergedata,Time>="12:30:00"|Time>="21:00:00",select=c("Emp_Code","Date","Time")) ### Seperating CheckOut time 

nrow(Checkin);head(Checkin)
nrow(CheckOut);head(CheckOut)

totaldata<-merge(Checkin,CheckOut,by=c("Emp_Code","Date"),all=T) ## Merging Both together with the Employee code and Date 
                                                                 ## When merging on the Date automatically missing values takeing the NA Values 
nrow(totaldata); ### 6999
colnames(totaldata)<-c("Emp_Code","Date","ChekinTime","CheckoutTime") 
> head(totaldata,20)
   Emp_Code       Date ChekinTime CheckoutTime
1         2 2015-01-01   09:43:00         <NA>
2         2 2015-01-02   09:23:00     18:18:00
3         2 2015-01-05   09:18:00     18:06:00
4         2 2015-01-06   09:38:00     18:52:00
5         2 2015-01-07   09:22:00         <NA>
6         2 2015-01-08   09:15:00     18:24:00
7         2 2015-01-09   10:07:00     19:02:00
8         2 2015-01-12   08:45:00     18:27:00
9         2 2015-01-13   09:00:00     18:33:00
10        2 2015-01-14   13:40:00     19:05:00
11        2 2015-01-14   13:40:00     13:40:00
12        2 2015-01-22   08:37:00     19:04:00
13        2 2015-01-23   09:10:00     18:15:00
14        2 2015-01-24   09:00:00     13:34:00
15        2 2015-01-24   13:34:00     13:34:00
16        2 2015-01-27   08:58:00     18:36:00
17        2 2015-01-28       <NA>     18:18:00
18        2 2015-01-29   09:19:00     18:56:00
19        2 2015-01-30   09:30:00         <NA>
20        2 2015-02-03       <NA>     18:59:00

### Looks fine but there are some pattrens in the data when employee punched repetedly same time on same date

74         2 2015-05-06   13:02:00     13:02:00
75         2 2015-05-06   13:02:00     18:06:00
78         2 2015-05-12   09:19:00     13:29:00
79         2 2015-05-12   13:29:00     13:29:00

Finaldata<-subset(totaldata,ChekinTime !=CheckoutTime) 

## It eliminates which objects repets the Same Time on the same date and Null values in either one 
           2 2015-01-01   09:43:00         <NA>
           2 2015-05-06   13:02:00     13:02:00

## If we want to analyse the Null vaules **totaldata**we can consider or with out null values **finaldata** we can consider
## CheckinTime Null values also we can analyse in this way :) 

nrow(Finaldata) ##6567
head(Finaldata,10)


intime<-strptime(Finaldata$ChekinTime,"%H:%M")          ## Converting intime with date for takeing differnce 
outtime<-strptime(Finaldata$CheckoutTime,"%H:%M")       ## Converting outtime with date for takeing differnce 
Finaldata$workingtime<-as.numeric(difftime(outtime,intime,units="mins"))-60 ## Differnce of the time excluding the lunch break 
Finaldata$Overtime<-Finaldata$workingtime-480           ## Over time Based on working time 


stdintime<-strptime("09:00:00","%H:%M")           ## Setting standard intime
stdouttime<-strptime("18:00:00","%H:%M")          ## Setting standard outtime

Finaldata$Lateby<-as.numeric(difftime(stdintime,intime,units="mins"))  ## Differnce of intime to std time
Finaldata$leavingafter<-as.numeric(difftime(outtime,stdouttime,units="mins")) ## Differnce of outtime to std time

head(Finaldata,10)
    Emp_Code       Date ChekinTime CheckoutTime workingtime Overtime Lateby leavingafter
2         2 2015-01-02   09:23:00     18:18:00         475       -5    -23           18
3         2 2015-01-05   09:18:00     18:06:00         468      -12    -18            6
4         2 2015-01-06   09:38:00     18:52:00         494       14    -38           52
6         2 2015-01-08   09:15:00     18:24:00         489        9    -15           24
7         2 2015-01-09   10:07:00     19:02:00         475       -5    -67           62
8         2 2015-01-12   08:45:00     18:27:00         522       42     15           27
9         2 2015-01-13   09:00:00     18:33:00         513       33      0           33
10        2 2015-01-14   13:40:00     19:05:00         265     -215   -280           65
12        2 2015-01-22   08:37:00     19:04:00         567       87     23           64
13        2 2015-01-23   09:10:00     18:15:00         485        5    -10           15


##Further has to work with this kind of data elimination I mean halfdays 
## If we dont want half days  we can give limits in the Checkin and checkout time only ---> Time<="08:00:00"|Time<="14:30:00"

## 10        2 2015-01-14   13:40:00     19:05:00         265     -215   -280           65

## Still has to apply filters ,this is just alternative approach to do in R (data splitting) 

#######################################################  The End #######################################################

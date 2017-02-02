#Read in consumer complaint data. 
#Original source: 
# https://catalog.data.gov/dataset/consumer-complaint-database#topic=consumer_navigation
consumer_complaints<-read.csv("Consumer_Complaints.csv",stringsAsFactors = FALSE)
head(consumer_complaints)
str(consumer_complaints)

library(dplyr)

min(consumer_complaints$Date.received)#1/1/12
max(consumer_complaints$Date.received)#9/9/16

#Next read in the equity capital information for the banks. The purpose is to 
#assign a size to the banks so that we can scale their complaint counts accordingly

#Source: http://www.usbanklocations.com/bank-rank/total-equity-capital.html?d=2016-09-30

Ranked_Banks_12_31_11<-read.csv('Ranked Banks 12-31-11.csv',stringsAsFactors=FALSE)
Ranked_Banks_3_31_12<-read.csv('Ranked Banks 3-31-12.csv',stringsAsFactors=FALSE)
Ranked_Banks_6_30_12<-read.csv('Ranked Banks 6-30-12.csv',stringsAsFactors=FALSE)
Ranked_Banks_9_30_12<-read.csv('Ranked Banks 9-30-12.csv',stringsAsFactors=FALSE)
Ranked_Banks_12_31_12<-read.csv('Ranked Banks 12-31-12.csv',stringsAsFactors=FALSE)
Ranked_Banks_3_31_13<-read.csv('Ranked Banks 3-31-13.csv',stringsAsFactors=FALSE)
Ranked_Banks_6_30_13<-read.csv('Ranked Banks 6-30-13.csv',stringsAsFactors=FALSE)
Ranked_Banks_9_30_13<-read.csv('Ranked Banks 9-30-13.csv',stringsAsFactors=FALSE)
Ranked_Banks_12_31_13<-read.csv('Ranked Banks 12-31-13.csv',stringsAsFactors=FALSE)
Ranked_Banks_3_31_14<-read.csv('Ranked Banks 3-31-14.csv',stringsAsFactors=FALSE)
Ranked_Banks_6_30_14<-read.csv('Ranked Banks 6-30-14.csv',stringsAsFactors=FALSE)
Ranked_Banks_9_30_14<-read.csv('Ranked Banks 9-30-14.csv',stringsAsFactors=FALSE)
Ranked_Banks_12_31_14<-read.csv('Ranked Banks 12-31-14.csv',stringsAsFactors=FALSE)
Ranked_Banks_3_31_15<-read.csv('Ranked Banks 3-31-15.csv',stringsAsFactors=FALSE)
Ranked_Banks_6_30_15<-read.csv('Ranked Banks 6-30-15.csv',stringsAsFactors=FALSE)
Ranked_Banks_9_30_15<-read.csv('Ranked Banks 9-30-15.csv',stringsAsFactors=FALSE)
Ranked_Banks_12_31_15<-read.csv('Ranked Banks 12-31-15.csv',stringsAsFactors=FALSE)
Ranked_Banks_3_31_16<-read.csv('Ranked Banks 3-31-16.csv',stringsAsFactors=FALSE)
Ranked_Banks_6_30_16<-read.csv('Ranked Banks 6-30-16.csv',stringsAsFactors=FALSE)
Ranked_Banks_9_30_16<-read.csv('Ranked Banks 9-30-16.csv',stringsAsFactors=FALSE)

#Stack the ranked banks into one set
all_ranked_banks <- rbind(Ranked_Banks_12_31_11,
                          Ranked_Banks_3_31_12,
                          Ranked_Banks_6_30_12,
                          Ranked_Banks_9_30_12,
                          Ranked_Banks_12_31_12,
                          Ranked_Banks_3_31_13,
                          Ranked_Banks_6_30_13,
                          Ranked_Banks_9_30_13,
                          Ranked_Banks_12_31_13,
                          Ranked_Banks_3_31_14,
                          Ranked_Banks_6_30_14,
                          Ranked_Banks_9_30_14,
                          Ranked_Banks_12_31_14,
                          Ranked_Banks_3_31_15,
                          Ranked_Banks_6_30_15,
                          Ranked_Banks_9_30_15,
                          Ranked_Banks_12_31_15,
                          Ranked_Banks_3_31_16,
                          Ranked_Banks_6_30_16,
                          Ranked_Banks_9_30_16)
rm(Ranked_Banks_12_31_11,
   Ranked_Banks_3_31_12,
   Ranked_Banks_6_30_12,
   Ranked_Banks_9_30_12,
   Ranked_Banks_12_31_12,
   Ranked_Banks_3_31_13,
   Ranked_Banks_6_30_13,
   Ranked_Banks_9_30_13,
   Ranked_Banks_12_31_13,
   Ranked_Banks_3_31_14,
   Ranked_Banks_6_30_14,
   Ranked_Banks_9_30_14,
   Ranked_Banks_12_31_14,
   Ranked_Banks_3_31_15,
   Ranked_Banks_6_30_15,
   Ranked_Banks_9_30_15,
   Ranked_Banks_12_31_15,
   Ranked_Banks_3_31_16,
   Ranked_Banks_6_30_16,
   Ranked_Banks_9_30_16)

#Reformat the dates
all_ranked_banks$Date2<-as.Date(all_ranked_banks$Date,format="%m/%d/%Y")
head(all_ranked_banks)
str(all_ranked_banks)
all_ranked_banks2<-subset(all_ranked_banks,select=-Date)
rm(all_ranked_banks)

#Extract unique bank names. Bank names will be merge keys. We want to experiment with
#some "fuzzy" merge techniques. 
unique_banks_from_complaints<-unique(consumer_complaints$Company)
length(unique_banks_from_complaints)#4060
unique_banks_from_complaints<-sort(unique_banks_from_complaints)
unique_banks_from_complaints[1:20]

unique_banks_from_ranked_banks<-unique(all_ranked_banks2$Bank.Name)
length(unique_banks_from_ranked_banks)#6722
unique_banks_from_ranked_banks<-sort(unique_banks_from_ranked_banks)
unique_banks_from_ranked_banks[1:20]

#Create the Cartesian product of the two sets of unique bank names. When we get that 
#done we'll find the Levenstein distances for all bank name pairs. 
library(dplyr)
huge_table<-full_join(unique_banks_from_complaints,unique_banks_from_ranked_banks,by=NULL)
#full_join isn't working. I get this error message: 
#Error in UseMethod("full_join") : 
#  no applicable method for 'full_join' applied to an object of class "character"



#Read in banks with numbers of branches
#Original source http://www.usbanklocations.com/bank-rank/number-of-branches.html
RankedBanksByNumBranch_12_31_11<-read.csv('RankedBanksByNumBranch 12-31-11.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_3_31_12<-read.csv('RankedBanksByNumBranch 3-31-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_6_30_12<-read.csv('RankedBanksByNumBranch 6-30-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_9_30_12<-read.csv('RankedBanksByNumBranch 9-30-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_12_31_12<-read.csv('RankedBanksByNumBranch 12-31-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_3_31_13<-read.csv('RankedBanksByNumBranch 3-31-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_6_30_13<-read.csv('RankedBanksByNumBranch 6-30-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_9_30_13<-read.csv('RankedBanksByNumBranch 9-30-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_12_31_13<-read.csv('RankedBanksByNumBranch 12-31-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_3_31_14<-read.csv('RankedBanksByNumBranch 3-31-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_6_30_14<-read.csv('RankedBanksByNumBranch 6-30-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_9_30_14<-read.csv('RankedBanksByNumBranch 9-30-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_12_31_14<-read.csv('RankedBanksByNumBranch 12-31-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_3_31_15<-read.csv('RankedBanksByNumBranch 3-31-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_6_30_15<-read.csv('RankedBanksByNumBranch 6-30-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_9_30_15<-read.csv('RankedBanksByNumBranch 9-30-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_12_31_15<-read.csv('RankedBanksByNumBranch 12-31-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_3_31_16<-read.csv('RankedBanksByNumBranch 3-31-16.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_6_30_16<-read.csv('RankedBanksByNumBranch 6-30-16.csv',stringsAsFactors=FALSE)
RankedBanksByNumBranch_9_30_16<-read.csv('RankedBanksByNumBranch 9-30-16.csv',stringsAsFactors=FALSE)
#Not working. Here's the error message: 
#Error in file(file, "rt") : cannot open the connection
#In addition: Warning message:
#  In file(file, "rt") :
#  cannot open file 'RankedBanksByNumBranch 9-30-16.csv': No such file or directory


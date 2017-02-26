#Read in consumer complaint data. 
#Original source: 
# https://catalog.data.gov/dataset/consumer-complaint-database#topic=consumer_navigation
setwd("C:/Users/Owner/Documents")

consumer_complaints<-read.csv("Consumer_Complaints.csv",stringsAsFactors = FALSE)
head(consumer_complaints)
str(consumer_complaints)

library(dplyr)

min(consumer_complaints$Date.received)#min date is 1/1/12
max(consumer_complaints$Date.received)#max date is 9/9/16

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

write.csv(all_ranked_banks2, file = "banks_ranked_by_equity_capital.csv")


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

#Stack the ranked (by number of branches) banks into one set
all_RankedBanksByNumBranch <- rbind(RankedBanksByNumBranch_12_31_11,
                                    RankedBanksByNumBranch_3_31_12,
                                    RankedBanksByNumBranch_6_30_12,
                                    RankedBanksByNumBranch_9_30_12,
                                    RankedBanksByNumBranch_12_31_12,
                                    RankedBanksByNumBranch_3_31_13,
                                    RankedBanksByNumBranch_6_30_13,
                                    RankedBanksByNumBranch_9_30_13,
                                    RankedBanksByNumBranch_12_31_13,
                                    RankedBanksByNumBranch_3_31_14,
                                    RankedBanksByNumBranch_6_30_14,
                                    RankedBanksByNumBranch_9_30_14,
                                    RankedBanksByNumBranch_12_31_14,
                                    RankedBanksByNumBranch_3_31_15,
                                    RankedBanksByNumBranch_6_30_15,
                                    RankedBanksByNumBranch_9_30_15,
                                    RankedBanksByNumBranch_12_31_15,
                                    RankedBanksByNumBranch_3_31_16,
                                    RankedBanksByNumBranch_6_30_16,
                                    RankedBanksByNumBranch_9_30_16)
rm(RankedBanksByNumBranch_12_31_11,
   RankedBanksByNumBranch_3_31_12,
   RankedBanksByNumBranch_6_30_12,
   RankedBanksByNumBranch_9_30_12,
   RankedBanksByNumBranch_12_31_12,
   RankedBanksByNumBranch_3_31_13,
   RankedBanksByNumBranch_6_30_13,
   RankedBanksByNumBranch_9_30_13,
   RankedBanksByNumBranch_12_31_13,
   RankedBanksByNumBranch_3_31_14,
   RankedBanksByNumBranch_6_30_14,
   RankedBanksByNumBranch_9_30_14,
   RankedBanksByNumBranch_12_31_14,
   RankedBanksByNumBranch_3_31_15,
   RankedBanksByNumBranch_6_30_15,
   RankedBanksByNumBranch_9_30_15,
   RankedBanksByNumBranch_12_31_15,
   RankedBanksByNumBranch_3_31_16,
   RankedBanksByNumBranch_6_30_16,
   RankedBanksByNumBranch_9_30_16)

#Reformat the dates
all_RankedBanksByNumBranch$Date2<-as.Date(all_RankedBanksByNumBranch$Date,format="%m/%d/%Y")
head(all_RankedBanksByNumBranch)
str(all_RankedBanksByNumBranch)
all_RankedBanksByNumBranch2<-subset(all_RankedBanksByNumBranch,select=-Date)
rm(all_RankedBanksByNumBranch)

install.packages("stringdist")
library(stringdist)

#We will compare every bank name in the set of banks from the complaints data to the bank names in the
#set of bank sizes. We will keep every pair of bank names that is "close", that is, every 
#pair of bank names with string distance <= threshold. 

distances <-data.frame(bank1=character(),bank2=character(),cleanedUpBank1=character(),
                       cleanedUpBank2=character(), d=integer(),stringsAsFactors = FALSE)
threshold<-0.2
for (i in 1:length(unique_banks_from_complaints)){
  for (j in 1:length(unique_banks_from_ranked_banks)){
#Here we remove words from the bank names that give no information about the bank's identity. 
#These words are sometimes called "stop words"
    CleanedUpBankName1<-gsub(' BANK','',toupper(unique_banks_from_complaints[i]))
    CleanedUpBankName1<-gsub(' INC.','',CleanedUpBankName1)
    CleanedUpBankName1<-gsub(' BANCO','',CleanedUpBankName1)
    CleanedUpBankName1<-gsub(' FINANCIAL','',CleanedUpBankName1)
    CleanedUpBankName1<-gsub(' FINANCE','',CleanedUpBankName1)
    CleanedUpBankName1<-gsub(' CAPITAL','',CleanedUpBankName1) 
    CleanedUpBankName1<-gsub(' SERVICES','',CleanedUpBankName1)
    CleanedUpBankName2<-gsub(' BANK','',toupper(unique_banks_from_ranked_banks[j]))
    CleanedUpBankName2<-gsub(' INC.','',CleanedUpBankName2)
    CleanedUpBankName2<-gsub(' BANCO','',CleanedUpBankName2)
    CleanedUpBankName2<-gsub(' FINANCIAL','',CleanedUpBankName2)
    CleanedUpBankName2<-gsub(' FINANCE','',CleanedUpBankName2)
    CleanedUpBankName2<-gsub(' CAPITAL','',CleanedUpBankName2)
    CleanedUpBankName2<-gsub(' SERVICES','',CleanedUpBankName2)
  
  #Distance between 2 bank names is defined as longest common substring distance, 
  #normalized by length of cleaned up bank name
    dist<-stringdist(CleanedUpBankName1,CleanedUpBankName2,method="lcs")/nchar(CleanedUpBankName1)


    if (dist <= threshold){
      newrow<-data.frame(bank1=unique_banks_from_complaints[i],bank2=unique_banks_from_ranked_banks[j],
                       cleanedUpBank1=CleanedUpBankName1,cleanedUpBank2=CleanedUpBankName2,d=dist, 
                       stringsAsFactors = FALSE)
      distances<-rbind(distances,newrow)
      }
  }
}
#There were only about 100 pairs of matched names out of 4060 unique bank names from the complaints data.
#This suggest to me that the two sets of bank names actually have a very small intersection. 

#take a random sample of size 50000 from a dataset mydata 
#sample without replacement
consumer_complaints_samp50000 <- consumer_complaints[sample(1:nrow(consumer_complaints), 
                                                     50000, replace=FALSE),]
write.csv(consumer_complaints_samp50000, file = "consumer_complaints_samp50000.csv")

#Read in consumer complaint data. 
#Original source: 
# https://catalog.data.gov/dataset/consumer-complaint-database#topic=consumer_navigation
setwd("C:/Users/Owner/Documents")

consumer_complaints<-read.csv("Consumer_Complaints.csv",stringsAsFactors = FALSE)
head(consumer_complaints)
str(consumer_complaints)

#Use Date.received to get year and quarter of complaint

consumer_complaints$complaintQtrYr<-paste(quarters(as.Date(consumer_complaints$Date.received,format="%m/%d/%Y")),
                                    substr(consumer_complaints$Date.received,nchar(consumer_complaints$Date.received)-3,
                                           nchar(consumer_complaints$Date.received)),
                                    sep=" ")

consumer_complaints[10000:10050,c("Date.received","complaintQtrYr")]
#consumer_complaints_banksOnly<-consumer_complaints[consumer_complaints$Product %in% 
#                                                 c("Consumer Loan","Bank account or service"),]
#str(consumer_complaints_banksOnly)
#108,764 obs

library(dplyr)

#min(consumer_complaints_banksOnly$Date.received)
#min date is 1/1/13
#max(consumer_complaints_banksOnly$Date.received)
#max date is 9/9/16

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

#Get year and quarter from the dates
all_ranked_banks$quarterYear<-paste(quarters(as.Date(all_ranked_banks$Date,format="%m/%d/%Y")),
                                    substr(all_ranked_banks$Date,nchar(all_ranked_banks$Date)-3,nchar(all_ranked_banks$Date)),
                                    sep=" ")
head(all_ranked_banks)

all_ranked_banks<-subset(all_ranked_banks,select=-Date)

str(all_ranked_banks)#133880 obs
write.csv(all_ranked_banks, file = "banks_ranked_by_equity_capital.csv")
max(all_ranked_banks$Rank) #7366

maxrank<-50

all_ranked_banks_top50<-all_ranked_banks[all_ranked_banks$Rank<=maxrank,]
str(all_ranked_banks_top50)
head(all_ranked_banks_top50)


#Extract unique bank names. Bank names will be merge keys. We want to experiment with
#some "fuzzy" merge techniques. 
unique_banks_from_complaints<-unique(consumer_complaints_banksOnly$Company)
length(unique_banks_from_complaints)#1130
unique_banks_from_complaints[1:20]
write.csv(unique_banks_from_complaints,file="unique_banks_from_complaints.csv")

unique_banks_from_ranked_banks_top50<-unique(all_ranked_banks_top50$Bank.Name)
length(unique_banks_from_ranked_banks_top50)#67 banks
unique_banks_from_ranked_banks_top50
write.csv(unique_banks_from_ranked_banks_top50,file="top 50 ranked banks by equity capital.csv")

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

###########################################################################################################################################
# Here we standardize the bank names; that is, we 
#    1. Take the banks that ever achieved a rank in the top 50
#    2. Tweak those bank names to match the names of the banks in the complaints data
###########################################################################################################################################
for (i in 1:nrow(all_ranked_banks_top50))
{ all_ranked_banks_top50$Bank.Name_std[i]<-''
  if (all_ranked_banks_top50$Bank.Name[i]=='Ally Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Ally Financial Inc.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Banco Popular de Puerto Rico'){all_ranked_banks_top50$Bank.Name_std[i]<-'Banco Popular de Puerto Rico'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Bank of America'){all_ranked_banks_top50$Bank.Name_std[i]<-'Bank of America'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Bank of the West'){all_ranked_banks_top50$Bank.Name_std[i]<-'Bank of the West'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='BMO Harris Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'BMO Harris'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='The Bank of New York Mellon'){all_ranked_banks_top50$Bank.Name_std[i]<-'BNY Mellon'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Capital One'){all_ranked_banks_top50$Bank.Name_std[i]<-'Capital One'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Capital One Bank (USA)'){all_ranked_banks_top50$Bank.Name_std[i]<-'Capital One'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Charles Schwab Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Charles Schwab Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='CIT BANK'){all_ranked_banks_top50$Bank.Name_std[i]<-'CIT Bank National Association'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='CIT Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'CIT Bank National Association'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Citibank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Citibank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Comerica Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Comerica'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Deutsche Bank Trust Company Americas'){all_ranked_banks_top50$Bank.Name_std[i]<-'Deutsche Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Discover Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Discover'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='E*TRADE Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'E*Trade Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Fifth Third Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Fifth Third Financial Corporation'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='First Niagara Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'First Niagara Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='First Republic Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'First Republic Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Goldman Sachs Bank USA'){all_ranked_banks_top50$Bank.Name_std[i]<-'Goldman Sachs Bank USA'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='HSBC Bank USA'){all_ranked_banks_top50$Bank.Name_std[i]<-'HSBC North America Holdings Inc.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='JPMorgan Chase Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'JPMorgan Chase & Co.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Chase Bank USA'){all_ranked_banks_top50$Bank.Name_std[i]<-'JPMorgan Chase & Co.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Morgan Stanley Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Morgan Stanley'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='New York Community Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'New York Community Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Pacific Western Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'PACIFIC WESTERN BANK'}
  else if (all_ranked_banks_top50$Bank.Name[i]=="People's United Bank"){all_ranked_banks_top50$Bank.Name_std[i]<-"People's United Bank"}
  else if (all_ranked_banks_top50$Bank.Name[i]=='PNC Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'PNC Bank N.A.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Regions Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Regions Financial Corporation'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Santander Bank, N.A.'){all_ranked_banks_top50$Bank.Name_std[i]<-'Santander Bank US'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='State Street Bank and Trust Company'){all_ranked_banks_top50$Bank.Name_std[i]<-'State Street Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='SunTrust Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'SunTrust Banks, Inc.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Synchrony Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Synchrony Financial'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Synovus Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Synovus Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='TD Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'TD Bank US Holding Company'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='The Huntington National Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'The Huntington National Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='The Northern Trust Company'){all_ranked_banks_top50$Bank.Name_std[i]<-'The Northern Trust Company'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='UBS Bank USA'){all_ranked_banks_top50$Bank.Name_std[i]<-'UBS Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Umpqua Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Umpqua Holdings Corporation'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Union Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Union Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='USAA Savings Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'USAA Savings'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Wells Fargo Bank'){all_ranked_banks_top50$Bank.Name_std[i]<-'Wells Fargo & Company'}
}

write.csv(all_ranked_banks_top50,file="Bank names with standardized names.csv")

#Keep only the bank names from the ranked banks that have matches in the complaints data
all_ranked_banks_top50<-all_ranked_banks_top50[all_ranked_banks2_top$Bank.Name_std!='',]

#Next we have to merge the ranked banks data onto the complaints data by bank name and quarter





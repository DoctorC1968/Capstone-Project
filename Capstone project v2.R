#Read in consumer complaint data. 
#Original source: 
# https://catalog.data.gov/dataset/consumer-complaint-database#topic=consumer_navigation
setwd("C:/Users/Owner/Documents")

consumer_complaints<-read.csv("Consumer_Complaints.csv",stringsAsFactors = FALSE)
head(consumer_complaints)
str(consumer_complaints)
max(as.Date(consumer_complaints$Date.received,format="%m/%d/%Y"))

#Use Date.received to get year and quarter of complaint

consumer_complaints$quarterYear<-paste(quarters(as.Date(consumer_complaints$Date.received,format="%m/%d/%Y")),
                                    substr(consumer_complaints$Date.received,nchar(consumer_complaints$Date.received)-3,
                                           nchar(consumer_complaints$Date.received)),
                                    sep=" ")

#Remove records from first quarter of 2017 since this quarter isn't finished yet
consumer_complaints<-consumer_complaints[consumer_complaints$quarterYear!="Q1 2017",]

#Find frequencies of products that got complaints so that we known which products to 
#prioritize in our investigation 

Product_freq<-table(consumer_complaints$Product)
str(Product_freq)
print(Product_freq)
write.csv(Product_freq,file="Product_freq.csv")

#We will first focus on the product "Mortgage" since it has the most complaints
#After that I can try :
#Debt collection
#Credit reporting
#Credit card
#Bank account or service
#Consumer Loan
#Student loan

library(dplyr)

#Count the number of mortgage-related consumer complaints by counting up the number of distinct
#mortgage complaint ids per bank. Note that this method will only capture NONZERO complaint counts,
#because if there are no complaints then there will be no complaint ids. 

consumer_complaints %>%
  filter(Product == "Mortgage") %>%
  group_by(Company,quarterYear) %>%
  summarise(complaintCount=n_distinct(Complaint.ID))-> consumer_complaints_mortgage

str(consumer_complaints_mortgage)
summary(consumer_complaints_mortgage$complaintCount)

#For some banks and quarters, there are zero complaints. But these bank/quarter
#combinations are missing from the data. We have to put in the zero complaint counts ourselves.
#This is done below. 

bankNames<-unique(consumer_complaints_mortgage$Company)
quarterYears<-sort(unique(consumer_complaints_mortgage$quarterYear))
length(bankNames)
length(quarterYears)

for (Company in bankNames) {
  for (quarterYear in quarterYears) {
    newrow<-consumer_complaints_mortgage[1,]
    newrow$Company<-Company
    newrow$quarterYear<-quarterYear
    newrow$complaintCount<-0
    consumer_complaints_mortgage<-rbind(consumer_complaints_mortgage,newrow)
  }
}

#Compute complaint counts per company/quarter, including complaint counts of zero
consumer_complaints_mortgage %>%
  group_by(Company,quarterYear) %>%
  summarise(complaintCount=max(complaintCount,na.rm=TRUE))->consumer_complaints_mortgage2

#Make sure each bank has the same number of year/quarters:
consumer_complaints_mortgage2 %>%
  group_by(Company) %>%
  summarise(numquarters=n_distinct(quarterYear))->quarterCounts
summary(quarterCounts)
#Good, all companies have 21 quarters

summary(consumer_complaints_mortgage2$complaintCount)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000    0.000    0.000    8.583    0.000 4942.000 
#There are a lot of zero complaint counts, but that's because there are a lot of small banks

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
sort(unique(all_ranked_banks$quarterYear))
all_ranked_banks<-subset(all_ranked_banks,select=-Date)

str(all_ranked_banks)#133880 obs
write.csv(all_ranked_banks, file = "banks_ranked_by_equity_capital.csv")
max(all_ranked_banks$Rank) #7366

#Consider only the banks that had a rank of 50 or less at some time. 
maxrank<-50

all_ranked_banks %>%
  group_by(Bank.Name) %>%
  summarise(minrank=min(Rank))-> BanksMinRanks
head(BanksMinRanks)

BanksMinRanks %>% 
  filter(minrank <= maxrank) -> all_ranked_banks_names_top50
str(all_ranked_banks_names_top50) #67 obs

all_ranked_banks_top50 <-inner_join(all_ranked_banks,
                                    subset(all_ranked_banks_names_top50,select=c("Bank.Name")),
                         by=c("Bank.Name"))

str(all_ranked_banks_top50)#1638 obs. 
head(all_ranked_banks_top50)

#Next we make sure that each bank has exactly one rank in each quarter: 
myTable2<-table(all_ranked_banks_top50$Bank.Name,all_ranked_banks_top50$quarterYear)
write.csv(myTable2,file="myTable2.csv")
#Some banks have more than 1 rank in certain quarters

#Now I'm double-checking what I did above: 
all_ranked_banks_top50 %>%
  group_by(quarterYear,Bank.Name) %>%
  summarise(numranks=n_distinct(Rank))->numRanks
summary(numRanks$numranks)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00    1.00    1.00    1.39    1.00   20.00 
#Again, some banks have too many ranks in a quarter

#Extract unique bank names. Bank names will be merge keys. We will manually match up bank names from the complaints
#data with bank names from the ranked banks data
unique_banks_from_complaints<-unique(consumer_complaints$Company)
length(unique_banks_from_complaints)#4054

unique_banks_from_complaints[1:20]
write.csv(unique_banks_from_complaints,file="unique_banks_from_complaints.csv")

unique_banks_from_ranked_banks_top50<-unique(all_ranked_banks_top50$Bank.Name)
length(unique_banks_from_ranked_banks_top50)#67 banks
unique_banks_from_ranked_banks_top50
write.csv(unique_banks_from_ranked_banks_top50,file="top 50 ranked banks by equity capital.csv")

#Read in banks with numbers of branches
#This is an alternate way of measuring bank sizes. 
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

#Reformat the dates in data with number of branches
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
{ all_ranked_banks_top50$Company[i]<-''
  if (all_ranked_banks_top50$Bank.Name[i]=='Ally Bank'){all_ranked_banks_top50$Company[i]<-'Ally Financial Inc.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Banco Popular de Puerto Rico'){all_ranked_banks_top50$Company[i]<-'Banco Popular de Puerto Rico'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Bank of America'){all_ranked_banks_top50$Company[i]<-'Bank of America'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Bank of the West'){all_ranked_banks_top50$Company[i]<-'Bank of the West'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='BMO Harris Bank'){all_ranked_banks_top50$Company[i]<-'BMO Harris'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='The Bank of New York Mellon'){all_ranked_banks_top50$Company[i]<-'BNY Mellon'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Capital One'){all_ranked_banks_top50$Company[i]<-'Capital One'}
  #else if (all_ranked_banks_top50$Bank.Name[i]=='Capital One Bank (USA)'){all_ranked_banks_top50$Company[i]<-'Capital One'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Charles Schwab Bank'){all_ranked_banks_top50$Company[i]<-'Charles Schwab Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='CIT BANK'){all_ranked_banks_top50$Company[i]<-'CIT Bank National Association'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='CIT Bank'){all_ranked_banks_top50$Company[i]<-'CIT Bank National Association'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Citibank'){all_ranked_banks_top50$Company[i]<-'Citibank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Comerica Bank'){all_ranked_banks_top50$Company[i]<-'Comerica'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Deutsche Bank Trust Company Americas'){all_ranked_banks_top50$Company[i]<-'Deutsche Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Discover Bank'){all_ranked_banks_top50$Company[i]<-'Discover'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='E*TRADE Bank'){all_ranked_banks_top50$Company[i]<-'E*Trade Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Fifth Third Bank'){all_ranked_banks_top50$Company[i]<-'Fifth Third Financial Corporation'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='First Niagara Bank'){all_ranked_banks_top50$Company[i]<-'First Niagara Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='First Republic Bank'){all_ranked_banks_top50$Company[i]<-'First Republic Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Goldman Sachs Bank USA'){all_ranked_banks_top50$Company[i]<-'Goldman Sachs Bank USA'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='HSBC Bank USA'){all_ranked_banks_top50$Company[i]<-'HSBC North America Holdings Inc.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='JPMorgan Chase Bank'){all_ranked_banks_top50$Company[i]<-'JPMorgan Chase & Co.'}
  #else if (all_ranked_banks_top50$Bank.Name[i]=='Chase Bank USA'){all_ranked_banks_top50$Company[i]<-'JPMorgan Chase & Co.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Morgan Stanley Bank'){all_ranked_banks_top50$Company[i]<-'Morgan Stanley'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='New York Community Bank'){all_ranked_banks_top50$Company[i]<-'New York Community Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Pacific Western Bank'){all_ranked_banks_top50$Company[i]<-'PACIFIC WESTERN BANK'}
  else if (all_ranked_banks_top50$Bank.Name[i]=="People's United Bank"){all_ranked_banks_top50$Company[i]<-"People's United Bank"}
  else if (all_ranked_banks_top50$Bank.Name[i]=='PNC Bank'){all_ranked_banks_top50$Company[i]<-'PNC Bank N.A.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Regions Bank'){all_ranked_banks_top50$Company[i]<-'Regions Financial Corporation'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Santander Bank, N.A.'){all_ranked_banks_top50$Company[i]<-'Santander Bank US'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='State Street Bank and Trust Company'){all_ranked_banks_top50$Company[i]<-'State Street Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='SunTrust Bank'){all_ranked_banks_top50$Company[i]<-'SunTrust Banks, Inc.'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Synchrony Bank'){all_ranked_banks_top50$Company[i]<-'Synchrony Financial'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Synovus Bank'){all_ranked_banks_top50$Company[i]<-'Synovus Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='TD Bank'){all_ranked_banks_top50$Company[i]<-'TD Bank US Holding Company'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='The Huntington National Bank'){all_ranked_banks_top50$Company[i]<-'The Huntington National Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='The Northern Trust Company'){all_ranked_banks_top50$Company[i]<-'The Northern Trust Company'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='UBS Bank USA'){all_ranked_banks_top50$Company[i]<-'UBS Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Umpqua Bank'){all_ranked_banks_top50$Company[i]<-'Umpqua Holdings Corporation'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Union Bank'){all_ranked_banks_top50$Company[i]<-'Union Bank'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='USAA Savings Bank'){all_ranked_banks_top50$Company[i]<-'USAA Savings'}
  else if (all_ranked_banks_top50$Bank.Name[i]=='Wells Fargo Bank'){all_ranked_banks_top50$Company[i]<-'Wells Fargo & Company'}
}

#Keep only the bank names from the ranked banks that have matches in the complaints data
all_ranked_banks_top50<-all_ranked_banks_top50[all_ranked_banks_top50$Company!='',]
write.csv(all_ranked_banks_top50,file="Bank names with standardized names.csv")

#Next we have to merge the ranked banks data onto the complaints data by bank name and quarter

mergedBanks<-inner_join(consumer_complaints_mortgage2,
                        subset(all_ranked_banks_top50,select=c("Total.Equity.Capital","quarterYear","Company")),
                        by=c("Company","quarterYear"))
str(mergedBanks)#819 obs
write.csv(mergedBanks,file="mergedBanks.csv")

#Explore the distributions of equity capital and the log of equity capital

hist(all_ranked_banks_top50[all_ranked_banks_top50$quarterYear=="Q1 2015",]$Total.Equity.Capital,
     breaks=20,xlab="Total Equity Capital for all banks combined",main="Q1 2015")

mergedBanks$logTotEqCap<-log(mergedBanks$Total.Equity.Capital)
all_ranked_banks_top50$logTotEqCap<-log(all_ranked_banks_top50$Total.Equity.Capital)

hist(all_ranked_banks_top50[all_ranked_banks_top50$quarterYear=="Q1 2015",]$logTotEqCap,
     breaks=8,xlab="Log Total Equity Capital for all banks combined",main="Q1 2015")
hist(all_ranked_banks_top50[all_ranked_banks_top50$quarterYear=="Q2 2015",]$logTotEqCap,
     breaks=8,xlab="Log Total Equity Capital for all banks combined",main="Q2 2015")
hist(all_ranked_banks_top50[all_ranked_banks_top50$quarterYear=="Q3 2015",]$logTotEqCap,
     breaks=8,xlab="Log Total Equity Capital for all banks combined",main="Q3 2015")
hist(all_ranked_banks_top50[all_ranked_banks_top50$quarterYear=="Q4 2015",]$logTotEqCap,
     breaks=8,xlab="Log Total Equity Capital for all banks combined",main="Q4 2015")
hist(all_ranked_banks_top50[all_ranked_banks_top50$quarterYear=="Q1 2016",]$logTotEqCap,
     breaks=8,xlab="Log Total Equity Capital for all banks combined",main="Q1 2016")
#Judging from the gaps in the histograms above, I think that I was using too many 
#breaks. I'll try 4 breaks instead of 8

hist(mergedBanks[mergedBanks$quarterYear=="Q1 2015",]$logTotEqCap,
     breaks=4,main="Histo. of Log total equity capital in Q1 2015",xlab="log total equity cap")
hist(mergedBanks[mergedBanks$quarterYear=="Q2 2015",]$logTotEqCap,
     breaks=4)
hist(mergedBanks[mergedBanks$quarterYear=="Q3 2015",]$logTotEqCap,
     breaks=4)
hist(mergedBanks[mergedBanks$quarterYear=="Q4 2015",]$logTotEqCap,
     breaks=4)
hist(mergedBanks[mergedBanks$quarterYear=="Q1 2016",]$logTotEqCap,
     breaks=4)
#Equity capital is skewed to the right. 
#Log of equity capital is more symmetric, but possibly skewed left

#Next: transform units of equity capital from dollars to billions of dollars
mergedBanks$Total.Equity.Capital2<-mergedBanks$Total.Equity.Capital/10^9
range(mergedBanks$Total.Equity.Capital2)
#range: 0.008103 to 211.586000
write.csv(mergedBanks,file="mergedBanks.csv")

#Make sure that equity capital is at the level bank*quarter*year
mergedBanks %>%
  group_by(Company,quarterYear) %>%
  summarise(distinctEqCap=n_distinct(Total.Equity.Capital)) ->
  NumEqCapsPerBankQtr
summary(NumEqCapsPerBankQtr)
#No, a few banks have more than one value of equity capital in one quarter

#Get the AVERAGE complaint counts per bank
mergedBanks %>%
  group_by(Company) %>%
  summarise(MeanEquityCapital2=mean(Total.Equity.Capital2,na.rm=TRUE),
            MeanEquityCapital=mean(Total.Equity.Capital,na.rm=TRUE),
            MeanComplaintCount=mean(complaintCount,na.rm=TRUE))->
  MeanComplaintCountPerBank
write.csv(MeanComplaintCountPerBank,file="MeanComplaintCountPerBank.csv")
summary(MeanComplaintCountPerBank$MeanComplaintCount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.10    1.95    7.26  143.60   43.03 1902.00 
sort(MeanComplaintCountPerBank$MeanComplaintCount)

#Find relationship between equity capital per quarter and mean complaint count per quarter
plot.new()
plot(MeanComplaintCountPerBank$MeanEquityCapital2,MeanComplaintCountPerBank$MeanComplaintCount,
     xlab="Mean Equity Cap (billions of $)",ylab="Mean quarterly complaint count",
     main="Mean quarterly complaint count vs. Mean equity capital")
abline(lm(MeanComplaintCountPerBank$MeanComplaintCount~MeanComplaintCountPerBank$MeanEquityCapital2), 
       col="red")

MortgageModel1<-lm(MeanComplaintCountPerBank$MeanComplaintCount~
                     MeanComplaintCountPerBank$MeanEquityCapital2)
MortgageModel1$coefficients
summary(MortgageModel1)
cor(MeanComplaintCountPerBank$MeanEquityCapital2,MeanComplaintCountPerBank$MeanComplaintCount)
#Call:
#  lm(formula = MeanComplaintCountPerBank$MeanComplaintCount ~ MeanComplaintCountPerBank$MeanEquityCapital2)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-634.23  -30.65   11.27   32.59  567.62 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                  -55.9076    32.6436  -1.713   0.0954 .  
#MeanComplaintCountPerBank$MeanEquityCapital2   7.3307     0.5936  12.349 1.67e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 174.9 on 36 degrees of freedom
#Multiple R-squared:  0.809,	Adjusted R-squared:  0.8037 
#F-statistic: 152.5 on 1 and 36 DF,  p-value: 1.666e-14

#> cor(MeanComplaintCountPerBank$MeanEquityCapital2,MeanComplaintCountPerBank$MeanComplaintCount)
#[1] 0.8994597

#So there is a strong, positive, linear relationship between quarterly equity capital and average 
#complaints per quarter (though the estimate of the intercept of the regression line doesn't have
#as low a p-value as I'd like)


#Normalize complaint counts to remove effect of bank size (as measured by equity capital)
#on counts.

str(mergedBanks)
mergedBanks$adjComplaintCount<-(mergedBanks$complaintCount-MortgageModel1$coefficients[1])/
                                mergedBanks$Total.Equity.Capital2
sort(mergedBanks$adjComplaintCount)
summary(mergedBanks$adjComplaintCount)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.833    5.286    8.628  279.700   16.650 7722.000 
write.csv(mergedBanks,file="mergedBanks.csv")
cor(mergedBanks$adjComplaintCount,mergedBanks$Total.Equity.Capital2)
#cor= -0.1670008
#Looks like we've removed the correlation between complaint count and equity capital
MortgageModel2<-lm(mergedBanks$adjComplaintCount~mergedBanks$Total.Equity.Capital2)
summary(MortgageModel2)
#Call:
#  lm(formula = mergedBanks$adjComplaintCount ~ mergedBanks$Total.Equity.Capital2)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-332.8 -328.7 -309.0 -246.5 7362.7 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                       359.3399    34.5676  10.395  < 2e-16 ***
#  mergedBanks$Total.Equity.Capital2  -3.1455     0.6497  -4.841 1.54e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 870.2 on 817 degrees of freedom
#Multiple R-squared:  0.02789,	Adjusted R-squared:  0.0267 
#F-statistic: 23.44 on 1 and 817 DF,  p-value: 1.542e-06

#R^2 is practically zero. So hardly any variation in adjusted complaint count is due to
#the linear relationship between adjusted complaint count and equity capital. 
#between quarterly equity capital and quarterly average complaint counts. 

#Check distribution of adjusted complaint count for ALL banks combined
hist(mergedBanks$adjComplaintCount,breaks=16,main="Histogram of adj. complaint count",
     xlab="Adjusted complaint count")
#Could be a Poisson distribution with lambda close to zero.  

#Compute average equity capital (taken over all quarters) for each bank
mergedBanks %>%
  group_by(Company) %>%
  summarise(meanEqCap=mean(Total.Equity.Capital2))->MeanEqCapitalByBank
write.csv(MeanEqCapitalByBank,file="MeanEqCapitalByBank.csv")

#Now find distributions of adjusted complaint counts for each bank separately, so we can 
#see if the distributions look like Poisson. 
bankNames<-unique(mergedBanks$Company)
plot.new()
for (bankName in bankNames){
  hist(mergedBanks[mergedBanks$Company==bankName,]$adjComplaintCount,
       main=bankName,xlab="Adj. complaint count")
}
#For some of the banks, the distributions of adjusted quarterly claim counts look 
#Poisson. For other banks, the distributions look nothing like Poisson distributions. 
#The latter observation could be due to insufficient data. We could try to remedy this
#by collecting the past 10 years of quarterly data instead of just the past 5 years;
#but this would only work if the parameter (lambda) for each bank remained constant over
#the past 10 years. This does not seem like a reasonable assumption. 

#Here we attempt to perform the chi-square goodness of fit test to see if the quarterly
#adjusted complaint counts (taken for each bank separately) have a Poisson distribution. 
#In case of count data we can use goodfit() included in vcd package. Unfortunately, 
#the "vcd" package seems to be missing. 
install.packages(vcd)
library(vcd)
#Error in install.packages : object 'vcd' not found
gf <- goodfit(x.poi,type= "poisson",method= "MinChisq")
summary(gf)
plot(gf,main="Count data vs Poisson distribution")



#################################################################################
# Load numbers of employees
# We will try number of employees per bank, normalized by bank size, to predict
# number of quarterly complaints
#################################################################################

RankedBanksByNumEmpls_12_31_11<-read.csv('RankedBanksByNumEmpls 12-31-11.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_3_31_12<-read.csv('RankedBanksByNumEmpls 3-31-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_6_30_12<-read.csv('RankedBanksByNumEmpls 6-30-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_9_30_12<-read.csv('RankedBanksByNumEmpls 9-30-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_12_31_12<-read.csv('RankedBanksByNumEmpls 12-31-12.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_3_31_13<-read.csv('RankedBanksByNumEmpls 3-31-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_6_30_13<-read.csv('RankedBanksByNumEmpls 6-30-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_9_30_13<-read.csv('RankedBanksByNumEmpls 9-30-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_12_31_13<-read.csv('RankedBanksByNumEmpls 12-31-13.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_3_31_14<-read.csv('RankedBanksByNumEmpls 3-31-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_6_30_14<-read.csv('RankedBanksByNumEmpls 6-30-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_9_30_14<-read.csv('RankedBanksByNumEmpls 9-30-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_12_31_14<-read.csv('RankedBanksByNumEmpls 12-31-14.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_3_31_15<-read.csv('RankedBanksByNumEmpls 3-31-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_6_30_15<-read.csv('RankedBanksByNumEmpls 6-30-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_9_30_15<-read.csv('RankedBanksByNumEmpls 9-30-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_12_31_15<-read.csv('RankedBanksByNumEmpls 12-31-15.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_3_31_16<-read.csv('RankedBanksByNumEmpls 3-31-16.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_6_30_16<-read.csv('RankedBanksByNumEmpls 6-30-16.csv',stringsAsFactors=FALSE)
RankedBanksByNumEmpls_9_30_16<-read.csv('RankedBanksByNumEmpls 9-30-16.csv',stringsAsFactors=FALSE)

#Stack the ranked banks into one set
all_ranked_banksByNumEmpls <- rbind(RankedBanksByNumEmpls_12_31_11,
                          RankedBanksByNumEmpls_3_31_12,
                          RankedBanksByNumEmpls_6_30_12,
                          RankedBanksByNumEmpls_9_30_12,
                          RankedBanksByNumEmpls_12_31_12,
                          RankedBanksByNumEmpls_3_31_13,
                          RankedBanksByNumEmpls_6_30_13,
                          RankedBanksByNumEmpls_9_30_13,
                          RankedBanksByNumEmpls_12_31_13,
                          RankedBanksByNumEmpls_3_31_14,
                          RankedBanksByNumEmpls_6_30_14,
                          RankedBanksByNumEmpls_9_30_14,
                          RankedBanksByNumEmpls_12_31_14,
                          RankedBanksByNumEmpls_3_31_15,
                          RankedBanksByNumEmpls_6_30_15,
                          RankedBanksByNumEmpls_9_30_15,
                          RankedBanksByNumEmpls_12_31_15,
                          RankedBanksByNumEmpls_3_31_16,
                          RankedBanksByNumEmpls_6_30_16,
                          RankedBanksByNumEmpls_9_30_16)

rm(RankedBanksByNumEmpls_12_31_11,
   RankedBanksByNumEmpls_3_31_12,
   RankedBanksByNumEmpls_6_30_12,
   RankedBanksByNumEmpls_9_30_12,
   RankedBanksByNumEmpls_12_31_12,
   RankedBanksByNumEmpls_3_31_13,
   RankedBanksByNumEmpls_6_30_13,
   RankedBanksByNumEmpls_9_30_13,
   RankedBanksByNumEmpls_12_31_13,
   RankedBanksByNumEmpls_3_31_14,
   RankedBanksByNumEmpls_6_30_14,
   RankedBanksByNumEmpls_9_30_14,
   RankedBanksByNumEmpls_12_31_14,
   RankedBanksByNumEmpls_3_31_15,
   RankedBanksByNumEmpls_6_30_15,
   RankedBanksByNumEmpls_9_30_15,
   RankedBanksByNumEmpls_12_31_15,
   RankedBanksByNumEmpls_3_31_16,
   RankedBanksByNumEmpls_6_30_16,
   RankedBanksByNumEmpls_9_30_16)

#Get year and quarter from the dates
all_ranked_banksByNumEmpls$quarterYear<-paste(quarters(as.Date(all_ranked_banksByNumEmpls$Date,
                                              format="%m/%d/%Y")),
                                              substr(all_ranked_banksByNumEmpls$Date,
                                              nchar(all_ranked_banksByNumEmpls$Date)-3,
                                              nchar(all_ranked_banksByNumEmpls$Date)),sep=" ")

str(all_ranked_banksByNumEmpls)
#133940 obs.

#Get rid of the commas in employee counts and convert the counts into integers.
all_ranked_banksByNumEmpls$Number.of.Employees<-
  as.integer(gsub(",","",all_ranked_banksByNumEmpls$Number.of.Employees))
summary(all_ranked_banksByNumEmpls$Number.of.Employees)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0     20.0     40.0    308.9     90.0 234500.0 
#Employee counts are very skewed to the right. And why would any of the counts be zero?

hist(all_ranked_banksByNumEmpls$Number.of.Employees,
     main="Distribution of number of employees",
     xlab="Number of employees")


#Keep only the employess number records where the bank is in the top 50 by equity capital. 
bankNumEmpls_top50<-all_ranked_banksByNumEmpls[all_ranked_banksByNumEmpls$Bank.Name %in%
                                        unique_banks_from_ranked_banks_top50,
                                        c("Number.of.Employees","Bank.Name","quarterYear","Rank")]

str(bankNumEmpls_top50)
length(unique(bankNumEmpls_top50$Bank.Name))#67 banks (the correct number)

#Make sure the banks ranked by number of employees have just one rank per quarter
bankNumEmpls_top50 %>%
  group_by(quarterYear,Bank.Name) %>%
  summarise(numranks=n_distinct(Rank))->numRanks2
summary(numRanks2$numranks)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   1.000   1.375   1.000  20.000 

#Some of these banks have more than 1 rank in a quarter! 
#Should I discard bank/quarter combinations with more than one rank? If so, should I 
#interpolate the missing ranks? How much data will I end up throwing out (or imputing) if I 
#do this?

write.csv(numRanks2,file="numRanks2.csv")
#Standardize bank names from number of employees data

for (i in 1:nrow(bankNumEmpls_top50))
{ bankNumEmpls_top50$Company[i]<-''
if (bankNumEmpls_top50$Bank.Name[i]=='Ally Bank'){bankNumEmpls_top50$Company[i]<-'Ally Financial Inc.'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Banco Popular de Puerto Rico'){bankNumEmpls_top50$Company[i]<-'Banco Popular de Puerto Rico'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Bank of America'){bankNumEmpls_top50$Company[i]<-'Bank of America'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Bank of the West'){bankNumEmpls_top50$Company[i]<-'Bank of the West'}
else if (bankNumEmpls_top50$Bank.Name[i]=='BMO Harris Bank'){bankNumEmpls_top50$Company[i]<-'BMO Harris'}
else if (bankNumEmpls_top50$Bank.Name[i]=='The Bank of New York Mellon'){bankNumEmpls_top50$Company[i]<-'BNY Mellon'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Capital One'){bankNumEmpls_top50$Company[i]<-'Capital One'}
#else if (bankNumEmpls_top50$Bank.Name[i]=='Capital One Bank (USA)'){bankNumEmpls_top50$Company[i]<-'Capital One'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Charles Schwab Bank'){bankNumEmpls_top50$Company[i]<-'Charles Schwab Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='CIT BANK'){bankNumEmpls_top50$Company[i]<-'CIT Bank National Association'}
else if (bankNumEmpls_top50$Bank.Name[i]=='CIT Bank'){bankNumEmpls_top50$Company[i]<-'CIT Bank National Association'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Citibank'){bankNumEmpls_top50$Company[i]<-'Citibank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Comerica Bank'){bankNumEmpls_top50$Company[i]<-'Comerica'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Deutsche Bank Trust Company Americas'){bankNumEmpls_top50$Company[i]<-'Deutsche Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Discover Bank'){bankNumEmpls_top50$Company[i]<-'Discover'}
else if (bankNumEmpls_top50$Bank.Name[i]=='E*TRADE Bank'){bankNumEmpls_top50$Company[i]<-'E*Trade Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Fifth Third Bank'){bankNumEmpls_top50$Company[i]<-'Fifth Third Financial Corporation'}
else if (bankNumEmpls_top50$Bank.Name[i]=='First Niagara Bank'){bankNumEmpls_top50$Company[i]<-'First Niagara Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='First Republic Bank'){bankNumEmpls_top50$Company[i]<-'First Republic Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Goldman Sachs Bank USA'){bankNumEmpls_top50$Company[i]<-'Goldman Sachs Bank USA'}
else if (bankNumEmpls_top50$Bank.Name[i]=='HSBC Bank USA'){bankNumEmpls_top50$Company[i]<-'HSBC North America Holdings Inc.'}
else if (bankNumEmpls_top50$Bank.Name[i]=='JPMorgan Chase Bank'){bankNumEmpls_top50$Company[i]<-'JPMorgan Chase & Co.'}
#else if (bankNumEmpls_top50$Bank.Name[i]=='Chase Bank USA'){bankNumEmpls_top50$Company[i]<-'JPMorgan Chase & Co.'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Morgan Stanley Bank'){bankNumEmpls_top50$Company[i]<-'Morgan Stanley'}
else if (bankNumEmpls_top50$Bank.Name[i]=='New York Community Bank'){bankNumEmpls_top50$Company[i]<-'New York Community Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Pacific Western Bank'){bankNumEmpls_top50$Company[i]<-'PACIFIC WESTERN BANK'}
else if (bankNumEmpls_top50$Bank.Name[i]=="People's United Bank"){bankNumEmpls_top50$Company[i]<-"People's United Bank"}
else if (bankNumEmpls_top50$Bank.Name[i]=='PNC Bank'){bankNumEmpls_top50$Company[i]<-'PNC Bank N.A.'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Regions Bank'){bankNumEmpls_top50$Company[i]<-'Regions Financial Corporation'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Santander Bank, N.A.'){bankNumEmpls_top50$Company[i]<-'Santander Bank US'}
else if (bankNumEmpls_top50$Bank.Name[i]=='State Street Bank and Trust Company'){bankNumEmpls_top50$Company[i]<-'State Street Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='SunTrust Bank'){bankNumEmpls_top50$Company[i]<-'SunTrust Banks, Inc.'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Synchrony Bank'){bankNumEmpls_top50$Company[i]<-'Synchrony Financial'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Synovus Bank'){bankNumEmpls_top50$Company[i]<-'Synovus Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='TD Bank'){bankNumEmpls_top50$Company[i]<-'TD Bank US Holding Company'}
else if (bankNumEmpls_top50$Bank.Name[i]=='The Huntington National Bank'){bankNumEmpls_top50$Company[i]<-'The Huntington National Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='The Northern Trust Company'){bankNumEmpls_top50$Company[i]<-'The Northern Trust Company'}
else if (bankNumEmpls_top50$Bank.Name[i]=='UBS Bank USA'){bankNumEmpls_top50$Company[i]<-'UBS Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Umpqua Bank'){bankNumEmpls_top50$Company[i]<-'Umpqua Holdings Corporation'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Union Bank'){bankNumEmpls_top50$Company[i]<-'Union Bank'}
else if (bankNumEmpls_top50$Bank.Name[i]=='USAA Savings Bank'){bankNumEmpls_top50$Company[i]<-'USAA Savings'}
else if (bankNumEmpls_top50$Bank.Name[i]=='Wells Fargo Bank'){bankNumEmpls_top50$Company[i]<-'Wells Fargo & Company'}
}

#Keep only the bank names from the ranked banks that have matches in the complaints data
bankNumEmpls_top50<-bankNumEmpls_top50[bankNumEmpls_top50$Company!='',]
write.csv(bankNumEmpls_top50,file="Standardized bank names from employee count data.csv")
my_table3<-table(bankNumEmpls_top50$quarterYear,bankNumEmpls_top50$Company)
write.csv(my_table3,file="my_table3.csv")
#Merge employee counts onto rest of bank data

mergedBanks2<-inner_join(mergedBanks,subset(bankNumEmpls_top50,
                        select=c("Number.of.Employees","quarterYear","Company")),
                        by=c("Company","quarterYear"))
write.csv(mergedBanks2,file="mergedBanks2.csv")
str(mergedBanks)#819 obs
str(mergedBanks2)#698 obs


#************************************Investment Case Study****************************************************


#***********************************   Loading libraries  ***********************************************
library(tidyr)
library(dplyr)
library(stringr)

#Loading data in the data frames
companies <- read.table("companies.txt", sep="\t", header = TRUE, comment.char = "", quote = "\"", stringsAsFactors=FALSE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors=FALSE)
mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors=FALSE, check.names = F)

#Converting permalinks to lower case to facilitate merging
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

#Which permalinks are present in companies but not in rounds2?
setdiff(rounds2$company_permalink, companies$permalink)

#Which permalinks are present in rounds2 but not in companies?
setdiff(companies$permalink, rounds2$company_permalink)

#****************************** Checkpoint 1: Data Preparation ************************************

#Question 1: How many unique companies are present in rounds2? 
#Answer: 66368
length(unique(rounds2$company_permalink))

#Question 2: How many unique companies are present in companies? 
#Answer: 66368
length(unique(companies$permalink))

#Question 3: In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#Answer: permalink

#Question 4: Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
#Answer: N

#Question 5: Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink", all = T)

#****************************** Checkpoint 2: Funding Type Analysis ****************************** 

#Question 1: Average funding amount of venture type
#11,748,949.1294895
avg_venture_funding <- mean(master_frame[which(master_frame$funding_round_type=="venture"),"raised_amount_usd"], na.rm = T)

#Question 2: Average funding amount of angel type
#958,694.469753086
avg_angel_funding <- mean(master_frame[which(master_frame$funding_round_type=="angel"),"raised_amount_usd"], na.rm = T)

#Question 3: Average funding amount of seed type
#719,817.996907173
avg_seed_funding <- mean(master_frame[which(master_frame$funding_round_type=="seed"),"raised_amount_usd"], na.rm = T)

#Question 4: Average funding amount of Private Equity type
#73,308,593.0294421
avg_priv_equity_funding <- mean(master_frame[which(master_frame$funding_round_type=="private_equity"),"raised_amount_usd"], na.rm = T)

#Question 5: Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, which investment type is the most suitable for them?
#Answer: venture

#****************************** Checkpoint 3: Country Analysis ******************************
venture_investments <- filter(master_frame, funding_round_type=="venture")
country_group <- group_by(venture_investments, country_code)
country_wise_investment <- summarise(country_group, total_amt_raised = sum(raised_amount_usd, na.rm = T))
top9 <- arrange(country_wise_investment[which(country_wise_investment$country_code!=""),], desc(total_amt_raised))[1:9,]

#Question 1: Top English speaking country
#Answer: United States (Country Code USA)

#Question 2: Second English speaking country
#Answer: United Kingdom (Country Code GRB)

#Question 3: Third English speaking country
#Answer: India (Country Code IND)

#****************************** Checkpoint 4: Data Preparation for sectoral analysis ******************************
#Extract the primary sector of each category list from the category_list column
master_frame$primary_sector <- sapply(master_frame$category_list, function(x) {str_trim(unlist(str_split(x, fixed("|")))[1])})

#Converting wide to long in the mapping file
mapping_long <- gather(mapping, sector, sec_val, 2:10)
#Remove extra rows which have value 0, as well as the last column as it will always be 1
mapping_long <- mapping_long[!(mapping_long$sec_val == 0),1:2]

#Correcting the data. Replacing "0" with "na"
mapping_long$category_list_new <- str_replace(mapping_long$category_list, fixed("0"), "na")

#Covert primary_sector in master_frame as well as category_list_new in mapping_long to facilitate merging
master_frame$primary_sector <- tolower(master_frame$primary_sector)
mapping_long$category_list_new <- tolower(mapping_long$category_list_new)

#Check which categories in mapping_long are not present in master_frame
setdiff(mapping_long$category_list_new, master_frame$primary_sector)

#Check which categories in master_frame are not present in mapping_long
setdiff(master_frame$primary_sector, mapping_long$category_list_new)

#Some more data cleaning for "enterprise 2.na" and "personal fi0nce"
mapping_long[which(mapping_long$category_list_new=="enterprise 2.na"), "category_list_new"] <- "enterprise 2.0"
mapping_long[which(mapping_long$category_list_new=="personal fi0nce"), "category_list_new"] <- "personal finance"

#Reverify which categories in mapping_long are not present in master_frame. Should show 0
setdiff(mapping_long$category_list_new, master_frame$primary_sector)

#Merge master_frame with mapping_long on primary_sector and category_list respectively to get the "main sector"
master_frame_with_main_sector <- merge(master_frame, mapping_long, by.x = "primary_sector", by.y = "category_list_new", all = T)

#************************** Checkpoint 5: Sectoral Analysis ****************************
D1 <- filter(master_frame_with_main_sector, country_code=="USA", funding_round_type=="venture", raised_amount_usd>=5000000, raised_amount_usd<=15000000)
D2 <- filter(master_frame_with_main_sector, country_code=="GBR", funding_round_type=="venture", raised_amount_usd>=5000000, raised_amount_usd<=15000000)
D3 <- filter(master_frame_with_main_sector, country_code=="IND", funding_round_type=="venture", raised_amount_usd>=5000000, raised_amount_usd<=15000000)

#The total number (or count) of investments for each main sector in a separate column
#The total amount invested in each main sector in a separate column
D1<-mutate(group_by(D1,primary_sector),total_number_of_investment=length(raised_amount_usd),total_amount_invested=sum(raised_amount_usd))
D2<-mutate(group_by(D2,primary_sector),total_number_of_investment=length(raised_amount_usd),total_amount_invested=sum(raised_amount_usd))
D3<-mutate(group_by(D3,primary_sector),total_number_of_investment=length(raised_amount_usd),total_amount_invested=sum(raised_amount_usd))

#Question 1: Total number of Investments (count)
#Number of Investments in USA : 12150
nrow(D1)
#Number of Investments in GBR : 628
nrow(D2)
#Number of Investments in IND : 330
nrow(D3)

#Question 2: Total amount of investment
#In USA: 108,531,347,515
sum(D1$raised_amount_usd)
#In GBR: 5436843539
sum(D2$raised_amount_usd)
#In IND: 2976543602
sum(D3$raised_amount_usd)

#Group investments within each country by sectors.
usa_sector_group <- group_by(D1, sector)
usa_num_investments_per_sector <- arrange(summarise(usa_sector_group, num_investment = n()), desc(num_investment))

gbr_sector_group <- group_by(D2, sector)
gbr_num_investments_per_sector <- arrange(summarise(gbr_sector_group, num_investment = n()), desc(num_investment))

ind_sector_group <- group_by(D3, sector)
ind_num_investments_per_sector <- arrange(summarise(ind_sector_group, num_investment = n()), desc(num_investment))

#Questions 3-7 are based on the values in usa_num_investments_per_sector,gbr_num_investments_per_sector ind_num_investments_per_sector 

#Question 3: Top Sector name (no. of investment-wise)
#Top sector in USA: "Others"
#Top sector in GBR: "Others"
#Top sector in IND: "Others"

#Question 4:
#Second sector in USA: "Social, Finance, Analytics, Advertising"
#Second sector in GBR: "Social, Finance, Analytics, Advertising"
#Second sector in IND: "Social, Finance, Analytics, Advertising"

#Question 5:
#Third sector in USA: "Cleantech / Semiconductors"
#Third sector in GBR: "Cleantech / Semiconductors"
#Third sector in IND: "News, Search and Messaging"

#Question 6: Number of investments in top sector 
#In USA: 2950
#In GBR: 147
#In IND: 110

#Question 7: Number of investments in second sector 
#In USA: 2714
#In GBR: 133
#In IND: 60

#Question 8: Number of investments in third sector 
#In USA: 2350
#In GBR: 130
#In IND: 52

#Calculating top sector for USA,GBR,IND for Q9
usa_top_sector<-usa_num_investments_per_sector[1,1] #Others
gbr_top_sector<-gbr_num_investments_per_sector[1,1] #Others
ind_top_sector<-ind_num_investments_per_sector[1,1] #Others

#Question 9: For point 3 (top sector count-wise), which company received the highest investment?
usa_others <- filter(D1, sector==usa_top_sector)
usa_company_group <- group_by(usa_others, company_permalink)
usa_others_top_company <- arrange(summarise(usa_company_group, total_amt_raised = sum(raised_amount_usd)), desc(total_amt_raised))[1,1]
#"/organization/virtustream" is the top company for USA in sector "Others"

gbr_others <- filter(D2, sector==gbr_top_sector) 
gbr_company_group <- group_by(gbr_others, company_permalink)
gbr_others_top_company <- arrange(summarise(gbr_company_group, total_amt_raised = sum(raised_amount_usd)), desc(total_amt_raised))[1,1]
#"/organization/electric-cloud" is the top company for United Kingdom in sector "Others"

ind_others <- filter(D3, sector==ind_top_sector)
ind_company_group <- group_by(ind_others, company_permalink)
ind_others_top_company <- arrange(summarise(ind_company_group, total_amt_raised = sum(raised_amount_usd)), desc(total_amt_raised))[1,1]
#"/organization/firstcry-com" is the top company for India in sector "Others"


#Calculating second best sector for USA,GBR,IND for Q9
usa_secondbest_sector<-usa_num_investments_per_sector[2,1] #Social, Finance, Analytics, Advertising
gbr_secondbest_sector<-gbr_num_investments_per_sector[2,1] #Social, Finance, Analytics, Advertising
ind_secondbest_sector<-ind_num_investments_per_sector[2,1] #Social, Finance, Analytics, Advertising

#Question 10: For point 4 (second best sector count-wise), which company received the highest investment?
usa_social_etc <- filter(D1, sector==usa_secondbest_sector)
usa_company_group <- group_by(usa_social_etc, company_permalink)
usa_social_top_company <- arrange(summarise(usa_company_group, total_amt_raised = sum(raised_amount_usd)), desc(total_amt_raised))[1,1]
#"/organization/shotspotter" is the top company for USA in sector "Social, Finance, Analytics, Advertising"

gbr_social_etc <- filter(D2, sector==gbr_secondbest_sector)
gbr_company_group <- group_by(gbr_social_etc, company_permalink)
gbr_social_top_company <- arrange(summarise(gbr_company_group, total_amt_raised = sum(raised_amount_usd)), desc(total_amt_raised))[1,1]
#"/organization/celltick-technologies" is the top company for United Kingdok in sector "Social, Finance, Analytics, Advertising"

ind_social_etc <- filter(D3, sector==ind_secondbest_sector)
ind_company_group <- group_by(ind_social_etc, company_permalink)
ind_social_top_company <- arrange(summarise(ind_company_group, total_amt_raised = sum(raised_amount_usd)), desc(total_amt_raised))[1,1]
#"/organization/manthan-systems" is the top company for India in sector "Social, Finance, Analytics, Advertising"
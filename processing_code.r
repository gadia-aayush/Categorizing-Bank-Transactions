#first set the working directory where already your CSV file exists in R Studio or R Console.

library(data.table)
library(dplyr)
library(stringr)


data <- fread("./Step2_raw_Transactions [AayushGadia].csv")
#data  <- fread("./Step2_raw_Transactions [Gradufund].csv")


#Mode Column Filling
data$mode <- as.character(data$mode)
txns <- c("INB","UPI","ATM WDL","by debit card","(CDM)","BULK POSTING","BY TRANSFER- SBIPG","CREDIT INTEREST","ANNUAL FEE","IMPS","ACH","NEFT")
txns_mode <- c("INTERNET BANKING","UPI","ATM Withdrawl","Debit Card","CASH","EFT","EFT","ACH","ACH","IMPS","ACH","NEFT")
i=1
for (val in txns){
	data[grep(val,data$narration),]$mode <- txns_mode[i]
	i <- i+1
}



#Merchant_Category Column Filling
data$merchant_category <- as.character(data$merchant_category)
merchant_keyword <- c("IRCTC","S[Tt][Ii][Pp][Ee][Nn][Dd]","[Ff][Oo][Oo][Dd]","Kalyani Expen",
	"[Ff][Re][Ee][Ee][Cc][Hh][Aa][Rr][Gg][Ee]","ONE97","INTEREST","BALARAM MULLICK","[Rr][Ee][Cc][Hh][Aa][Rr][Gg][Ee]","[Ii][Dd][Ee][Aa]",
	"[Uu][Bb][Ee][Rr]","CENTRAL","[Ff][Xx] [Mm][Aa][Rr][Tt]","CHAMPIAN HUB","(CDM)","[Aa][Mm][Aa][Zz][Oo][Nn]","BY TRANSFER","ATM WDL","TO TRANSFER",
	"R[Ee][Nn][Tt]","Myntra","[Pp][Ii][Ss][Cc][Ee][Ss] [Ee][Ss][Ee][Rr][Vv][Ii][Cc][Ee][Ss]","Zomato","[Ff][Uu][Tt][Uu][Rr][Ee] [Rr][Ee][Tt][Aa][Ii][Ll]",
	"Mummy Bday","BULK POSTING-","ATM ANNUAL","INDIA TRADING","ACH","NEFT","IMPS","CRTR","PMC")

merchant_catg <- c("TRAVEL","INCOME","FOOD","FOOD","E-WALLET","E-WALLET","INCOME","FOOD","TELEPHONE","TELEPHONE","TRAVEL","SHOPPING","SHOPPING","FUEL",
	"INCOME","SHOPPING","INCOME","CASH Withdrawl","INTERNET Transfers","RENT","SHOPPING","FOOD","FOOD","GROCERY","FOOD","INCOME","BANK DEBIT- Annual Charges",
	"Others","Bank- ACH","Bank Transfers","Bank Transfers","CHEQUE","Bank Transfers")

i=1
for (val in merchant_keyword){
	data[grep(val,data$narration),]$merchant_category <- merchant_catg[i]
	i <- i+1
}



#Transaction Amount Column Edit
edit_fn <- function(x){
gsub(",","",str_trim(x)) }

data[,9] <- lapply(data[,9],edit_fn)
data[,9] <- as.numeric(unlist(data[,9]))
data[,9] <- data[,9] * -1
data[,10] <- lapply(data[,10],edit_fn)
data[,10] <- as.numeric(unlist(data[,10]))
data[is.na(data)] <- 0



#Repeat Transaction Column Filling
data$repeat_transaction_category <- as.character(data$repeat_transaction_category)

unique_debit <- as.vector(unlist(unique(data[,9])))
for (d in seq(along=unique_debit)){
	dataset_unique <- data[which(data[,9]==unique_debit[d]),]
	uniquetxn_length <- length(unique(substr(unlist(dataset_unique[,4]),1,20)))
	txn_length <- length(substr(unlist(dataset_unique[,4]),1,20))
	if(uniquetxn_length==1 && txn_length!=1){
		txn_name <- unique(substr(unlist(dataset_unique[,4]),1,20))
		txn_id<-which(data[,9]==unique_debit[d])
		data[txn_id,8] <- paste("REPEATING TRANSACTION-- ",txn_name," ",unique_debit[d])}

	else{
		txn_name <- unique(substr(unlist(dataset_unique[,4]),1,20))
		txn_id<-which(data[,10]==unique_credit[c])		
		data[txn_id,8] <- paste("--UNIQUE TRANSACTION-- ")
	}	
}

unique_credit <- as.vector(unlist(unique(data[,10])))
for (c in seq(along=unique_credit)){
	dataset_unique <- data[which(data[,10]==unique_credit[c]),]
	uniquetxn_length <- length(unique(substr(unlist(dataset_unique[,4]),1,20)))
	txn_length <- length(substr(unlist(dataset_unique[,4]),1,20))
	if(uniquetxn_length==1 && txn_length!=1){
		txn_name <- unique(substr(unlist(dataset_unique[,4]),1,20))
		txn_id<-which(data[,10]==unique_credit[c])
		data[txn_id,8] <- paste("REPEATING TRANSACTION-- ",txn_name," ",unique_credit[c])}

	else{
		txn_name <- unique(substr(unlist(dataset_unique[,4]),1,20))
		txn_id<-which(data[,10]==unique_credit[c])		
		data[txn_id,8] <- paste("--UNIQUE TRANSACTION-- ")
	}	
}
write.csv(data,"./output_aayushgadia[0].csv")
#write.csv(data,"./output_gradufund[0].csv")


#[code by AAYUSH GADIA]
setwd("D:/R/Data/Ney")#set the work directory()
library(readxl)
data <- read_excel("D:/A/Papers andamento/Lesbia/RBCS/data/data.xlsx")
names(data)
data<-data[,c(9:10,16:63)]
data<-as.data.frame(data)

spe<-data[,3:ncol(data)]
str(spe)
apply(spe, 2, sum)#Dent Rac1 =0

# Function to calculate relative frequency of species by land use and landscape
RF <- function(data, landuse_col, landscape_col, species_cols) {
  
  # Create a new column combining land use and landscape
  data$combined <- paste0(data[[landuse_col]], data[[landscape_col]])
  
  # Initialize list to store relative frequencies
  relative_frequencies <- list()
  
  # Get the unique combinations of land use and landscape
  combinations <- unique(data$combined)
  
  # Iterate over each combination of land use and landscape
  for (comb in combinations) {
    # Filter the data by the current combination
    subset <- data[data$combined == comb, ]
    
    # Calculate the total number of individuals for each species in the subset
    total_individuals <- colSums(subset[, species_cols])
    
    # Calculate the total number of individuals across all species
    total_overall <- sum(total_individuals)
    
    # Calculate the relative frequency per species
    if (total_overall > 0) {
      relative_frequency <- total_individuals / total_overall
    } else {
      relative_frequency <- rep(0, length(total_individuals))  # Avoid division by zero
    }
    
    # Add the result to the list with the name of the combination
    relative_frequencies[[comb]] <- relative_frequency
  }
  
  # Convert the list of relative frequencies into a data frame
  frequency_df <- do.call(rbind, relative_frequencies)
  
  # Add species names as column headers to the resulting data frame
  colnames(frequency_df) <- colnames(data)[species_cols]
  
  # Return the data frame with relative frequencies
  return(frequency_df)
}


# Define the columns that contain species data (columns 3 to the last one)
species_cols <- 3:ncol(data)

# Run the function
rftable <- RF(data, "landuse", "landscape", species_cols)
rftable <- t(rftable)
rftable<-rftable[,c(1,2,4,3,7,8,6,5)]

colnames(rftable)<-c("clf","cllf","clsf","clp",
                   "dlf","dllf","dlsf","dlp")

lsclu<-round(rftable,digits = 3)
lsclu<-as.data.frame(lsclu)

abrel<-round(rftable*100,digits = 1)

################################################################
################################################################
################################################################
##### CL
lsclu$clf[lsclu$clf>0.5]<-"d"
lsclu$clf[lsclu$clf>0.31 & lsclu$clf<=0.5]<-"mc"
lsclu$clf[lsclu$clf>0.1 & lsclu$clf<=0.31]<-"c"
lsclu$clf[lsclu$clf>=0.001 & lsclu$clf<=0.1]<-"r"
lsclu$clf[lsclu$clf<0.001]<-"-"

lsclu$cllf[lsclu$cllf>0.5]<-"d"
lsclu$cllf[lsclu$cllf>0.31 & lsclu$cllf<=0.5]<-"mc"
lsclu$cllf[lsclu$cllf>0.1 & lsclu$cllf<=0.31]<-"c"
lsclu$cllf[lsclu$cllf>=0.001 & lsclu$cllf<=0.1]<-"r"
lsclu$cllf[lsclu$cllf<0.001]<-"-"

lsclu$clsf[lsclu$clsf>0.5]<-"d"
lsclu$clsf[lsclu$clsf>0.31 & lsclu$clsf<=0.5]<-"mc"
lsclu$clsf[lsclu$clsf>0.1 & lsclu$clsf<=0.31]<-"c"
lsclu$clsf[lsclu$clsf>=0.001 & lsclu$clsf<=0.1]<-"r"
lsclu$clsf[lsclu$clsf<0.001]<-"-"

lsclu$clp[lsclu$clp>0.5]<-"d"
lsclu$clp[lsclu$clp>0.31 & lsclu$clp<=0.5]<-"mc"
lsclu$clp[lsclu$clp>0.1 & lsclu$clp<=0.31]<-"c"
lsclu$clp[lsclu$clp>=0.001 & lsclu$clp<=0.1]<-"r"
lsclu$clp[lsclu$clp<0.001]<-"-"

##### DL
lsclu$dlf[lsclu$dlf>0.5]<-"d"
lsclu$dlf[lsclu$dlf>0.31 & lsclu$dlf<=0.5]<-"mc"
lsclu$dlf[lsclu$dlf>0.1 & lsclu$dlf<=0.31]<-"c"
lsclu$dlf[lsclu$dlf>=0.001 & lsclu$dlf<=0.1]<-"r"
lsclu$dlf[lsclu$dlf<0.001]<-"-"

lsclu$dllf[lsclu$dllf>0.5]<-"d"
lsclu$dllf[lsclu$dllf>0.31 & lsclu$dllf<=0.5]<-"mc"
lsclu$dllf[lsclu$dllf>0.1 & lsclu$dllf<=0.31]<-"c"
lsclu$dllf[lsclu$dllf>=0.001 & lsclu$dllf<=0.1]<-"r"
lsclu$dllf[lsclu$dllf<0.001]<-"-"

lsclu$dlsf[lsclu$dlsf>0.5]<-"d"
lsclu$dlsf[lsclu$dlsf>0.31 & lsclu$dlsf<=0.5]<-"mc"
lsclu$dlsf[lsclu$dlsf>0.1 & lsclu$dlsf<=0.31]<-"c"
lsclu$dlsf[lsclu$dlsf>=0.001 & lsclu$dlsf<=0.1]<-"r"
lsclu$dlsf[lsclu$dlsf<0.001]<-"-"

lsclu$dlp[lsclu$dlp>0.5]<-"d"
lsclu$dlp[lsclu$dlp>0.31 & lsclu$dlp<=0.5]<-"mc"
lsclu$dlp[lsclu$dlp>0.1 & lsclu$dlp<=0.31]<-"c"
lsclu$dlp[lsclu$dlp>=0.001 & lsclu$dlp<=0.1]<-"r"
lsclu$dlp[lsclu$dlp<0.001]<-"-"


RFTable<-cbind(lsclu,abrel)

#write.table(FRTable, file = "FRTable.txt", append = FALSE, quote = TRUE, sep = "\t",
#            na = "NA", dec = ".", row.names = TRUE,
#            col.names = TRUE, qmethod = c("escape", "double"),
#            fileEncoding = "")

###########################################################################
###########################################################################
###########################################################################
###########################################################################

lu<-c(1,1,2,2,1,1,2,2,4,4,4,4,4,4,3,3,3,3,2,1,1,2,3,3,4,4,
      3,4,1,1,4,1,1,1,1,4,4,3,3,3,2,2,2,2,2,2,3,3)
lsc<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
       2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
lulsc.<-paste(lsc,lu,sep = "")
lulsc.<-as.numeric(lulsc.)

library(labdsv)
idv3<-indval(spe,lulsc.)

idv.<-round(idv3$indval,digits = 3)
colnames(idv.)<-c("clf","cllf","clsf","clp",
                  "dlf","dllf","dlsf","dlp")
idv.$p<-idv3$pval
idv.$sig<-idv3$pval
idv.$sig[idv.$sig>0.05]<-"-"
idv.$sig[idv.$sig>0.01 & idv.$sig<0.05]<-"*"
idv.$sig[idv.$sig>0.001 & idv.$sig<0.01]<-"**"
idv.$sig[idv.$sig==0.001]<-"***"
rownames(idv.)
idv.$group<-c("-","-","DL.P","CL.SF","-","DL.P","-","DL.P","-","-","-","-",
              "-",
              "-","-","-","-",
              "-",
              "CL.P",
              "CL.P","DL.FF","CL.SF","CL.P","CL.P","CL.P","-","-","CL.P","-","-","CL.P","-","-","-","-","-","-",
              "-",
              "-","DL.P","-","-",
              "-","-","-","-","-","-")

summary(indval(spe,lulsc.))



#write.table(idv., file = "indval.txt", append = FALSE, quote = TRUE, sep = "\t",
#            na = "NA", dec = ".", row.names = TRUE,
#            col.names = TRUE, qmethod = c("escape", "double"),
#            fileEncoding = "")


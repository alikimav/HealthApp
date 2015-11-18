# Analyzing Apple Health Data with R

rm(list=ls())
## Working directory

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

## Libraries
library(XML)
library(plyr)
library(dplyr)
library(tools)
library(chron)
library(qdap)
library(ggplot2)

# install.packages("XML", dependencies = TRUE)
# filename <- readline("Enter the name of the file containing the data (no extn): ")
# filename <- paste0(filename, ".xml")
# doc1 <- xmlTreeParse(filename, asText = TRUE)

filename <- readLines("./export.xml")


xmltext <- paste(filename, "\n", collapse = "")
doc1 <- xmlTreeParse(xmltext, asText=TRUE)

# xmltop <- xmlRoot(doc1)

data <- xmlToList(doc1)

dfid <- as.data.frame(data[names(data) == "Me"])
dataMe <- as.data.frame(t(dfid))

# clean up variable names 
colnames(dataMe) <- gsub(".*Identifier", "", colnames(dataMe))

# extract information on subject (me)
dataMe$DateOfBirth <- strptime(dataMe$DateOfBirth, '%Y%m%d')

# The rest are under "Record" but not all elements have the same number of rows...
dfrecords <- data[names(data) == "Record"]

# So we use rbind.fill to row-bind a list of data frames and fill miss columns with NA values. We'll clean this
# up later once we have a list of all the different data frames
dfrecords0 <- rbind.fill(lapply(dfrecords,function(x){as.data.frame(t(x),stringsAsFactors=FALSE)}))
# colnames(dfrecords) <- gsub(".*Identifier", "", colnames(dfrecords))
dfrecords <- as.data.frame(sapply(dfrecords0,gsub,pattern=".*Identifier|^HK",replacement=""))
names(dfrecords) <- gsub("^HK", "", names(dfrecords))

# Next: let's sort the date. 
# convertDate <- function(dfname, variable = character(0)){

  for(col in c("creationDate", "startDate", "endDate")){
   dfrecords[, col] <- list(strptime(dfrecords[, col], format = "%Y%m%d%H%M%S"))
    }

dfrecords <- mutate(dfrecords, creationdate = as.Date(creationDate),
                  creationtime = strftime(creationDate,format = "%H:%M:%S"), 
                  startdate = as.Date(startDate),
                  starttime = chron( times = strftime(startDate,format = "%H:%M:%S")),
                  #enddate = as.Date(endDate),
                  endtime = chron(times = strftime(endDate,format = "%H:%M:%S")),
                  timezone = strftime(creationDate,format = "%Z"))

dfrecords2 <- dplyr::select(dfrecords, -matches("Date", ignore.case = FALSE))


for(var in c("value", "min", "max", "average")){
  dfrecords2[, var] <- as.numeric(dfrecords2[, var])
}


# I get a sample to do my trial wrangling on..

dfsample <- sample_n(dfrecords2, size = 40)

babylistdf <- split(dfsample, dfsample$type)

## Create a list of 
listdf <- split(dfrecords2, dfrecords2$type)

# listdf has a list of dataframes according to TYPE. The following line removes those columns from each dataframe in the
# list that only contains NA (i.e. columns that were introduced through rbind above). Note the base::Filter b/c we loaded 
# the qdap package which has its own filter function
listdf2 <- sapply(listdf, function(x) base::Filter(function(y)!all(is.na(y)), x))

newdf <- as.data.frame(sapply(listdf2, function(x) NROW(x)))

#    call <- substitute(mutate(dfsample, as.Date(col)), list(col = as.name(col)))
#    eval(call)

# Import my ptracker data

# ptrack <- read.table("Ptracker.txt", sep="\\", fill=FALSE, strip.white=TRUE, skip = 8)

ptrack <- read.csv("Ptracker.csv", header = FALSE)

ptrack <- colsplit2df(ptrack,, c("V1", "V2"), ":")

PeriodStart <- NULL
PeriodEnd <- NULL
CycleLength <- NULL
for(row in 1:nrow(ptrack)){
  if(ptrack[row, 1] == "Period Start"){
     PeriodStart <- append(PeriodStart, ptrack[row, 2])}
  else if(ptrack[row, 1] == "Period End"){
    PeriodEnd <- append(PeriodEnd, ptrack[row, 2])}
  else if(ptrack[row, 1] == "Cycle Length"){
    CycleLength <- append(CycleLength, ptrack[row, 2])
    }
}
ptracker <- cbind(as.data.frame(PeriodStart), as.data.frame(PeriodEnd), as.data.frame(CycleLength))
ptracker[ptracker == " n/a"] = NA

## ANALYSIS

dfAEB <- as.data.frame(listdf2$ActiveEnergyBurned) # AEB = active energy burned
dfBEB <- as.data.frame(listdf2$BasalEnergyBurned) # BEB = basal energy burned

dfAEB <- dplyr::select(dfAEB, -c(creationdate, creationtime,timezone)) %>% arrange(startdate) 
dfBEB <- dplyr::select(dfBEB, -c(creationdate, creationtime,timezone)) %>% arrange(startdate)


## Groups

AEBbydate <- aggregate(dfAEB$value, by = list(dfAEB$startdate), FUN = sum)
colnames(AEBbydate) <- c("Date", "cals_AEB")

BEBbydate <- aggregate(dfBEB$value, by = list(dfBEB$startdate), FUN = sum)
colnames(BEBbydate) <- c("Date", "cals_BEB")
mergeddf <- join(AEBbydate, BEBbydate, by = 'Date')

test <- melt(mergeddf, id = c("Date"))

ggplot(test2, aes(x = variable, y = value))+
  geom_boxplot(aes(color = variable))+
  scale_color_brewer(palette = "Dark2")




# sources <- as.data.frame(sapply(1:length(datasample),function(x) datasample[[x]][2]))

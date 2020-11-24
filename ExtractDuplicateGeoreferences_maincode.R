#####
#title: ExtractDuplicateGeoreferences
#author: Katelin (Katie) Pearson
#contact: katelin.d.pearson24@gmail.com
#date: November 24, 2020
####

#load packages
library(dplyr)
#library(grr)
library(stringr)
library(gtools)

#load the table of specimens (occids) that have duplicates
#in most Symbiota instances, this is the omoccurduplicatelink table
duplinks <- read.csv("PATH/omoccurduplicatelink.csv")

#load the table of georeference data from the database
#we used a modified version of omoccurrences that only includes the columns occid,
#decimalLatitude, decimalLongitude, geodeticDatum, coordinateUncertaintyInMeters, footprintWKT,
#coordinatePrecision, georeferencedBy, georeferenceSources, and georeferenceRemarks
others <- read.csv("PATH/omoccurrences_geo.csv")

#load the table of data from the collection for which you are importing data
#make sure that the first column contains the occurrence ID and is called "occid"
#by default, it will be named "id" if coming out of Symbiota
mycoll <- read.csv("PATH/omoccurrences_mycoll.csv")

#make a new data frame that will hold all the new georeferences
#note that the maximum is 50,000 records here; increase size as needed
newMycoll <- matrix(ncol=10,nrow=50000)
colnames(newMycoll) <- c("occid","decimalLatitude","decimalLongitude","geodeticDatum","coordinateUncertaintyInMeters","footprintWKT","coordinatePrecision","georeferencedBy","georeferenceSources","georeferenceRemarks")
newMycoll <- as.data.frame(newMycoll)
f <- 1

#look at each record in the mycoll dataframe
for(i in 1:dim(mycoll)[1]){
  print(i)
  #if desired, uncomment the line below to track progress (it will slow down the code, though)
  #print(paste((i/dim(mycoll)[1])*100,"% complete"))
  #if the record in mycoll already has georeference data, go to the next record
  if(!is.na(mycoll$decimalLatitude[i])){
    next
  } else {
    #does the mycoll record have a duplicate?
    m <- match(mycoll$occid[i],duplinks$occid)
    #if no duplicate found, go to the next record
    if(is.na(m)){
      next
    }  
      else {
      this <- mycoll$occid[i]
      #find the id number of the duplicate cluster to which this record belongs
      dupid <- duplinks$duplicateid[duplinks$occid==this]
      #make a temporary dataframe of all the records that belong to this cluster
      dupes <- subset(duplinks,duplicateid==dupid)
      #from the temporary dataframe, remove the record that we are searching against (the original record)
      dupes <- subset(dupes, occid!=mycoll$occid[i])
      #if there are no other duplicate records (because of a mistake), go to the next record in mycoll
      if(dim(dupes)[1]<1){
        next
      } else {
        #for every duplicate in the temporary dataframe
        for(j in 1:dim(dupes)[1]){
          #find the occid for that duplicate
          thisdup <- dupes$occid[j]
          #if the duplicate does not have georeference data, go to the next duplicate in the temporary dataframe
          if(others$decimalLatitude[others$occid==thisdup]=="NULL"){
            next
          } else {
            #if the duplicate DOES have georeference data, add all the data to the "newMycoll" dataframe
            newMycoll$occid[f]=mycoll$occid[i]
            newMycoll$decimalLatitude[f]=as.character(others$decimalLatitude[others$occid==thisdup])
            newMycoll$decimalLongitude[f]=as.character(others$decimalLongitude[others$occid==thisdup])
            newMycoll$geodeticDatum[f]=as.character(others$geodeticDatum[others$occid==thisdup])
            newMycoll$coordinateUncertaintyInMeters[f]=as.character(others$coordinateUncertaintyInMeters[others$occid==thisdup])
            newMycoll$footprintWKT[f]=as.character(others$footprintWKT[others$occid==thisdup])
            newMycoll$coordinatePrecision[f]=as.character(others$coordinatePrecision[others$occid==thisdup])
            newMycoll$georeferencedBy[f]=as.character(others$georeferencedBy[others$occid==thisdup])
            newMycoll$georeferenceSources[f]=as.character(others$georeferenceSources[others$occid==thisdup])
            newMycoll$georeferenceRemarks[f]=paste("copied from duplicate; ", as.character(others$georeferenceRemarks[others$occid==thisdup]))
            f <- f+1
          }
        }
      }
    }
  }
}

write.csv(newMycoll,"PATH/myColl_new_georefs.csv")


##Adding additional fields for easy import

for(p in 1:dim(newMycoll)[1]){
  m <- match(newMycoll$occid[p],mycoll$occid)
  newMycoll$catalogNumber[p] <- as.character(mycoll$catalogNumber[m])
  newMycoll$otherCatalogNumber[p] <- as.character(mycoll$otherCatalogNumbers[m])
  newMycoll$collectorNumber[p] <- as.character(mycoll$recordNumber[m])
  newMycoll$collector[p] <- as.character(mycoll$recordedBy[m])
}

write.csv(newMycoll,"PATH/myColl_new_georefs2.csv")
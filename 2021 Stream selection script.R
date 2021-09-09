# File: Flstreams.R
# Purpose: Select stream sites for Florida streams
#                 throughout the 6 basins for 2021
# Programmer: Chris Sedlacek, Jay Silvanima
# Date: 25 February 2021

# Load the library
library (spsurvey)

# Read the .dbf file
att<-read.dbf("C:/R/2021 Stream selections/Cycle15_SmallStreams_Coverage")
names(att)<-tolower(names(att)) # This action just makes typing easier

# Check the first 6 and last 6 lines of data in att
head(att)
tail(att)
levels(att$c3_zone)
#levels(att$stratum)
#levels(att$basin_name)
att$basin_name<- att$c3_zone

strmlngth<-tapply(att$length_km,list(att$c3_zone), sum)
strmlngth[is.na(strmlngth)] <- 0
round(addmargins(strmlngth),1)

# Read.dbf automatically calculates the length of each stream segment
# names the result length_mdm
# Change length calculated by spsurvey to km
att$length_mdm<-att$length_mdm/1000

# Create a column to use as a stratum variable and check the components in 
# the att.stratum column and the att.basin_name column 
att$stratum<-factor(as.character(att$c3_zone))
levels(att$stratum)
levels(att$c3_zone)


# Create the panel design for the six basins created for Cycle 3 streams 
# selection with a 9x oversample
# Stratified           

dsgn_SS <- list('Zone 1'=list(panel=c(Base=15),
                              seltype='Equal',
                              over=135), 
                'Zone 2'=list(panel=c(Base=15),
                              seltype='Equal',
                              over=135),
                'Zone 3'=list(panel=c(Base=15),
                              seltype='Equal',
                              over=135),
                'Zone 4'=list(panel=c(Base=15),
                              seltype='Equal',
                              over=135),
                'Zone 5'=list(panel=c(Base=15),
                              seltype='Equal',
                              over=135),        
                'Zone 6'=list(panel=c(Base=15),
                              seltype='Equal',
                              over=135)
                
)


# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(356356)  # Don't change unless want a different set of sites
dsgntime <- proc.time()  # keep track of how long spsurvey takes
sites <- grts(design=dsgn_SS,
              DesignID="FLSS21001",
              type.frame="linear",
              src.frame="shapefile",
              in.shape="C:/R/2021 Stream selections/Cycle15_SmallStreams_coverage",
              att.frame=att,
              stratum="stratum",
              prjfilename="C:/R/2021 Stream selections/Cycle15_SmallStreams_coverage",
              out.shape="C:/R/2021 Stream selections/")
dsgntime <- (proc.time() - dsgntime)/60
dsgntime
# print summary of sites selected
dsgnsum(sites)
# Print the initial six lines of the survey design
sites <- sites@data    # this is attribute part of shapefile created
head(sites)

write.csv(sites, "2021_Stream_Selections.csv")

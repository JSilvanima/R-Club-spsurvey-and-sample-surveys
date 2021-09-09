# File: Flstreams.R
# Purpose: Select stream sites for Florida streams
#                 throughout the 6 basins for 2021
# Programmer: Chris Sedlacek, Jay Silvanima
# Date: 25 February 2021
# Updated Sept 2021 to add comments and graphic

# This script needs to be run in R version 3.4 or lower as the grts function
#   will crash otherwise.  Can toggle to 3.4 if you have it loaded via RStudio 
#   Tools global options R versions - change.

##Set directory. This is where the outputs will be saved. 
#   Alter to desired location. Use getwd() to determine the directory for
#     your r project.

setwd("C:/R/2021 Stream selections")

# Load the library
library (spsurvey)


# Load extent of small stream resource coverage in .dbf format from ArcGIS 
# shapefile. Note shape file needs the information for the stream reachcodes and
# and geographic strata (6 Zones)

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

# Determine the extent of the streams in km for each zone and for the state.

strmlngth<-tapply(att$length_km,list(att$c3_zone), sum)
strmlngth[is.na(strmlngth)] <- 0
round(addmargins(strmlngth),1)

# Results from above calculations
#  Zone 1  Zone 2  Zone 3  Zone 4  Zone 5  Zone 6     Sum 
# 12329.2  2201.8  4484.5  4199.1   982.6   182.5 24379.8

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
              out.shape="C:/R/2021 Stream selections/ss site plot")
dsgntime <- (proc.time() - dsgntime)/60
dsgntime
# print summary of sites selected
dsgnsum(sites)
# Print the initial six lines of the survey design
sites <- sites@data    # this is attribute part of shapefile created
head(sites)

write.csv(sites, "2021_Stream_Selections.csv")

# plot sites
library(rgdal)

sssiteplot <- readOGR("C:/R/2021 Stream selections/ss site plot.shp")

plot(sssiteplot)

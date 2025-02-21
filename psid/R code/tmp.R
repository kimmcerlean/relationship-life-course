##trying to figure out here and failing

getwd()

install.packages("here")
here()

here:i_am("C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course")

library(here)
setwd("C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course")

kim <- here("kim", "C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course")
lea <- here("lea", "C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course")

here(kim)
setwd(here("kim"))
getwd()

setwd(here("lea"))
getwd()

load("created data/setupsequence.RData")

kim <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course'
test <- 'C:/Users/mcerl/Istituto Universitario Europeo/Pessin, Lea - 1. WeEqualize - Team Folder'
lea <- 'C:/Users/lpessin/OneDrive - Istituto Universitario Europeo/1. WeEqualize - Team Folder/Papers/Cross National Analysis of the Division of Labor across the Relationship Life Course'

setwd(kim)
getwd()

setwd(test)
getwd()

getwd()
version

Sys.getenv()
Sys.getenv(c("USERNAME")) ## mcerl

if (Sys.getenv(c("USERNAME")) == "mcerl") { setwd(kim) }

getwd()

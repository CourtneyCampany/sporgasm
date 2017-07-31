#read in and format saccoloma leaf age dataset from La Selva July 2017

chlro <- read.csv("saccoloma/sacc_chl.csv")
photo <- read.csv("saccoloma/sacc_photo.csv")


addleafage_func <- function(x){
  x$leafage <- ifelse(x$Color == "w", 1, 99)
  x$leafage <- ifelse(x$Color == "o", 2, x$leafage)
  x$leafage <- ifelse(x$Color == "y", 3, x$leafage)
  x$leafage <- ifelse(x$Color == "p", 4, x$leafage)
return(x)
  }

chlro <- addleafage_func(chlro)
photo <- addleafage_func(photo)

years <- c("Year 1", "Year 2", "Year 3", "Year 4")

library(doBy)
photo_agg <- summaryBy(Photo + Cond + Ci + Trmmol ~ Indiv + leafage, data=photo,
                       FUN=mean, keep.names = TRUE)

  photo_agg$iwue <- with(photo_agg, Photo/Trmmol)
  #indiviudal 26 seems to have bad h20 data

boxplot(iwue ~ leafage, data=photo_agg, outline=FALSE, xaxt="n")
axis(1, at=1:3, years[1:3])
##wue is likely lower in age 3

boxplot(Photo ~ leafage, data=photo_agg, outline=FALSE)
#An seems to be higher in year 2, a lot of variation in year 1 however

boxplot(Trmmol ~ leafage, data=photo_agg, outline=FALSE)

boxplot(chl ~ leafage, data=chlro, outline=FALSE)
#chlorophyll content unchanged over leaf age categories
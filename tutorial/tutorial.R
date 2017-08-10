##tutorial with Fern data

#install R (R cran) and R studio for FREE

#setup R studio in a convienent way (tools:global options:pane layout)
#1. source is where most of your work goes, you will write scripts here
#2. console is a tool to perform quicker options you likely dont want to save
## console also prints output of code you run in source
#3. environment allows you to see what you currently have loaded (datasets, objects, etc.)

# R basics ----------------------------------------------------------------
##comment with at least one # (no code will be run, useful formaking notes or stopping code)

##R works has a calculator (these operations will prin in console)

# Arithmetic
12 * (10 + 1)

# pi is a built-in constant
sin(pi/2)

# Absolute value
abs(-10)

# Square root
sqrt(225)

# Exponents
15^2

# Projects ----------------------------------------------------------------

##skipping a lot of intro steps to learn the ease of Rprojects (just like a pc desktop)
##setup folders for raw_data, scripts, functions, output, etc..
##IMPORTANTLY your working directory starts in the project (super easy)


# First script ------------------------------------------------------------


#what is a script? A text file with code that allows for reproducibility
#Scripts should be saved with .R extenstion (i.e. tutorial.R)


# OBJECTS -----------------------------------------------------------------

#objects make the scripting world go round. They are name variables that can hold
# different values.  Can be a single value, many or a whole dataset!

#lets make a few: define them, hit ctrl+enter(or run) and watch them appear in environment
#once they are in the environment you can reuse them (until you quit R or clear environment) 
#use  "<-"

x <- 23
y <- -15
#do some calculations
x+y

z <- x*2 + y^2

#rewrite a new 'x':
x <- "I am sooo awesome"
message(x)


# VECTORS -----------------------------------------------------------------

#vector is a string of numbers or bits of text (but not a combination of both). 
#The power of R is that most functions can use a vector directly as input

nums1 <- c(1,4,2,8,11,100,8)

# Get the sum of a vector:
sum(nums1)

# Get mean, standard deviation, number of observations (length):
mean_nums1 <- mean(nums1)
sd(nums1)
length(nums1)
cs_nums1 <- cumsum(nums1) # cumulative sum

#how would you reuse or save the output of one of these operations??????

##Vectorized operations: perform operations on multiple vectors
##results in another vector (performs one elemts of the vector at a time)

nums1 * y
nums1^2

#two vectors of same length
nums2 <- c(7,11,12,8,14,10,80)

#pairwise calculations
nums1/nums2

#there are so many logical functions to apply to vectors which are key to learn
length()
sort()
order()
head()
tail()
unique()
round()
which.max()
which.min()
mean()
var()
sd()

##you can use multiple functions in one line of code: 

# Mean of the vector, *then* square it:
mean(nums1)^2

# Mean of the log of vec2:
mean(log(nums1))

# The sum of squared deviations from the sample mean:
sum((nums1 - mean(nums1))^2)
## [1] 10
# The sample variance:
sum((nums1 - mean(nums1))^2) / (length(nums1) - 1)

###How would you calculate the SE of vec1????
sd(nums1)/sqrt(length(nums1))

##remember vectors can be characters too...useful for plotting labels and colors
mycols <- c("red","forestgreen","cornflowerblue","gold","pink")
sort(mycols)
nchar(mycols)
#extract one element of a vector
mycols[3]

# making data -------------------------------------------------------------

# we can use c() to ’concatenate’ (link together) a series of numbers
nums3 <- c(nums1, nums2)

#generate sequences of numbers using :, seq and rep 
 
# Sequences of integer numbers using the ":" operator:
  1:10 # Numbers 1 through 10

# Examples using seq()
seq(from=1,to=67,by=8)

# Replicate 
rep(2, times = 10)
rep(c(4,5), each=3)

#random numbers using the runif function. Draws from a uniform distribution,
#meaning there is an equal probability of any number being chosen.

runif(10)
# Five random numbers between 100 and 1000
a <- runif(425, 100, 1000)

##workspace quickies
ls()
rm(x)

# learn how to read in dataframes -----------------------------------------------
traits <- read.csv("pathtofile.csv")

traits <- read.csv("raw_data/fern_traits.csv")

#the traits object is a dataframe (rows + columns)

##lets inspect the data several ways (i usally do these operations in console)
traits
head(traits)
str(traits)
summary(traits)
nrow(traits)
ncol(traits)

#you can easily make a dataframe
mydat <- data.frame(var1=nums1, var2=nums2)

# work with data ----------------------------------------------------------

#you usually read in a dataframe but then we need to clean uop, format and calculate

# column names
names(traits)

names(traits) <- #c("S","D","SP","N","Pno", "g", "h' ")
# rename first vairable
names(traits)[1] <- "Location"
# rename 1st and 2nd:
names(traits)[1:2] <- c("Place","Date")

#INDEXING (super mega very important)

#Individual elements of a vector can be extracted using square brackets, [ ]

nums1[1:3]

# Select a few elements of a vector (reusable)
selectthese <- c(1,5,7)
nums1[selectthese]

# Remove the first element:
nums1[-1]

#logical indexing
nums2[nums2 > 10]
nums2[nums2 >= 11]
nums1[nums1 == 8]

nums2[nums2 > 5 & nums2 < 10] #two arguments together
nums2[nums2 < 1 | nums2 > 20] #either 

nums1[nums1 != 100] #does not equal
nums1[nums1 %in% c(1,4,11)] #includes

nums2[nums2 > 10 & !nums2 == 80]

#subsetting (can use subset function but better to learn indexing)

#remember = dataframe[rows, columns]

traits[4,4]
traits[,3]
traits[1:5, "stipe_length_cm"]
newdat <- traits[, c("site", "niche", "plant_no","chl_mg_m2" )]

traits[which.max(traits$laminalength_cm), "chl_mg_m2"]

#something useful
levels(traits$niche)
chl_epi <- traits[traits$niche=="epiphyte", c("plant_no", "chl_mg_m2", "niche")]

str(traits)
traits_nochl <- traits[,-11 ]

##i would suggest reading about subset() 

##EXPORTING:

#now that we made new dataframes (datasets)...we can save them for later use

write.csv(chl_epi, "calculated_data/chl_data.csv", row.names = FALSE)


#classes
#many of these variables have different classes: all of which are important
#numeric, character, factor(categorical), logical, date, POSIXct (date+time) are the main ones
str(traits)
#R tries to assign character classes at factors (but you can always do it yourself)
str(traits$site)
traits$site <- as.character(traits$site)
traits$site <- as.factor(traits$site)

levels(traits$niche) #factor levels are important (zeros, deleting data)
table(traits$niche)

#if you delete a certain factor in your dataframe all the factors levels will still remain
epi_dat <- traits[!traits$niche == "terrestrial",]
levels(epi_dat$niche) #this can be very important for stats
droplevels(traits) #if you want it really gone.

#paste()
##FOR MIKE V.: usually we have our treatment groups in sepearte columns (co2 and wp)
dfr$trt <- paste(dfr$co2, dfr$wp, sep="-")
#lets pretend that we had different sites for the traits dataset
traits$id <- paste(traits$site, traits$species, sep="-")
#or use with()
traits$id2 <- with(traits, paste("site", "niche", sep="."))

#another useful function is gsub() for formatting (find and replace)
traits$species2 <- gsub("_", " ", traits$species)

traits$genus <- as.factor(gsub("_.*","", traits$species)) #uses regular expression

#calculations to make new variables
traits$laminalength_m <- 


####for example: you have 2 CO2 treatments which are read as numeric
dfr$co2 <- as.factor(dfr$co2)

#DATES: there is a standard format for date with coding:
##:   "1969-08-18 09:00:00"
##most of the time we forget to do this and R often classifies our Date column as factor

# Date
sometime <- as.Date("1980-6-19")
str(sometime)

#format date for traits dataframe
head(traits$date)
?as.Date
?strptime
traits$date <- as.Date(traits$date, format= "%d/%m/%Y", tz="UTC")
#did it work?
max(traits$date)
str(traits$date)

max(traits$date) - min(traits$date) 

# missing values ----------------------------------------------------------

#missing values are represented with NA,indicateing the data is simply Not Available.

myvec1 <- c(11,13,5,6,NA,9)
mean(myvec1)
mean(myvec1, na.rm=TRUE)

#super useful to check your data...
is.na(myvec1)
which(is.na(myvec1))

is.na(traits)
which(is.na(traits))

#if you want the dataframe with only rows that have no missing values (BE CAREFUL)
traits_nona <- traits[complete.cases(traits),]


# visualizing data --------------------------------------------------------

##choosing a plot type:  you have all the options (plot, barplot, hist, pie, boxplot)

#Two ways to make a plot of two variables X and Y that are contained in a dataframe

# Option 1: plot of X and Y
  with(dfr, plot(X,Y))
# Option 2: formula interface (Y 'as a function of' X)   
  plot(Y ~ X, data=dfr) #i use this one
  
plot(frond_length_cm ~ lamina_area_cm2, data=traits)

###time time make it look pretty
###?par is your BFF
plot(frond_length_cm ~ lamina_area_cm2, data=traits,
     ylim=c(0, 180), xlim=c(0, 1800))
#you can easily add treatment colors as long as they are factors
plot(frond_length_cm ~ lamina_area_cm2, data=traits,
     ylim=c(0, 180), xlim=c(0, 1800),
     bg=niche, pch=25)
#that pretty cool but the colors are soo ugly
levels(traits$niche)
length(mycols)

plot(frond_length_cm ~ lamina_area_cm2, data=traits,
     ylim=c(0, 180), xlim=c(0, 1800),
     bg=mycols[niche], pch=21)
#so often you have points that overlap, so we can add transparency and change point type

##This is our first look at packages (they are made to enhance base R)
library(scales) #you have to install them first, then load intro environment
mycols2 <- alpha(mycols, .75)

windows()
plot(frond_length_cm ~ lamina_area_cm2, data=traits,ylim=c(0, 180), 
     xlim=c(0, 1800), cex=1.5,
     bg=mycols2[niche], pch=21)

##we need to make some better axis titles (use ylab, xlab)
plot(frond_length_cm ~ lamina_area_cm2, data=traits,ylim=c(0, 180), xlim=c(0, 1800),
     bg=mycols2[niche], pch=21, ylab="Frond Length (cm)", 
     xlab=expression(Lamina~area~~(cm^2))) #expression allows sub/superscripts

#what if you need to use the super long lamina area label for many plots???
LAlabel <- expression(Lamina~area~~(cm^2))


plot(frond_length_cm ~ lamina_area_cm2, data=traits,ylim=c(0, 180), xlim=c(0, 1800),
     bg=mycols2[niche], pch=21, ylab="Frond Length (cm)", 
     xlab=LAlabel)

#almost there but are outside margins are funky.... ?par
#we can setup some global plotting parameters which will work for all subsequent plots

windows()
par(mar=c(4,4,1,1), cex.axis=.8, cex.lab=1.1, mgp=c(2.5, 1, 0))

plot(frond_length_cm ~ lamina_area_cm2, data=traits,ylim=c(0, 180), xlim=c(0, 1800),
         bg=mycols2[niche], pch=21, ylab="Frond Length (cm)", cex=1.5,
         xlab=LAlabel)  
    
#almost done...but we need a legend 

legend("bottomright", levels(traits$niche), pch=21,inset=0.01, 
       pt.bg=mycols2, bty='n') 

dev.copy2pdf(file="output/lengthbyarea.pdf")
dev.off()
###BASIC STATS
    
mean(traits$lamina_area_cm2)
var_la <- var(traits$lamina_area_cm2)
sqrt(var_la) #stand dev 0r ....
sd(traits$lamina_area_cm2)
median(traits$lamina_area_cm2)
quantile(traits$lamina_area_cm2)
#these functions might fail with NA's s0 remember na.rm=TRUE)


#calculate a confidence inteval (perhaps subset a specific niche)
alpha <- 0.05 # 95% confidence interval
xbar <- mean(traits$lamina_area_cm2)
s <- sd(traits$lamina_area_cm2)
n <- length(traits$lamina_area_cm2)
half.width <- qt(1-alpha/2, n-1)*s/sqrt(n)
# Confidence Interval
c(xbar - half.width, xbar + half.width)

qqnorm(traits$lamina_area_cm2)



# simple linear regression ------------------------------------------------

##lets assume that our previous plot looked remotely linear

plot(frond_length_cm ~ lamina_area_cm2, data=traits)

model <- lm(log10(frond_length_cm) ~ log10(lamina_area_cm2), data=traits)
summary(model)

plot(log10(frond_length_cm) ~ log10(lamina_area_cm2), data=traits,log='xy')
abline(model)

##we can plot the model object to get some diagnostics of the fit
plot(model)
library(car)
qqPlot(model) #puts confidence intervals on to better understand departures from normality
###we could perform a data transformation
plot(frond_length_cm ~ lamina_area_cm2, data=traits, log='xy')
logmodel <- lm(log10(frond_length_cm) ~ log10(lamina_area_cm2), data=traits)
summary(logmodel)
plot(logmodel)
#R points out the row numbers of the outliers on diagnostic plots

norm <- residuals(model)
hist(norm)


# summarizing data --------------------------------------------------------

#probably my most used package is "doBy" which is great for data summaries

library(doBy)
#summaryBy(Yvar1 + Yvar2 ~ Groupvar1 + Groupvar2, FUN=c(mean,sd), data=mydata)

chl_agg <- summaryBy(chl_mg_m2 ~ species + niche, FUN=mean, data=traits)

traits_agg <- summaryBy(chl_mg_m2 + frond_length_cm + lamina_area_cm2 ~ 
                       niche, FUN=mean, data=traits)

#if you only have one FUN, you can add keep.names to not alter variable names (BE CAREFUL)
species_agg <- summaryBy(chl_mg_m2 + frond_length_cm + lamina_area_cm2 ~ 
                          species, FUN=mean, data=traits, keep.names = TRUE)
##this function is pretty smart too so you can to all variables
## assuming your factors are properly assigned
traits_agg2 <- summaryBy(. ~ species + niche, FUN=mean, data=traits, keep.names = TRUE)

#if we add more FUN, we very likley need to get rid of any missing values
traits_nona <- traits[complete.cases(traits),]

se <- function(x) sd(x)/sqrt(length(x))
traits_summ <- summaryBy(. ~ species + niche,
          data=traits_nona,
          FUN=c(mean, se))

##we can save this summary dataframe to make a table later...we didnt do it super clean
##so lets use what we have learned to get rid of plant_no_mean
str(traits_summ)
fernsumm <- traits_summ[, -c(3, 10)]

write.csv(fernsumm, "output/fern_traits_summary.csv", row.names = FALSE)
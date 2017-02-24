#Jadwiga Buchwald
#Created 17. Feb. 2017 (IODS week 4: clustering and classification)
#Continued on the 24. Feb 2017 (IODS week 5: Dimention reduction)
#creating data named human, which originates from the United Nations Development Programme
#http://hdr.undp.org/en/content/human-development-index-hdi #their web page
#http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf #overview of the indices the data contains
#https://raw.githubusercontent.com/TuomoNieminen/Helsinki-Open-Data-Science/master/datasets/human_meta.txt #descriptions of modified variables

setwd("C:/HY-Data/JADWIGAB/PhD_FIMM/NotesOutsideTheOffice/Courses/OpenData2017/For_IODS/IODS-project/data")
getwd()
##################################################################################################################################
####week 4

##############Wrangling
### Reading "Human development" and "Gender inequality" datas into memory
#meta: http://hdr.undp.org/en/content/human-development-index-hdi
#technical details: http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

### Explore the dimensions and stucture of the data
dim(hd); dim(gii) #both have 195 observations, hd has 8 variables and gii 10.
names(hd); names(gii) #Different variable names , but the linking variable seems to be Country
str(hd); str(gii) #mainly numeric variables
summary(hd);summary(gii) #all variables on different scales and some missing values

#Renaming the variables
names(hd)<-c("HDI.Rank","Country","HDI","Life.Exp","Eduyears.Exp", "EduAt.Mean","GNI","GNIminusHDI.Rank")
names(gii)<-c("GII.Rank","Country","Gender.Ineq.I","Mat.Mort.Ratio","Adol.Birth.Rate", "Parl.F.Prcnt","Sec.Edu.F","Sec.Edu.M","LabourForce.F","LabourForce.M")

#Two new variables to gii
#The first one should be the ratio of Female and Male populations with secondary education in each country. (i.e. edu2F / edu2M). 
#The second new variable should be the ratio of labour force participation of females and males in each country (i.e. labF / labM).
gii$Sec.Edu.FMRatio<-with(gii,Sec.Edu.F/Sec.Edu.M)
gii$LabourForce.FMRatio<-with(gii,LabourForce.F/LabourForce.M)
summary(gii) #looks right


### Merging the above datasets making sure we keep ony the countries present in both datasets ad the ordering of rows to that of data set hd
human<-merge(hd,gii,by="Country", all.x=F,sort=F)
dim(human) #195 rows and 19 columns 
names(human) #13 first variables the ones by which the merging was done. Columns 14:33 maths class versions of the other variables and 34:53 the por versions.
str(human) #characters,integers, and numeric variables. 
tail(human,n=10)
###Saving the data!
write.table(human,"./human.txt",sep="\t",row.names = F)

###Checking we can read it back in
human<-read.table("./human.txt",sep="\t", header=T, dec=".",stringsAsFactors = FALSE)
dim(human) #195 x 19
head(human)
str(human) #perfect!

####################################################################################################################################
#week 5

#Reading in the data we created last week
human<-read.table("./human.txt",sep="\t", header=T, dec=".",stringsAsFactors = FALSE)
#checking the dimentions match those said on the mooc exercise pages i.e. 195 x 19
dim(human) #195 x19
head(human)
str(human) #based on head and str everything looks good. 

# Transforming the Gross National Income (GNI) character variable to numeric Using string manipulation
library(stringr) # accessing the stringr package 
human$GNI<-str_replace(human$GNI, pattern=",", replace ="")%>%as.numeric() # removing the commas from GNI and creatig a numeric version of it
str(human$GNI) #now it is numeric!:)

#keeping only some variables
names(human)
keep<-c("Country", "Sec.Edu.FMRatio", "LabourForce.FMRatio", "Eduyears.Exp", "Life.Exp", "GNI", "Mat.Mort.Ratio", "Adol.Birth.Rate", "Parl.F.Prcnt")
human <- select(human, one_of(keep)) # select the 'keep' columns
dim(human) #now we only have 9 variables as opposed to the original 19

#removing all rows with missing values
human<- filter(human, complete.cases(human)); dim(human) #we now just have 162 observations as opposed to the 195

# We remove the observations concerning regions. 
# looking at the last 10 observations of human we notice that the 7 last observations concern regions, not countries
tail(human,n=10)
last <- nrow(human) - 7 #Define the last indice we want to keep
human<- human[1:last, ] # choose everything until the last 7 observations

# we add countries as rownames
rownames(human) <- human$Country

#We delete the country column
human<-select(human,-Country)

dim(human) #155 x 8

#now we save the human data, which no longer has any missing values, regions, the country variable other than in the rownames and only includes 9 variables

###Saving the data!
write.table(human,"./human.txt",sep="\t",row.names = T) #we overwrite the week 4 version of it

###Checking we can read it back in
human<-read.table("./human.txt",sep="\t", header=T, dec=".",stringsAsFactors = FALSE)
dim(human) #155 x 8
head(human)
str(human) #perfect!

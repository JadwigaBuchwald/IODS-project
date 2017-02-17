#Jadwiga Buchwald
#Created 17. Feb. 2017
#IODS week 4: clustering and classification

setwd("C:/HY-Data/JADWIGAB/PhD_FIMM/NotesOutsideTheOffice/Courses/OpenData2017/For_IODS/IODS-project/data")


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
names(gii)<-c("GII.Rank","Country","Gender.Ineq.I","Mat.Mort.I","Adol.Birth.R", "Parl.Rep.Prcnt","Sec.Edu.F","Sec.Edu.M","LabourForce.F","LabourForce.M")

#Two new variables to gii
#The first one should be the ratio of Female and Male populations with secondary education in each country. (i.e. edu2F / edu2M). 
#The second new variable should be the ratio of labour force participation of females and males in each country (i.e. labF / labM).
gii$Sec.Edu.R<-with(gii,Sec.Edu.F/Sec.Edu.M)
gii$LabourForce.R<-with(gii,LabourForce.F/LabourForce.M)
summary(gii) #looks right


### Merging the above datasets making sure we only keep countries present in both data sets (all.x=F) 
human<-merge(hd,gii,by="Country", all.x=F)
dim(human) #195 rows and 19 columns 
names(human) #13 first variables the ones by which the merging was done. Columns 14:33 maths class versions of the other variables and 34:53 the por versions.
str(human) #characters,integers, and numeric variables. 

###Saving the data!
write.table(human,"./human.txt",quote=F,sep="\t",row.names = F)

###Checking we can read it back in
human<-read.table("./human.txt",sep="\t", header=T, dec=".",stringsAsFactors = FALSE)
head(human)
str(human) #perfect!

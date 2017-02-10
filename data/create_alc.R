#Jadwiga Buchwald
#Created 10. Feb. 2017
#IODS week 3: data wrangling and Logistic regression

setwd("C:/HY-Data/JADWIGAB/PhD_FIMM/NotesOutsideTheOffice/Courses/OpenData2017/For_IODS/IODS-project/data")


##############Wrangling
### Reading data related to secondary school student alcohol comsumption in Portugal into memory
### Data from: https://archive.ics.uci.edu/ml/machine-learning-databases/00356/ student.zip folder
mat<-read.table("student-mat.csv",sep=";",header=TRUE) #maths class data
por<-read.table("student-por.csv",sep=";",header=TRUE) #portuguese class data

### Explore the dimensions and stucture of the data
dim(mat); dim(por) #both have 33 variables, mat has 395 rows of data and por 649 rows of data
names(mat); names(por) #The two data sets contain the exact same variables i.e. columns
str(mat); str(por) #the variables are coded as factors or integers.


### Merging the above datasets by all the listed columns and making sure we only keep students present in both data sets (all.x=F) 
#and naming the duplicated variables in a logical way using the suffix statement
students<-merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"), all.x=F, suffix=c(".math",".por"))
dim(students) #382 rows and 53 columns 
names(students) #13 first variables the ones by which the merging was done. Columns 14:33 maths class versions of the other variables and 34:53 the por versions.
str(students) #factors and integers. 

### Creating a data set called `alc`: Getting rid of duplicated variables in `students` (14:53) by taking the mean answer, and when we have a non numeric variable we take the math answer 
duplicated_variables<-names(mat)[!names(mat)%in%c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")]
library(dplyr)
alc<-students[,1:13] #creating a dataset `alc` which only has the variables that were used to join `mat` and `por`

# for every column name not used for joining...
for(i in duplicated_variables) {
  # select the two columns from `alc` which represent the i:th duplicated variable
  two_columns <- select(students, starts_with(i))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[i] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[i] <- first_column
  }
}

###Creating additional variables `alc_use` and `high_use` in to the `alc` data frame
# defining a new variable, alc_use, by combining weekday and weekend alcohol use
alc$alc_use <- rowMeans(alc[,c("Dalc","Walc")])
head(alc[,c("Dalc","Walc","alc_use")]) #everything looks ok

# defining a new logical variable 'high_use'
alc$high_use <- alc$alc_use > 2
head(alc[,c("alc_use","high_use")]) #looking good

###Taking a glimpse at our new data!
glimpse(alc) # Looking good
str(alc) #variable format: Factor, int, num or logi
dim(alc) #382 x 35

###Saving the data!
write.table(alc,"./alc.txt",quote=F,sep="\t",row.names = F)

###Checking we can read it back in
alc<-read.table("./alc.txt",sep="\t", header=T, dec=".")



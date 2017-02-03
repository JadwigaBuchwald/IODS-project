#Jadwiga Buchwald
#Created 1. Feb. 2017
#IODS week 2: data wrangling and modelling

##############Wrangling
### read the data into memory
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
#looking at the data
dim(lrn14) #183 obs and 60 variables
str(lrn14) #1:59 integer formatted variables, the 60th is the factor variable, gender.

###creating data with the variables: gender, age, attitude, deep, stra, surf and points. Preparing these in the same way
#as in the DataCamp exercises, description of these also to be found here: http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt)

#scaling Attitude approapriately
lrn14$attitude<-lrn14$Attitude/10

# Access the dplyr library
library(dplyr)

#Checking my way of creating new variables and the way done in Data camp produces the same output, testing with deep:

#The way done in DataCamp:
# questions related to deep
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

#My way:
lrn14$Deep <- rowMeans(lrn14[,c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")])

#Testing they match:
with(lrn14,cor(deep,Deep)) #we get cor=1, perfect!
with(lrn14,plot(deep,Deep)) #plot shows perfect match

#Using my way to create surf and stra
lrn14$surf <- rowMeans(lrn14[,c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")])
lrn14$stra <- rowMeans(lrn14[,c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")])

# choosing the wanted columns
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
learning2014 <- select(lrn14,one_of(keep_columns))

# see the stucture of the new dataset
str(learning2014)

#renaming age and points so that all variables begin with a small letter
names(learning2014)[c(2,7)]<-c("age","points")

# select rows where points is greater than zero
learning2014 <- filter(learning2014, points > 0)
dim(learning2014) #166 x 7

summary(learning2014) #all variables are on the right scale and everything looks ok. Gender distribution remarkably uneven though.

setwd("C:/HY-Data/JADWIGAB/PhD_FIMM/NotesOutsideTheOffice/Courses/OpenData2017/For_IODS/IODS-project/data")

write.table(learning2014,"./learning2014.txt",quote=F,sep="\t",row.names = F)
learning2014<-read.table("./learning2014.txt",sep="\t", header=T, dec=".")


#############Analysis


###Drawing a scatterplot of pointsx attitude by gender
# Access the gglot2 library
library(ggplot2)

#One way:
qplot(attitude, points, data = learning2014) + geom_smooth(method = "lm") #gender hasn't yet been added

# initialize plot with data and aesthetic mapping
p1 <- ggplot(learning2014, aes(x = attitude, y = points, col= gender))

# define the visualization type (points)
p2 <- p1 + geom_point()

# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")

# add a main title and draw the plot
p4 <- p3 + ggtitle("Student's attitude versus exam points")
p4


###Drawing pairwise plots of all columns

# access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)


# create an plot matrix with ggpairs()
ggpairs(learning2014, lower = list(combo = wrap("facethist", bins = 20)))

#fancier version:
p <- ggpairs(learning2014, mapping = aes(col=gender,alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
p
#model validation graphically
# create a regression model with multiple explanatory variables
my_model2 <- lm(points ~ attitude + stra, data = learning2014)
#plots:  Residuals vs Fitted values, Normal QQ-plot and Residuals vs Leverage
par(mfrow=c(2,2))
plot(my_model2, which=c(1,2,5))


###Predicting values:
# Create model object m
m <- lm(points ~ attitude, data = learning2014)

# print out a summary of the model
summary(m)

# New observations
new_attitudes <- c("Mia" = 3.8, "Mike"= 4.4, "Riikka" = 2.2, "Pekka" = 2.9)
new_data <- data.frame(attitude = new_attitudes)

# Print out the new data
new_data

# Predict the new students exam points based on attitude
predict(m, newdata = new_data)
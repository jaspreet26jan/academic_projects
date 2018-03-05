### Exploratory Factor Analysis of bone data table 3.7 
# read the dataset into R variable 
 bone <- read.table('C:/Users/jaspr/iCloudDrive/QUARTER 6/DA 410/8/bone.txt', header = TRUE) # display the data 

#load the package (if necessary) 
 library(psych)
 
#Correlation matrix 
 corMat <- cor(bone[-1]) # display the correlation matrix 
 corMat 
#fa() to conduct an oblique principal-axis exploratory factor analysis 
 bone.factor <- fa(r = corMat, nfactors = 2, rotate = "oblimin", fm = "pa")
 bone.factor
 
#varimax rotation  
 pca_bone_rotated <- psych::principal(bone[-1], rotate="varimax", nfactors=2, scores=TRUE)
 pca_bone_rotated

 
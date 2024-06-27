#### Group project- Team 3
#### Nabanita, Yi, Elaheh, Lulu
#### MATH 6357

# --------------------------------
# Importing data
# --------------------------------
cancer = read.csv("cancer_reg.csv", header = TRUE)

# --------------------------------
# Exploring data
# --------------------------------
names(cancer)
length(unique(cancer$Geography)) # 3047; data is from 3047 counties
head(unique(cancer$Geography))
tail(unique(cancer$Geography))
unique(cancer$Geography)[3040:3047]
unique(cancer$Geography)[160:170]
sum(is.na(cancer)) # lot of Nas/missing values
nrow(na.omit(cancer)) # 591 rows with no missing values
sum(is.na(cancer$TARGET_deathRate)) # 0 missing values for the target variable
# Checking for Nas/missing values in each variables/columns
c.na = c()
for (i in (1:34)){
  result = sum(is.na(cancer[,i]))
  c.na[i] = result
}
c.na
which(c.na != 0) # columns 18, 22 & 25 have missing values
names(cancer)[c(18,22,25)] # Name of columns with missing values
sum(is.na(cancer$PctSomeCol18_24))# % residents with highest education with some college has lot of missing values (2285). 
sum(is.na(cancer$PctEmployed16_Over))# % employed over 16 has 152 missing values
sum(is.na(cancer$PctPrivateCoverageAlone))# % with private health coverage alone has 609 missing values
which(is.na(cancer$PctEmployed16_Over))
intersect(which(is.na(cancer$PctPrivateCoverageAlone)), which(is.na(cancer$PctEmployed16_Over))) 
# "intersect" gives rows where both variables have missing values
length(intersect(which(is.na(cancer$PctPrivateCoverageAlone)), which(is.na(cancer$PctEmployed16_Over)))) 
# 46 common rows with missing values for both variables

#match(intersect(list.1, list.2), list.1)#indices into list.1

# Removing missing values for necessary variables
cancer = cancer[-c(which(is.na(cancer$PctPrivateCoverageAlone))),]
cancer = cancer[-c(which(is.na(cancer$PctEmployed16_Over))),]
names(cancer)

# --------------------------------
# Exploratory Data Analysis
# --------------------------------
colnames(cancer[,c(1, 2, 6, 8, 9, 15)])
cancer = cancer[, -c(1, 2, 6, 8, 9, 15)] # Removing variables not needed
names(cancer)
head(cancer)
summary(cancer)
names(cancer)

# -------------------------------------
# maybe group by state
# ? remove unwanted characters
cancer$State = sub(".*, ", "", cancer$Geography)  
# Extract characters after pattern; throws error due to some no recognizable charac. on line 139
cancer$Geography[139]
cancer$Geography[139] = "Ana County, New Mexico"
cancer$State = sub(".*, ", "", cancer$Geography) 
length(unique(cancer$State)) #51 states including "District of Columbia"
unique(cancer$State)
as.data.frame(table(cancer$State))
sort(as.data.frame(table(cancer$State))[,2])
# Sort data using freq of data in each State
library("data.table")
df2 = setorder(as.data.frame(table(cancer$State)), Freq)
df2
#Texas has the highest data points; maybe will analyze separately
# ----------------------------------------------------------------
write.csv(cancer,"cancerEd.csv", row.names=FALSE)




##Extras
#gsub("1","",as.character(df1$x1))





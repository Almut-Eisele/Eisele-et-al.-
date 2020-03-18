



##### 0 Import data
x1<-read.table("X", sep="\t", header=TRUE)

##### 1 Table creation

# make a file per mouse and list
# adapt mouse number according to experiment
H1 <- x1[,grep("_1_", colnames(x1))]
H1<- cbind(x1[,1],H1)
H2 <- x1[,grep("_2_", colnames(x1))]
H2<- cbind(x1[,1],H2)
H3 <- x1[,grep("_3_", colnames(x1))]
H3<- cbind(x1[,1],H3)
H4 <- x1[,grep("_4_", colnames(x1))]
H4<- cbind(x1[,1],H4)
H5 <- x1[,grep("_5_", colnames(x1))]
H5<- cbind(x1[,1],H5)
H6 <- x1[,grep("_6_", colnames(x1))]
H6<- cbind(x1[,1],H6)
H7 <- x1[,grep("_7_", colnames(x1))]
H7<- cbind(x1[,1],H7)
H8 <- x1[,grep("_8_", colnames(x1))]
H8<- cbind(x1[,1],H8)
Hmouse<-list(H1, H2, H3, H4, H5, H6, H7, H8)


# Take the mean of the replicates and remove rest of values
# Adapt grep names according to experiments
Hmouse <- lapply(Hmouse, function(x) cbind(x,"Bmean"=rowMeans(x[grep("_B_", names(x))])))
Hmouse <- lapply(Hmouse, function(x) cbind(x,"Emean"=rowMeans(x[grep("E.P7", names(x))])))
Hmouse <- lapply(Hmouse, function(x) cbind(x,"Mmean"=rowMeans(x[grep("_M_", names(x))])))
Hmouse <- lapply(Hmouse, function(x) x[,-c(2:(ncol(x)-3))])


# Renormalize per barcode
for (i in 1:length(Hmouse)) {
  HBiX<-as.data.frame(prop.table(as.matrix(HBimouse[[i]][,-c(1)]),margin=1))
  HBimouse[[i]]<-cbind(HBiX, HBimouse[[i]])
  assign(paste0("HBii", i), as.data.frame(HBimouse[[i]]))
}

# make new list and rename columns
HBiimouse<-list(HBii1, HBii2, HBii3, HBii4, HBii5, HBii6, HBii7, HBii8)
HBiimouse <- lapply(HBiimouse, function(x) setnames(x, c("BmeanR","EmeanR", "MmeanR", "tag", "Bmean", "Emean", "Mmean")))


##### 2 Assign lineage bias

# Function to run on tables to assign classes based on M, E, and B lineage.

allbinarizationwDC <- function( a, y){
  
  y$progenitor<-0
  y$progenitor<-with(y, ifelse((y[,1]>a) & (y[,2]<=a) & (y[,3]<=a), "B1", y$progenitor))
  y$progenitor<-with(y, ifelse((y[,3]>a) & (y[,1]<=a) & (y[,2]<=a), "M1", y$progenitor))
  y$progenitor<-with(y, ifelse((y[,2]>a) & (y[,1]<=a) & (y[,3]<=a), "E1", y$progenitor))
  y$progenitor<-with(y, ifelse((y[,3]>a) & (y[,1]>a) & (y[,2]>a), "all1", y$progenitor))
  y$progenitor<-with(y, ifelse((y[,1]>a) & (y[,3]>a) & (y[,2]<=a), "MB1", y$progenitor))
  y$progenitor<-with(y, ifelse((y[,2]>a) & (y[,3]>a) & (y[,1]<=a), "ME1", y$progenitor))
  y$progenitor<-with(y, ifelse((y[,1]>a) & (y[,2]>a) & (y[,3]<=a), "BE1", y$progenitor))
  y$progenitor<-with(y, ifelse((y[,1]<=a) & (y[,2]<=a) & (y[,3]<=a), "low1", y$progenitor))
  
  y<-y[,-c(1:3)]
  y<-y[,-c(2:4)]
  return(y)
}


# run the function on a list of dataframes
X<- lapply(HBiimouse, function(x) allbinarizationwDC(0.1,x))

# Merge result with initial dataframe
for (i in 1:length(HBiimouse)) {
  M<-merge(X[[i]],HBiimouse[[i]], all=T)
  assign(paste0("H", i), as.data.frame(M))
}

# Make one big table out of it and assign conditions
# Adapt number of mice according to experiment
# Adapt Condition values according to experiment
NORM<-gdata::combine(H1, H2, H3, H4, H5, H6, H7, H8)

NORM$Condition<- 0
NORM<- within(NORM, Condition[source == "H1"]<- "No EPO")
NORM<- within(NORM, Condition[source == "H2"]<- "No EPO")
NORM<- within(NORM, Condition[source == "H4"]<- "No EPO")
NORM<- within(NORM, Condition[Condition == "0"]<- "EPO")

####### The resulting table can be used to make plots 













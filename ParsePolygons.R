library(rvest)
url <- "enter file path here"
#Make sure all the coordinates are evenly spaced in the text file

webpage <- read_html(url)

area<-html_nodes(webpage,'area') #get text between the <area> tags
coords<-html_attr(area, "coords") #get coordinates for each <area>

#initialize vectors to store coordinate values
x = vector()
y = vector()
unit = vector()

#Function to get X coordinates
getX<- function(x) {
  
  xy<-unlist(strsplit(x," "))
  pairs <- sapply(xy,function(x) gsub("(\\,.*?)\\,","\\1",x))
 
  as.numeric(sapply(pairs,function(x) unlist(strsplit(x,","))[1]))
  
}

#Function to get Y coordinates
getY<- function(x) {
  xy<-unlist(strsplit(x," "))
  pairs <- sapply(xy,function(x) gsub("(\\,.*?)\\,","\\1",x))
  
  as.numeric(sapply(pairs,function(x) unlist(strsplit(x,","))[2]))
}

xval <- sapply(coords,getX)
yval <- sapply(coords,getY)

index <- 0
#Function to get Polygon unit (unique no. given to each polygon)
getUnit<- function(x) {
  
  index <<- index + 1 #global variable
  u <- as.vector(rep(index,lengths(xval[index])))
  unit <- append(unit,u)
}

unit <-sapply(coords,getUnit)

xval <- unlist(xval)
yval <- unlist(yval)
unit <- unlist(unit)

#create a data frame
grid<-as.data.frame(cbind(Unit = as.vector(unit),Xval = as.vector(xval),Yval = as.vector(yval)))

#Initialize Path variable
grid$Path <- 0

#Generate values for Path
for(u in grid$Unit){
  
  grid$Path[grid$Unit == u] <- seq(1:length(grid$Unit[grid$Unit == u]))
  
}

#Output to csv
write.csv(grid,"sachin_grid.csv")

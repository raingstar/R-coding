######################################################################################################################################################################################################################
#The goal of this competition is to predict which place a person would like to check in to. For the purposes of this competition, Facebook created an artificial world consisting of more than 100,000 places located in a 10 km by 10 km square. For a given set of coordinates, your task is to return a ranked list of the most likely places. Data was fabricated to resemble location signals coming from mobile devices, giving you a flavor of what it takes to work with real data complicated by inaccurate and noisy values. Inconsistent and erroneous location data can disrupt experience for services like Facebook Check In.
#Evaluation :Submissions are evaluated according to the Mean Average Precision @3
    ###################################################################################################
###################################################################################################################
# facebook final code:
# Load package
rm(list=ls()) # clear the envirment
gc() # refresh the memory
library(readr)
library(data.table) # ME = MEMORY HUNGRY
library(dplyr)
library(qdap)
library(ggplot2)
#library(class)
library(plotly)
library(FNN)
eps=0.00001
x_cut=20  # Split the X-coordinate 20 pieces
y_cut=20  # Split the Y-coordinate 20 pieces
x_bound=0.04 # Comparable to the mean deviation of X
y_bound=0.01 # Comparable to the mean deviation of Y
N=19      # Magic number for KNN method, chosen from pre-validate the training data.
w=c(2,1/100,1/2500,1/120,1/1100,1/250,1/50) # Magic weights for each features [y, hour of the day (hour), accuracy, day of the week (wd), day of the year(day), month of the year(month),year (0,1))]. This weight was tunned via the training data.
pred=NULL  # Define a predication 
#########################################################
system.time(train <- data.table(read_csv('**/facebook/train.csv')))[[3]] #load the train data, count the system time.
train[,max_time:=max(time),by=place_id] #For each place_id, find its latest check in time. Filter the inactivity (closed) place_id later
train[,':='(hour=(time/60)%%24,wd=floor((time/(60*24))%%7),day=floor((time/(60*24))%%365),month=floor((time/(60*24*30))%%12),year=floor(time/(60*24*365)))]# Define new features: hour=hour of the day(0-23), wd=day of the week(0-6), day=day of the year (0-364), month=month of the year (0-11), year (0,1).
system.time(test <- data.table(read_csv('/Users/Raines/Downloads/facebook/test.csv')))[[3]]#load the test data, count the system time.
test[,':='(hour=(time/60)%%24,wd=floor((time/(60*24))%%7),day=floor((time/(60*24))%%365),month=floor((time/(60*24*30))%%12),year=floor(time/(60*24*365)),grid_num=floor(abs(x-eps)*x_cut/10)*y_cut+floor(abs(y-eps)*y_cut/10))] # Define new features at test data too. If we are doing grid-search, labor its grid_num at test data. 
train_new<-train[max_time>743000] # Filter the inactivity place (no check in within the last month), and save it into train_new.
train<-NULL # Delete train to save memory space. 
gc() # Clear the memory
train_new=preparedata(train_new,w)
test=preparedata(test,w) # Using user defined preparedata function to pretreat the train and test dataset.
time.start=proc.time()[[3]] # Monitor the algorithm running time.
##Next we are doing a grid search to improve the predict accuracy.
for (i in 1:x_cut){
  for (j in 1:y_cut){
    train_new %>% filter(x>=(i-1)*10/x_cut-x_bound,x<=(i)*10/x_cut+x_bound,y>=2*((j-1)*10/y_cut-y_bound),y<=2*((j)*10/y_cut+y_bound)) ->train_grid # Split the whole dataset (10*10 place) into 40 small pieces (train_grid). x_bound and y_bound allow a little bit overlap for each pieces.   
    train_grid[,place_count:=.N,by=place_id] # For each place_id, calculate its total history check-in times (place_count).
    train_grid<-train_grid[place_count>=5] # Filter rare check-ins.
    test_grid<-test[grid_num==(i-1)*y_cut+(j-1)] # Chose the corresponding test_grid dataset.
    pred=rbindlist(list(pred,Knn_predict(train_grid,test_grid,w))) # Update the final prediction using KNN method. We can modify the Knn_predict to change the distancefunction and the corresponding weight for distance. 
  }
}
time.stop1=proc.time()[[3]] # Monitor the process time.
pred=pred[,head(.SD,1),row_id] # Get rid of duplicate prediction.
pred[,row_id:=as.integer(row_id)] # Change the result datatype.
write.csv(pred,file="submit.csv",row.names = F) # Wrote the prediction into a csv file.
# Time monitor
time.stop2=proc.time()[[3]] 
print(paste('The process time is:',round(time.stop1-time.start,2),'seconds'))
print(paste('The total time is:',round(time.stop2-time.start,2),'seconds'))

######################################################################################################################################################################################################################
#Relative functions
###########################################################################################################
#Define a preparedata function to rescale dataset features and delete unnecessary features.
preparedata<-function(data,w){
  data<-data[,':='(x=x,y=y*w[1],hour=hour*w[2],accuracy=accuracy*w[3],wd=wd*w[4],day=day*w[5],month=month*w[6],year=year*w[7])]
  data[,':='(time=NULL,max_time=NULL)]
  return(data)
}
###########################################################################################################
#KNN prediction funtion. This one use the default knn method (equal weight on nearest neighbors).
Knn_predict1=function(train_data,test_data)
{
  id<-test_data$row_id # Save the row id
  place<-as.factor(train_data[,place_id])# Save the place_id
  #Keep only the related features in train_data and test_data
  train_data<-train_data[,.(x,y,hour,accuracy,wd,day,month,year)]
  test_data<-test_data[,.(x,y,hour,accuracy,wd,day,month,year)] 
  system.time(test_predict<-knn(train=train_data,test=test_data,cl=place,k=N)) # Find the nearest neighbors
  test_neighbor_loc<-attr(test_predict,'nn.index') # Got nearest neighbors index
  test_neighbor<-apply(test_neighbor_loc,2,function(col){place[col]}) # Find the nearest neighbors' place_id from training data. 
  id<-rep(id,N)
  m<-cbind(as.vector(test_neighbor),as.numeric(id))
  colnames(m)=c('place_id','id')
  m<-as.data.table(m)
  m%>% group_by(id,place_id) %>% summarise(count=n()) %>% top_n(n=3,wt=count)->m1 # Show the top 3 nearest neighbors.
  rm(m,test_neighbor,test_neighbor_loc) # Remove old results.
  gc()
  setorder(m1,id,-count)
  m1<-m1[,.SD[,paste(place_id,collapse = ' ')],by=id]
  names(m1)<-c('row_id','place_id')
  return(m1)
}
###########################################################################################################
#KNN prediction funtion. Similar to Knn_predict1,
Knn_predict=function(train_data,test_data,w)
{
  id<-test_data$row_id # save the row id
  place<-as.factor(train_data[,place_id])# Save the place_id
  #Keep only the related features in train_data and test_data
  train_data<-train_data[,.(x,y,hour,accuracy,wd,day,month,year)]
  test_data<-test_data[,.(x,y,hour,accuracy,wd,day,month,year)]
  system.time(test_predict<-knn(train=train_data,test=test_data,cl=place,k=N))# Using KNN to find the nearest neighbors
  test_neighbor_loc<-attr(test_predict,'nn.index')# Got the nearest neighbors index.
  test_neighbor<-apply(test_neighbor_loc,2,function(col){place[col]})
  test_dist<-attr(test_predict,'nn.dist')# Got the nearest neighbors distance.
  id<-rep(id,N)
  m<-cbind(as.vector(test_neighbor),as.vector(test_dist),as.numeric(id))
  colnames(m)=c('place_id','dist','id')
  m<-as.data.table(m)
  m[,dist:=as.numeric(dist)]
  m[,dist:=1/dist] # Assign higher weight for closer neighbors. Can chose other function.
  m %>% group_by(id,place_id) %>% summarise(count=sum(dist)) %>% top_n(n=3,wt=count)->m1# Show the top 3 nearest neighbors.
  rm(m,test_neighbor,test_neighbor_loc,test_dist)
  gc()
  setorder(m1,id,-count)
  m1<-m1[,.SD[,paste(place_id,collapse = ' ')],by=id]
  names(m1)<-c('row_id','place_id')
  return(m1)
}

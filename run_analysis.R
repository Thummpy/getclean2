#step one

#create x set
xtest<-read.table("test/X_test.txt")
xtrain<-read.table("train/X_train.txt")
xSet<-rbind(xtest,xtrain)
rm(xtest);rm(xtrain)

#create y set
ytest<-read.table("test/y_test.txt")
ytrain<-read.table("train/y_train.txt")
ySet<-rbind(ytest,ytrain)
rm(ytest);rm(ytrain)

#create subjects
subTrain<-read.table('train/subject_train.txt')
subTest<-read.table('test/subject_test.txt')
subSet<-rbind(subTest,subTrain)
rm(subTest);rm(subTrain)

#get remaining data
feats<-read.table('features.txt')
act<-read.table('activity_labels.txt')
colnames(act) = c('activityID','activityType')

#combine sets into full set
colnames(xSet)<-as.character(feats[,2])
colnames(ySet)<-"activityID"
colnames(subSet)<-"subjectID"
fullSet<-cbind(xSet,ySet,subSet)
rm(xSet);rm(ySet);rm(subSet)

#set names
colnames(fullSet)<-cbind(t(as.character(feats[,2])),"activityID","subjectID")
rm(feats)
print(nrow(fullSet$activityID))

#step 2
cn <- colnames(fullSet); 
trim<-(grepl("activity|subject|mean\\(|std\\(",cn) )
fullSet<-fullSet[trim]

#step 3
fullSet<-merge(fullSet,act,by='activityID',all.x=TRUE)
cn <- colnames(fullSet)

#step 4
for (n in 1:length(cn)){
  cn[n]= gsub("\\(\\)","",cn[n])
  cn[n]= gsub("-mean","Mean",cn[n])
  cn[n]= gsub("-std$","StdDev",cn[n])
  cn[n]= gsub("^(t)","time",cn[n])
  cn[n]= gsub("^(f)","freq",cn[n])
  cn[n]= gsub("([Gg]ravity)","Gravity",cn[n])
  cn[n]= gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",cn[n])
  cn[n]= gsub("[Gg]yro","Gyro",cn[n])
  cn[n]= gsub("AccMag","AccMagnitude",cn[n])
  cn[n]= gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",cn[n])
  cn[n]= gsub("JerkMag","JerkMagnitude",cn[n])
  cn[n]= gsub("GyroMag","GyroMagnitude",cn[n])
}
colnames(fullSet)<-cn

#Step 5
dataAg<-fullSet[,names(fullSet)!="activityType"];
dataAg<-aggregate(dataAg[,names(dataAg)!=c("activityID","subjectID")],by=list(dataAg$activityId,dataAg$subjectID),mean);
dataAg<-merge(dataAg,act,by='activityID',all.x=TRUE)

write.table(dataAg, "HAR_tidy.txt",row.names=TRUE,sep="\t")

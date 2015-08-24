#1
library(XML)
tags = xmlParse("tags.xml") 
tags_list =  t(xmlSApply(xmlRoot(tags), xmlAttrs)) 
len<-length(tags_list)
counts=rep(0,len)
Tags=rep("0",len)
for(i in 1:len){
	counts[i]=tags_list[[i]]["Count"]
	Tags[i]=tags_list[[i]]["TagName"]
}

tags_data<-data.frame(count=as.numeric(counts),Tag=Tags)
re<-tags_data[order(tags_data$count),]
tag5<-as.character(re[len-4,2])

posts = xmlParse("posts.xml") 
posts_list =  t(xmlSApply(xmlRoot(posts), xmlAttrs))
len<-length(posts_list)
Tags=rep("0",len)
for(i in 1:len){
	Tags[i]=posts_list[[i]]["Tags"]
}
tag_data<-data.frame(Tag=Tags,stringsAsFactors=FALSE)
table(grepl(tag5,tags_data$Tag))
options(digits=10)
False<-89633
True<-2342
True/(True+False)

#2
ques<-0
ans<-0
for(i in 1:len){
	if(posts_list[[i]]["PostTypeId"]==1) ques<-ques+1
	if(posts_list[[i]]["PostTypeId"]==2) ans<-ques+1
}
qscore<-rep(0,ques)
ascore<-rep(0,ans)
q<-1
a<-1
for(i in 1:len){
	if(posts_list[[i]]["PostTypeId"]==1) {
		qscore[q]<-posts_list[[i]]["Score"]
		q<-q+1}
	if(posts_list[[i]]["PostTypeId"]==2) {
		ascore[a]<-posts_list[[i]]["Score"]
		a<-a+1}
}
mean(as.numeric(ascore))-mean(as.numeric(qscore))

idnum<-0
for(i in 1:len){
	if(!is.na(posts_list[[i]]["OwnerUserId"])) idnum<-idnum+1
}
Ids<-rep(0,idnum)
Scores<-rep(0,idnum)
n<-1
for(i in 1:len)	{
	if(!is.na(posts_list[[i]]["OwnerUserId"])){
		Ids[n]<-posts_list[[i]]["OwnerUserId"]
		Scores[n]<-posts_list[[i]]["Score"]
		n<-n+1}}
id_score<-data.frame(Id=Ids,Score=Scores,stringsAsFactors=FALSE)

users = xmlParse("Users.xml") 
users_list =  t(xmlSApply(xmlRoot(users), xmlAttrs))[,-1] 
user_len<-length(users_list)
users2<-rep(0,user_len)
rep<-rep(0,user_len)
for(i in 1:user_len){
		users2[i]<-users_list[[i]]["Id"]
		rep[i]<-users_list[[i]]["Reputation"]
}
id_rep<-data.frame(Id=users2,Rep=rep,stringsAsFactors=FALSE)
rep_score=merge(id_score,id_rep,by="Id",all=FALSE)

rep_score[,2]<-as.numeric(rep_score[,2])
rep_score[,3]<-as.numeric(rep_score[,3])
fit <- lm(Rep ~ Score, data=rep_score)
cor.test( rep_score$Rep,rep_score$Score, method = "pearson", alternative = "greater")

#3
FavoriteCount
accept_id<-rep(0,ques)
parent_id<-rep(0,ans)
q<-1
a<-1
for(i in 1:len){
	if(posts_list[[i]]["PostTypeId"]==1) {
		accept_id[q]<-posts_list[[i]]["AcceptedAnswerId"]
		q<-q+1}
	if(posts_list[[i]]["PostTypeId"]==2) {
		parent_id[a]<-posts_list[[i]]["ParentId"]
		a<-a+1}
}
accept_id<-data.frame(Id=accept_id)
parent_id<-data.frame(Id=parent_id)

up<-rep(0,user_len)
for(i in 1:user_len){
		up[i]<-users_list[[i]]["UpVotes"]
}
id_up<-data.frame(Id=users2,Up=up,stringsAsFactors=FALSE)
accept_up=merge(accept_id,id_up,by="Id",all=FALSE)
parent_up=merge(parent_id,id_up,by="Id",all=FALSE)
mean(as.numeric(parent_up$Up))-mean(as.numeric(accept_up$Up))

#4
creatt<-rep(0,ques)
post_id<-rep(0,ques)
q<-1
for(i in 1:len){
	if(posts_list[[i]]["PostTypeId"]==1) {
		post_id[q]<-posts_list[[i]]["Id"]
		creatt[q]<-posts_list[[i]]["CreationDate"]
		q<-q+1}
}
library(lubridate)
creatt<-ymd_hms(creatt)
postid<-data.frame(post_id,creatt)

comments = xmlParse("Comments.xml") 
comments_list =  t(xmlSApply(xmlRoot(comments), xmlAttrs)) 
com_len<-length(comments_list)
com_id<-rep(0,com_len)
creat_com<-rep(0,com_len)
for(i in 1:com_len){
	com_id[i]<-comments_list[[i]]["PostId"]
	creat_com[i]<-comments_list[[i]]["CreationDate"]
}
creat_com<-ymd_hms(creat_com)
comment<-data.frame(com_id,creat_com)

mergy_time<-merge(comment,postid,by.x="com_id",by.y="post_id",all = FALSE)
time<-mergy_time$creat_com-mergy_time$creatt
mergy_time<-data.frame(mergy_time,time)

library(dplyr)
mergy_id<-group_by(mergy_time,com_id)
mergy_id<-summarize(mergy_id,time=mean(time,na.rm=TRUE),creatt=mean( creat_com,na.rm=TRUE))
mergy_id2<-data.frame(mergy_id)
mhour<-strftime(mergy_id2$creatt, "%H")
mergy_id2<-data.frame(mergy_id2,mhour)
mergy_id3<-group_by(mergy_id2,mhour)
mergy_id4<-summarize(mergy_id3,time=mean(time,na.rm=TRUE))
(max(mergy_id4$time)-min(mergy_id4$time))/60/60

tree_plot<-function(dataset,title,fontcolor="black",fontname = "helvetica",arrowcolor="black",boxcolor="black"){
require(data.tree)
dataset=dataset$frame[,c("node","var","xval","n","mean","end")]
dataset$mean=round(dataset$mean,2)
dataset$xval=round(as.numeric(dataset$xval),2)
dataset=dataset[order(dataset[,"node"]),]
require(dplyr)
dataset=dataset%>%mutate(name=ifelse(dataset$node%%2==0,paste(dataset$var,"<=",dataset$xval),paste(dataset$var,">",dataset$xval)))
dataset$name[1]<-paste("n=",dataset$n[1])
dataset$parentname=title
for(i in c(2:length(dataset[,1]))){
  dataset$parentname[i]=dataset$name[dataset$node==round((dataset$node[i]-0.1)/2)]
}
newdata=dataset
ak=c(2:length(dataset[,1]))
newdata$parentname[ak[dataset$end[ak]=="E"]]=dataset$name[ak[dataset$end[ak]=="E"]]
newdata$name[ak[dataset$end[ak]=="E"]]=paste("n=",dataset$n[ak[dataset$end[ak]=="E"]],",","mean=",dataset$mean[ak[dataset$end[ak]=="E"]],"mm Hg")
newdata1=rbind(dataset,newdata)

dat_network <- newdata1[,c("name", "parentname")]
dat_tree <- FromDataFrameNetwork(dat_network, check = "check")

SetGraphStyle(dat_tree, rankdir = "TB")
SetEdgeStyle(dat_tree, arrowhead = "vee", color = arrowcolor, penwidth = 2)
SetNodeStyle(dat_tree, style = "filled,rounded", shape = "box",
             fontname = fontname, tooltip = GetDefaultTooltip,color=boxcolor,fontcolor=fontcolor,penwidth=1)
plot(dat_tree)
}

#The dataset in this Function is the name of tree you generate, this function could assign the fontcolor,boxcolor and arrowcolor.
#for example the dia_tree_mn3.



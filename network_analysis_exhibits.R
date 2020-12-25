
library(igraph)
library(RSQLite)
source("E:/__data_cleaning_tools/header.R")

con <- dbConnect(SQLite(), "E:/db/ohlc.sqlite")

### get universe
#"SH000300"300/ "SH000905"500/ "SH000906"800/
index_name <-"SH000300" 
index_weight <- dbSendQuery(con,paste0("select Date,Code,",index_name," from index_wei where date > '2010-01-01';"))
index_all = dbFetch(index_weight)
dbClearResult(index_weight)
dbDisconnect(con)
index_all=na.omit(index_all)

#count=universe%>%group_by(Date,.add=T)%>%summarise(n=n())%>%as.data.frame()

### get daily return data
trt=closert_xts('2013-01-01','T')

#get eom
dates=data.frame(Date=unique(trt$Date))
dates$eom=substr(dates$Date,1,7)
dates[!duplicated(dates$eom,fromLast = T),'is_eom']=1

#get eom index constituents
index_all$Date=as.Date(index_all$Date)
indexcon=merge.data.frame(trt[,1:2],dates,all.x = T)
mark = indexcon$Date[1]
for (each in unique(indexcon$Date)){
  
  if (each%in%index_all$Date){
    DateCode <- index_all[index_all$Date==each,]
    DateCode <- na.omit(DateCode)
    mark = each
  }else{
    DateCode <- index_all[index_all$Date==mark,]
    DateCode <- na.omit(DateCode)
  }
  indexcon[(indexcon$Date==each)&(indexcon$Code %in%DateCode$Code),'index']=1
}
indexcon1=na.omit(indexcon)

count=indexcon1%>%group_by(Date,.add=T)%>%summarise(n=n())%>%as.data.frame()

#i= unique(indexcon1$Date)[1]
### create adjacency matrix
weights=NULL
for(i in unique(indexcon1$Date)){
  yyy=trt[(trt$Date %between% c(as.Date(i-360),as.Date(i)))&(trt$Code %in% indexcon1[indexcon1$Date==i,'Code']),]
  yyy=spread(yyy,Code,pret)
  adj_matrix=as.data.frame(cor(yyy[,-1],use = 'pairwise.complete.obs',method = "spearman"))
  adj_matrix=sqrt(2*(1-adj_matrix))#convert correlation to distance measure
  adj_matrix$from_code=rownames(adj_matrix)
  adj_matrix=gather(adj_matrix,to_code,weight,-from_code)
  adj_matrix=adj_matrix[(adj_matrix$from_code!=adj_matrix$to_code)&(adj_matrix$weight!=0),]
  adj_matrix=na.omit(adj_matrix)
  g <- graph_from_data_frame(adj_matrix, directed = F)
  g1<-mst(g)
  
  yyy=trt[(trt$Date %between% c(as.Date(i-90),as.Date(i)))&(trt$Code %in% indexcon1[indexcon1$Date==i,'Code']),]
  yyy=spread(yyy,Code,pret)
  adj_matrix=as.data.frame(cor(yyy[,-1],use = 'pairwise.complete.obs',method = "spearman"))
  adj_matrix=sqrt(2*(1-adj_matrix))#convert correlation to distance measure
  adj_matrix$from_code=rownames(adj_matrix)
  adj_matrix=gather(adj_matrix,to_code,weight,-from_code)
  adj_matrix=adj_matrix[(adj_matrix$from_code!=adj_matrix$to_code)&(adj_matrix$weight!=0),]
  adj_matrix=na.omit(adj_matrix)
  g <- graph_from_data_frame(adj_matrix, directed = F)
  g2<-mst(g)
  
  yyy=trt[(trt$Date %between% c(as.Date(i-120),as.Date(i)))&(trt$Code %in% indexcon1[indexcon1$Date==i,'Code']),]
  yyy=spread(yyy,Code,pret)
  adj_matrix=as.data.frame(cor(yyy[,-1],use = 'pairwise.complete.obs',method = "spearman"))
  adj_matrix=sqrt(2*(1-adj_matrix))#convert correlation to distance measure
  adj_matrix$from_code=rownames(adj_matrix)
  adj_matrix=gather(adj_matrix,to_code,weight,-from_code)
  adj_matrix=adj_matrix[(adj_matrix$from_code!=adj_matrix$to_code)&(adj_matrix$weight!=0),]
  adj_matrix=na.omit(adj_matrix)
  g <- graph_from_data_frame(adj_matrix, directed = F)
  g3<-mst(g)
  
  yyy=trt[(trt$Date %between% c(as.Date(i-240),as.Date(i)))&(trt$Code %in% indexcon1[indexcon1$Date==i,'Code']),]
  yyy=spread(yyy,Code,pret)
  adj_matrix=as.data.frame(cor(yyy[,-1],use = 'pairwise.complete.obs',method = "spearman"))
  adj_matrix=sqrt(2*(1-adj_matrix))#convert correlation to distance measure
  adj_matrix$from_code=rownames(adj_matrix)
  adj_matrix=gather(adj_matrix,to_code,weight,-from_code)
  adj_matrix=adj_matrix[(adj_matrix$from_code!=adj_matrix$to_code)&(adj_matrix$weight!=0),]
  adj_matrix=na.omit(adj_matrix)
  g <- graph_from_data_frame(adj_matrix, directed = F)
  g4<-mst(g)
  
  #centrality=data.frame(centrality=alpha_centrality(g))
  centrality=data.frame(egc=closeness(g1),ect=closeness(g2),btn=closeness(g3),cln=closeness(g1))
  centrality$Code=rownames(centrality)
  centrality$Date=i
  rownames(centrality)=NULL
  vola=data.frame(lapply(yyy[,-1],sd))%>%gather(Code,vola)
  vola$Code=substr(vola$Code,2,10)
  centrality=merge.data.frame(centrality,vola)
  print(as.Date(i))
  weights=rbind(weights,centrality)
}
weights$Date=as.Date(weights$Date)
#weights$weight=weights$centrality
count=weights%>%group_by(Date,.add=T)%>%summarise(n=n())%>%as.data.frame()





yyy=trt[(trt$Date %between% c(as.Date(i-365),as.Date(i)))&(trt$Code %in% indexcon1[indexcon1$Date==i,'Code']),]
yyy=spread(yyy,Code,pret)
adj_matrix=as.data.frame(cor(yyy[,31:52],use = 'pairwise.complete.obs',method = "spearman"))
adj_matrix=sqrt(2*(1-adj_matrix))#convert correlation to distance measure
adj_matrix$from_code=rownames(adj_matrix)
adj_matrix=gather(adj_matrix,to_code,weight,-from_code)
adj_matrix=adj_matrix[(adj_matrix$from_code!=adj_matrix$to_code)&(adj_matrix$weight!=0),]
adj_matrix=na.omit(adj_matrix)

g <- graph_from_data_frame(adj_matrix, directed = F)
g1<-mst(g)


ex3=weights%>%group_by(Date,.add=T)%>%summarise(m_egc=mean(egc),m_ect=mean(ect),
                                                m_btn=mean(btn),m_cln=mean(cln))%>%as.data.frame()
plot(ex3$Date,ex3$m_cln,type='l')


plot(g,vertex.color="red1",vertex.size=8,edge.arrow.size=0.3,
     vertex.label.cex=0,vertex.label.dist=0.7)
plot(g1,vertex.color="red1",vertex.size=8,edge.arrow.size=0.3,
     vertex.label.cex=0,vertex.label.dist=0.7)


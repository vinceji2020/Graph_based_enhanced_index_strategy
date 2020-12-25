
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
  #centrality=data.frame(centrality=alpha_centrality(g))
  centrality=data.frame(egc=eigen_centrality(g1,weights = E(g1)$weight)$vector,
                        deg=degree(g1,normalized = T),
                        ect=eccentricity(g1),
                        btn=betweenness(g1,weights =E(g1)$weight,normalized = T),
                        cln=closeness(g1,weights =E(g1)$weight))
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

weights$eom=substr(weights$Date,1,7)
eom_shift <-function(x){
  if(substr(x,5,6)==12){
    eom=as.numeric(gsub('-','',x))+100-11
    return(paste0(substr(eom,1,4),'-',substr(eom,5,6)))
  }
  else{
    eom=as.numeric(gsub('-','',x))+1
    return(paste0(substr(eom,1,4),'-',substr(eom,5,6)))
  }
}

#weights$eom=apply(weights[,'eom',drop=F],1,eom_shift)

weights1=weights[,c('eom','Code','egc','deg','btn','cln','vola','ect')]
centrality='deg'
counts=weights%>%group_by(Date,.add=T)%>%summarise(max=max(get(centrality)),min=min(get(centrality)),mean=mean(get(centrality),na.rm=T),n=n())%>%as.data.frame()
plot(counts$Date ,counts$mean,type='l',col='blue')

for(i in unique(weights1$eom)){
  a=max(weights1[weights1$eom==i,'ect'])
  b=min(weights1[weights1$eom==i,'ect'])
  weights1[weights$eom==i,'ect1']=(weights1[weights$eom==i,'ect']-b)/(a-b)
}
centrality='egc'
counts=weights1%>%group_by(eom,.add=T)%>%summarise(max=max(get(centrality)),min=min(get(centrality)),mean=mean(get(centrality)),n=n())%>%as.data.frame()


trt10=closert_xts_mon('2013-01-01')
###assign weights to daily returns
trt10$eom=substr(trt10$Date,1,7)
trt1=merge.data.frame(trt10,weights1, all.x = T)
#trt1$centralityinv=1/trt1$centrality
#trt1[is.infinite(trt1$centralityinv),"centralityinv"]=0
trt1=na.omit(trt1)
count=trt1%>%group_by(Date,.add=T)%>%summarise(n=n())%>%as.data.frame()

portfolio_rt=trt1%>%group_by(Date,.add=T)%>%summarise(hs300_rt=mean(fret),cln_rt=weighted.mean(fret,cln/vola),deg_rt=weighted.mean(fret,deg/vola),btn_rt=weighted.mean(fret,btn/vola))%>%as.data.frame()
#xxx=weighted.mean(trt1$pret,trt1$centralityinv)
#min(trt1$centralityinv)
write.csv(portfolio_rt,"E:/projects/network_analysis/report/centrality_ret.csv")
colnames(portfolio_rt)=c('HS300','Closeness','Degree','Betweenness')
cumrt_group = plot_ret(portfolio_rt1)
cumrt_group

portfolio_rt1 = cumprod(portfolio_rt[,-1]+1)-1

rownames(portfolio_rt1)=portfolio_rt[,1]

portfolio_rt1$closeness_spread=portfolio_rt1$Closeness-portfolio_rt1$HS300
portfolio_rt1$degree_spread=portfolio_rt1$Degree-portfolio_rt1$HS300
portfolio_rt1$btn_spread=portfolio_rt1$Betweenness-portfolio_rt1$HS300

cumrt_group = plot_ret(portfolio_rt1[,5:7])
cumrt_group

#plot(portfolio_rt1$Date,portfolio_rt1$spread,type='l',col='blue')


plot(portfolio_rt1$Date,portfolio_rt1$p_rt,type='l',col='blue')
lines(portfolio_rt1$Date,portfolio_rt1$mean_rt,col='red')
lines(portfolio_rt1$Date,portfolio_rt1$p1_rt,col='green')

lines(portfolio_rt1$Date,portfolio_rt1$spread1,col='black')
lines(portfolio_rt1$Date,portfolio_rt1$spread2,col='black')



######test as factor
factor=trt1
market_ret = group_by(trt1,Date,add=T) %>%
  summarize(mret=mean(fret)) %>%
  as.data.frame()
market_ret
####put everything in factor dataset
factor=merge.data.frame(factor,market_ret,all.x=T)

quantiles = c(0,0.2,0.40,0.60,0.80,1)
quantiles = c(0,0.8,1)
q_names = c(1,2)
mark = factor$Date[1]
t=c(0,0,0,0,0,0)
v="cln"
### rank alpha
for (each in unique(factor$Date)){
  
  t = factor[(factor$Date==each),]
  factor[(factor$Date==each),'factor_quantile'] = cut(t[,v],quantile(t[,v],names=FALSE,na.rm=T,probs=quantiles),labels=q_names,include.lowest = T)
  factor[(factor$Date==each),'rret'] <-  factor[(factor$Date==each),'fret']- factor[(factor$Date==each),'mret']
  print(quantile(t[,v],names=FALSE,na.rm=T,probs=quantiles))
  
}
factor=na.omit(factor)
factornames = names(factor)
factor_stats = group_by(factor,Date,factor_quantile,.add=T) %>%
  summarize(n=n(),
            mean=mean(get(v)),
            sd=sd(get(v)),
            min=min(get(v)),
            max=max(get(v))) %>%
  as.data.frame()
factor_stats
ttt=spread(factor_stats[,c('Date','factor_quantile','sd')],factor_quantile,sd)
rownames(ttt)=ttt$Date
colnames(ttt)=c('Date','Low Centrality','High Centrality')
ttt1=plot_ret(ttt[,-1])
ttt1

ret.table = group_by(factor,Date,.add=T) %>%
  group_by(factor_quantile,.add=T) %>%
  summarize(ret = mean(fret,na.rm = T)) %>%
  as.data.frame()
ret.table.mean = group_by(ret.table,factor_quantile,.add=T) %>%
  summarize(ret = mean(ret,na.rm = T)) %>%
  as.data.frame()
ret.table.mean

ret.table.mean = group_by(factor,factor_quantile,.add=T) %>%
  summarize(ret = mean(fret,na.rm = T)) %>%
  as.data.frame()
ret.table.mean

cum.ret=spread(ret.table,factor_quantile,ret)
cum.ret1 = cumprod(cum.ret[,-1]+1)-1
rownames(cum.ret1)=cum.ret$Date

cumrt_group = plot_ret(cum.ret1)
cumrt_group


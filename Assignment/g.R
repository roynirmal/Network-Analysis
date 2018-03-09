library(igraph)

data = read.csv("Data_Highschool.txt", sep="\t")

datanocol = data[,c("i", "j")]
dataNoDup = datanocol[!duplicated(datanocol),]

g <- graph_from_data_frame(dataNoDup,directed = FALSE)
N = gorder(g)
ecount(g)
edge_density(g)
mean(degree_distribution(g))

hist(degree(g), breaks = 19)

D = as.numeric(names(sort(degree(g),decreasing=TRUE))) 

assortativity.degree(g)

transitivity(g,type= "local")
#C = order(transitivity(g,type= "local"), decreasing =TRUE)
C = as.numeric(names(sort(cl,decreasing=TRUE))) 

A = as_adj(g, type= "both")

ev <-eigen(A)$values
max(ev)

L <- laplacian_matrix(g)
evL <-eigen(L)$values
sort(evL)[2]

mean_distance(g, directed = FALSE)

diameter(g)


data[data$t ==1,]

tempdata = data[data$t ==1,]




remove(finaldf)
remove(countdf)
finaldf <- data.frame(Mother = integer(), Node = integer(), Time = integer(), stringsAsFactors = FALSE)
countdf <- data.frame(Mother = integer(), Time = integer(), Count = integer(), stringsAsFactors = FALSE)
for (m in 1:4){
remove(inf)
inf <-data.frame( Mother = m, Node =m, Time = 0, stringsAsFactors = FALSE)

count = 0;
for (t in 1:50){
tempdata = data[data$t ==t,]


for (list in inf$Node){
for(j in 1:nrow(tempdata)){
  row = tempdata[j,]
  if(row$i == list){
    if(!row$j %in% inf$Node){
    inf[nrow(inf)+1,]= c(m, row$j, tempdata$t)
    count= count + 1}
  } else if(row$j == list){
    if(!row$i %in% inf$Node){
    inf[nrow(inf)+1,]= c(m, row$i, tempdata$t)
    count = count + 1}
  }

}
}
countdf[nrow(countdf)+1,] = c(m, t , count)
}
finaldf <- rbind(finaldf, inf)
}

stat <-ddply(countdf,~Time,summarise, mean_infection =mean(Count), standev = sd(Count))
plot( stat$Time, stat$mean_infection, type = "n")
lines( stat$Time, stat$mean_infection, type = "l")
  



remove(countdf)
countdf <- data.frame(Mother = integer(), Time = integer(), Count = integer(), stringsAsFactors = FALSE)
for (m in 120:130){
  tic <- Sys.time()
  print(m)
  inf <- m
  a = seq(1, 327,1)
  sus = a[! a %in%  inf]
  countdf[nrow(countdf)+1,] = c(m, 0, 1)
    for ( t in 1:7375){
    inf1 = data$j[(data$i %in% inf) & (data$t == t)]
    inf2 = data$i[(data$j %in% inf) & (data$t == t)]
      inf = unique(append(inf, c(inf1, inf2)))
    sus = sus[! sus %in% inf ]
    
    if(length(inf)==327){
      print("entered")
      countdf[nrow(countdf)+1:(7375-t+1),] =  data.frame(rep(m,length(7375-t)), t:7375, rep(length(inf),length(7375-t)))
      print("done")
      break
    } else {
      countdf[nrow(countdf)+1,] = c(m, t, length(inf))
    }
    }
  toc <- Sys.time()
  print(toc-tic)
  }

thresh = ddply(countdfFinal[countdfFinal$Count>=262,],~Mother,summarise, mean_infection =min(Time))
order(thresh$mean_infection)
  
stat <-ddply(countdf,~Time,summarise, mean_infection =mean(Count), standev = sd(Count))

ggplot(stat, aes(x = Time, y = mean_infection)) +   geom_point(size = 2) +
  geom_errorbar(aes(ymin=mean_infection-standev, ymax=mean_infection+standev), width=.05,
                position=position_dodge(.5))

plot( stat$Time, stat$mean_infection, type = "n")
lines( stat$Time, stat$mean_infection, type = "l")


countdfFinal<- data.frame(Mother = integer(), Time = integer(), Count = integer(), stringsAsFactors = FALSE)

infection <- function(i){
remove(countdf)
countdf <- data.frame(Mother = integer(), Time = integer(), Count = integer(), stringsAsFactors = FALSE)
  tic <- Sys.time()
  print(i)
  inf <- i
  a = seq(1, 327,1)
  sus = a[! a %in%  inf]
  countdf[nrow(countdf)+1,] = c(m, 0, 1)
  for ( t in 1:7375){
    inf1 = data$j[(data$i %in% inf) & (data$t == t)]
    inf2 = data$i[(data$j %in% inf) & (data$t == t)]
    inf = unique(append(inf, c(inf1, inf2)))
    sus = sus[! sus %in% inf ]
    
    if(length(inf)==327){
      print("entered")
      countdf[nrow(countdf)+1:(7375-t+1),] =  data.frame(rep(i,length(7375-t)), t:7375, rep(length(inf),length(7375-t)))
      print("done")
      break
    } else {
      countdf[nrow(countdf)+1,] = c(i, t, length(inf))
    }
  }
  toc <- Sys.time()
  print(toc-tic)
  return(countdf)
}

for (i in 1:327){
  countdf = infection(i)
  countdfFinal = rbind(countdfFinal, countdf)
    
}


r =  order(thresh$mean_infection)
f = seq(0.02,0.5,0.05)

metricD = integer(0)
metricC = integer(0)
metricClose = integer(0)
metricE = integer(0)

for (step in f){
  metricClose[length(metricClose)+1] = (length(intersect(r[1:round(step*N)], close[1:round(step*N)])))/length(r[1:round(step*N)])
  metricE[length(metricE)+1] = (length(intersect(r[1:round(step*N)], E[1:round(step*N)])))/length(r[1:round(step*N)])
  
}

  plot( f, metricE, type = "n")
  lines( f, metricE, type = "l", col = 'blue')
  
    cl=transitivity(g,type= "local")
  names(cl)=names(degree(g))
 
  
##my metric  
c=0  
metricM <-matrix(,nrow = 740, ncol = 327)
for(time in seq(1,7370,10)){
  print(time)
  c=c+1
  for (n in 1:327){
    tempdata = subset(data, t>=time & t < (time+10))
    datanocol = tempdata[,c("i", "j")]
    dataNoDup = datanocol[!duplicated(datanocol),]
    tempg = graph_from_data_frame(dataNoDup,directed = FALSE)
    if(! toString(n) %in% vertex_attr(tempg)$name){
      metricM[c,n] = 0
    }else{
      metricM[c,n] = (lengths(ego(tempg, 2 , toString(n))))/c
    }
  }
}

colSums(metricM[1:2,])
order(colSums(metricM[1:2,]))

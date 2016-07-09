library (Matrix)  
library (arules)

#导入数据
diagnosis <- read.table('diagnosis.data', header=F, dec = ",", sep='\t', fileEncoding="UTF-16LE", as.is=TRUE, col.names=c('temperature','nausea','lumbar','urine','micturition','urethra','bladder','nephritis'))
colum <- list('temperature','nausea','lumbar','urine','micturition','urethra','bladder','nephritis')

#对数据集进行预处理，转换成适合关联规则挖掘的形式
str_all=""
for(i in 1:nrow(diagnosis))
{
  if(as.numeric(diagnosis[i,1])<=36.9){str='normal'}
  else if(as.numeric(diagnosis[i,1])<=37.9){str='low'}
  else if(as.numeric(diagnosis[i,1])<=39.9){str='midde'}
  else if(as.numeric(diagnosis[i,1])<=42.0){str='high'}
 
  for(j in 2:ncol(diagnosis))
  {
    if(diagnosis[i,j]=="yes"){str <- paste(str,colum[[j]],sep=',')}
  }
  if(str_all==''){str_all=str}
  else{str_all <- paste(str_all,str,sep='\r')}
}
write(str_all,'data_preprocess.csv')

#找出频繁项集
a <- read.transactions('data_preprocess.csv',format = 'basket',sep=',')
sink("summary.txt")
summary(a)
sink()
sink("frequent_items.txt")
###frequentsets=eclat(a,parameter=list(support=0.1,maxlen=8))  
###inspect(frequentsets)
itemsets_apr = apriori (a, parameter=list (support=0.1,confidence=0.5,maxlen=8,target="frequent itemsets"), control=list(sort=-1)) 
inspect(itemsets_apr) 
sink()

#导出关联规则，计算其支持度和置信度
sink("rules.txt")
rules = apriori (a, parameter=list (support=0.1,confidence=0.5,minlen=3,maxlen=8,targe="rules"), appearance=list(rhs=c("bladder","nephritis"), default="lhs"), control=list(sort=-1))#若minlen=2则无法画出平行坐标图，并出现警告信息？？？？ 
inspect(rules) 
sink()

#去除冗余的规则
subset.matrix<-is.subset(rules,rules)#生成一个所有规则的子集矩阵,行和列分别是每条rules，其中的值是TRUE和FALSE，当rules2是rules1的子集时，rules2在rules1的值为TRUE
#subset.matrix[row(subset.matrix)==col(subset.matrix)]<-NA
subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA#将矩阵对角线以下的元素置为空，只保留上三角
redundant<-colSums(subset.matrix,na.rm=T)>=1#R会将矩阵中的TRUE当做1，统计每列的和（忽略缺失值），如果该列的和大于等于1，也就是表示该列（规则）是别的规则的子集，应该删除。
rules.pruned<-rules[!redundant]#去掉冗余的规则

sink("rules_delete_redundant.txt")
inspect(rules.pruned) 
sink()

#使用lift对规则进行评价
sink("rules_delete_redundant_sorted_lift.txt")
rules.pruned.sorted_lift = sort(rules.pruned,by='lift')
inspect(rules.pruned.sorted_lift) 
sink()

#使用可视化技术对规则进行展示
library (grid)
library (arulesViz)

#散点图
png(file="scatter_chart.png", bg="transparent")
plot(rules.pruned.sorted_lift)
dev.off()
#平行坐标
png(file="parallel_coordinates.png", bg="transparent")
plot(rules.pruned.sorted_lift,method="paracoord")
dev.off()
#泡泡图
png(file="bubble_chart.png", bg="transparent")
plot(rules.pruned.sorted_lift,method="grouped")
dev.off()

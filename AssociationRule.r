library (Matrix)  
library (arules)

#��������
diagnosis <- read.table('diagnosis.data', header=F, dec = ",", sep='\t', fileEncoding="UTF-16LE", as.is=TRUE, col.names=c('temperature','nausea','lumbar','urine','micturition','urethra','bladder','nephritis'))
colum <- list('temperature','nausea','lumbar','urine','micturition','urethra','bladder','nephritis')

#�����ݼ�����Ԥ����ת�����ʺϹ��������ھ����ʽ
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

#�ҳ�Ƶ���
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

#�����������򣬼�����֧�ֶȺ����Ŷ�
sink("rules.txt")
rules = apriori (a, parameter=list (support=0.1,confidence=0.5,minlen=3,maxlen=8,targe="rules"), appearance=list(rhs=c("bladder","nephritis"), default="lhs"), control=list(sort=-1))#��minlen=2���޷�����ƽ������ͼ�������־�����Ϣ�������� 
inspect(rules) 
sink()

#ȥ������Ĺ���
subset.matrix<-is.subset(rules,rules)#����һ�����й�����Ӽ�����,�к��зֱ���ÿ��rules�����е�ֵ��TRUE��FALSE����rules2��rules1���Ӽ�ʱ��rules2��rules1��ֵΪTRUE
#subset.matrix[row(subset.matrix)==col(subset.matrix)]<-NA
subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA#������Խ������µ�Ԫ����Ϊ�գ�ֻ����������
redundant<-colSums(subset.matrix,na.rm=T)>=1#R�Ὣ�����е�TRUE����1��ͳ��ÿ�еĺͣ�����ȱʧֵ����������еĺʹ��ڵ���1��Ҳ���Ǳ�ʾ���У������Ǳ�Ĺ�����Ӽ���Ӧ��ɾ����
rules.pruned<-rules[!redundant]#ȥ������Ĺ���

sink("rules_delete_redundant.txt")
inspect(rules.pruned) 
sink()

#ʹ��lift�Թ����������
sink("rules_delete_redundant_sorted_lift.txt")
rules.pruned.sorted_lift = sort(rules.pruned,by='lift')
inspect(rules.pruned.sorted_lift) 
sink()

#ʹ�ÿ��ӻ������Թ������չʾ
library (grid)
library (arulesViz)

#ɢ��ͼ
png(file="scatter_chart.png", bg="transparent")
plot(rules.pruned.sorted_lift)
dev.off()
#ƽ������
png(file="parallel_coordinates.png", bg="transparent")
plot(rules.pruned.sorted_lift,method="paracoord")
dev.off()
#����ͼ
png(file="bubble_chart.png", bg="transparent")
plot(rules.pruned.sorted_lift,method="grouped")
dev.off()

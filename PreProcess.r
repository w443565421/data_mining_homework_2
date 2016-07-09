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

setwd("D:\\KULIAH\\PSK\\R")
X=scan()
forecast=function(num,X,n)
{
  k=length(X)
  switch(num,
         satu={
           sma=array(0,dim=c(k))
           for (i in 1:n){
             sma[i+(n-1)]=mean(X[i:(i+(n-1))])
           }
           sma
           hasil=array(0,dim=c(k,2))
           for(i in 1:k){
             hasil[i,1]=round(X[i],digits=3)
             hasil[i,2]=round(sma[i],digits=3)
           }
           data_aktual=c(hasil[,1])
           sma_forecast=c(hasil[,2])
           tabel=data.frame(data_aktual,sma_forecast)
           tabel
         },
         dua={
           wma=array(0,dim=c(k))
           k1=c(1:k)
           for (i in 1:n){
             wma[i+(n-1)]=(sum(X[i:(i+(n-1))]*k1[i:(i+(n-1))]))/sum(k1[i:(i+(n-1))])
           }
           wma
           hasil=array(0,dim=c(k,2))
           for(i in 1:k){
             hasil[i,1]=round(X[i],digits=3)
             hasil[i,2]=round(wma[i],digits=3)
           }
           data_aktual=c(hasil[,1])
           wma_forecast=c(hasil[,2])
           tabel=data.frame(data_aktual,wma_forecast)
           tabel
         },
         tiga={
           sma=array(0,dim=c(k))
           k1=c(1:k)
           for (i in 1:n){
             sma[i+(n-1)]=mean(X[i:(i+(n-1))])
           }
           sma
           for(i in 1:n){
             smaawal=sma[i]
           }
           ema=array(0,dim=c(k))
           for(i in 1:n){
             ema[i+(n-1)]=((2/(k1[i+(n-1)]+1))*(X[i+(n-1)]-smaawal))+smaawal
           }
           ema
           for(i in 1:n){
             ema[i+n]=((2/(k1[i+n]+1))*(X[i+n]-ema[i+(n-1)]))+ema[i+(n-1)]
           }
           ema
           hasil=array(dim=c(k,2))
           for(i in 1:k){
             hasil[i,1]=round(X[i],digits=3)
             hasil[i,2]=round(ema[i],digits=3)
           }
           data_aktual=c(hasil[,1])
           ema_forecast=c(hasil[,2])
           tabel=data.frame(data_aktual,ema_forecast)
           tabel
         }
  )
}

forecast("satu",X,10)
forecast("dua",X,10)
forecast("tiga",X,10)

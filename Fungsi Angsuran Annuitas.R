setwd("D://KULIAH//PSK//R")
Angsuran=function(num,nilai,i,t,m=TRUE)
{
  j=i/m
  n=t*m
  v=1/(1+j)
  switch(num, 
         satu = {
           k.an.akhir=nilai/((1-v^n)/j)
           cat("Angsuran diketahui an annuitas akhir:",k.an.akhir)
         },
         dua = {
           k.sn.akhir=nilai/(((1+j)^n-1)/j)
           cat("Angsuran diketahui sn annuitas akhir:",k.sn.akhir)
         },
         tiga = {
           k.an.awal=nilai/((1-v^n)/(j*v))
           cat("Angsuran diketahui an annuitas awal:",k.an.awal)
         },
         empat = {
           k.sn.awal=nilai/(((1+j)^n-1)/(j*v))
           cat("Angsuran diketahui sn annuitas awal:",k.sn.awal)
         }
  )
}

Angsuran("satu",8000,0.08,5,6)
Angsuran("dua",8000,0.08,5,6)
Angsuran("tiga",8000,0.08,5,6)
Angsuran("empat",8000,0.08,5,6)

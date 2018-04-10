setwd("D:\\KULIAH\\PSK\\R")
annuitas=function(num,k,i,t,m=TRUE)
{
  j=i/m
  n=t*m
  v=1/(1+j)
  switch(num, 
         satu = {
           an.akhir=k*((1-v^n)/j)
           sn.akhir=k*(((1+j)^n-1)/j)
           cat("an annuitas akhir:",an.akhir)
           cat(" dan sn annuitas akhir:",sn.akhir)
         },
         dua = {
           an.awal=k*((1-v^n)/(j*v))
           sn.awal=k*(((1+j)^n-1)/(j*v))
           cat("an annuitas awal:",an.awal)
           cat(" dan sn annuitas awal:",sn.awal)
         }
  )
}
annuitas("satu",2000,0.08,4,2)
annuitas("dua",2000,0.08,4,2)
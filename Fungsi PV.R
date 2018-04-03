#Fungsi Present Value
# A: nilai akumulasi, i=bunga , t=tahun, m=banyak pemberian bunga
pv=function(num,A,i,t,m=TRUE)
{
  switch(num, 
         satu = {
           bunga.tunggal = A/(1+i*t)
           cat("PV bunga tunggal:",bunga.tunggal)
         },
         dua = {
           majemuk.nominal = A/(1+(i/m))^(m*t)
           cat("PV bunga majemuk nominal:",majemuk.nominal)
         },
         tiga = {
           majemuk.kontinu = A/exp(i*t)
           cat("PV bunga majemuk kontinu:",majemuk.kontinu)
         }
  )
}
pv("satu",3500,0.07,4)
pv("dua",3500,0.07,4)
pv("dua",3500,0.07,4,2)
pv("tiga",3500,0.07,4)
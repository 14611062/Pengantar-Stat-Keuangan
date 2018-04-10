#fungsi mencari nilai akumulasi
#k: modal, i:bunga ,t:tahun , m:banyak pmberian bunga
nilaiakumulasi=function(num,k,t,i,m=TRUE)
{
  switch(num, 
         satu = {
           bunga.tunggal = k*(1+i*t)
           cat("Nilai akumulasi bunga tunggal:",bunga.tunggal)
         },
         dua = {
           majemuk.nominal = k*(1+(i/m))^(m*t)
           cat("Nilai akumulasi bunga majemuk nominal:",majemuk.nominal)
         },
         tiga = {
           majemuk.kontinu = k*(exp(i*t))
           cat("Nilai akumulasi bunga majemuk kontinu:",majemuk.kontinu)
         }
  )
}
nilaiakumulasi("satu",2000,4,0.08,2)
nilaiakumulasi("dua",2000,4,0.08,2)
nilaiakumulasi("tiga",2000,4,0.08,2)

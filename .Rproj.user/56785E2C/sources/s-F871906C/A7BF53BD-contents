# Creates officers' body camera assignments
w.rep.1Ds<-matrix(ncol=10000,nrow=14) # matrix with 10000 columns

for(j in 1:10000)
{
  count<-1
   for(i in 1:7) # for each pair of officers
   {
    a<-sample(c(0,1),2,replace=FALSE)  # unique values
    w.rep.1Ds[count,j]<-a[1]  # puts 1 or 0 into matrix
    w.rep.1Ds[count+1,j]<-a[2] # puts 1 or 0 into matrix
    count<-count+2 # increments row
  }

}

# unique pairs only
dim(unique(t(w.rep.1Ds)))
w.rep.1Ds.t<-unique(t(w.rep.1Ds))
w.rep.1Ds<-t(w.rep.1Ds.t)
rand_matrix_1DS<-w.rep.1Ds
usethis::use_data(rand_matrix_1DS, compress = "xz")

# Creates officers' body camera assignments
w.rep.1D<-matrix(ncol=10000,nrow=284) # matrix with 10000 columns

for(j in 1:10000)
{
  count<-1
  for(i in 1:142) # for each pair of officers
  {
    a<-sample(c(0,1),2,replace=FALSE)  # unique values
    w.rep.1D[count,j]<-a[1]  # puts 1 or 0 into matrix
    w.rep.1D[count+1,j]<-a[2] # puts 1 or 0 into matrix
    count<-count+2 # increments row
  }

}

# unique pairs only
dim(unique(t(w.rep.1D)))
w.rep.1D.t<-unique(t(w.rep.1D))
w.rep.1D<-t(w.rep.1D.t)
rand_matrix_1D<-w.rep.1D
usethis::use_data(rand_matrix_1D, compress = "xz")

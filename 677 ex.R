#(1)
n<-100
type2_count<-NULL
type2_count2<-NULL
p<-seq(0.4,1,0.001)

#calculate type2 error rate when m=69
for (j in 1:601) {
  
  type2_count[j]<-sum(rbinom(1000,n,p[j])<69)
}


#calculate type2 error rate when m=73
for (j in 1:601) {
  
  type2_count2[j]<-sum(rbinom(1000,n,p[j])<73)
}

type2_error<-type2_count/1000
type2_error2<-type2_count2/1000

#plot
plot(p,1-type2_error,type='l')
points(p,1-type2_error2,col='red',type = 'l')
rect(xleft = 0.6, ybottom = 0.05, xright = 0.8, ytop = 0.95)

#(2)
 library('pwr')

pwr.p.test(h = ES.h( p1=0.8,p2 = 0.6),
           sig.level = 0.05,
           power = 0.95,
           alternative = "greater")
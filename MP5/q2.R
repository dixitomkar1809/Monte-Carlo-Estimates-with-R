library(boot)
set.seed(1234)
# Initializing
l=c(0.01,0.1,1,10)
n=c(5,10,30,100)
a=0.05
# Simulating 5000 runs
r=5000

# looping for n
for(i in n)
{
  # looping for lambda
  for(j in l)
  {
    largeCoverage=0
    bootCoverage=0
    # repeating for 5000 monte carlo estimation
    for(k in 1:r)
    {
      x=rexp(i,j)
      # Calculating Mean, Variance, Standard Error
      x.mean=1/j
      x.var=var(x)
      std.error=sqrt(x.var/i)
      # Calulating CI from  large sample
      large.ci=mean(x) + c(-1,1)*qnorm(1-a/2)*std.error
      if(large.ci[1]<x.mean && large.ci[2]>x.mean)
      {
        largeCoverage=largeCoverage+1
      }
      # Calculating CI from bootstrap percentile method
      boot.ci = boot(x, function(x, indices)
      {
        result = mean(x[indices])
        return(result)
      }, R=999, sim = "ordinary", stype = "i")
      boot.percentile = sort(boot.ci$t)[c(25,975)]
      if(boot.percentile[1]<x.mean && boot.percentile[2]>x.mean)
      {
        bootCoverage=bootCoverage+1
      }
    }
    # Calculating coverage probabilities of percentile bootstrap and sample z-interval for all value combinations possible
    boot_prob = bootCoverage/r
    z_prob=largeCoverage/r
    print(c(i, j, boot_prob, z_prob))
  }
}

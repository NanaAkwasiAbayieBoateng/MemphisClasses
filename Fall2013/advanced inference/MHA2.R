gamm<-function (n, a, b) 
{
        mu <- a/b
        sig <- sqrt(a/(b * b))
        vec <- vector("numeric", n)
        x <- a/b
        vec[1] <- x
        for (i in 2:n) {
                can <- rnorm(1, mu, sig)
                aprob <- min(1, (dgamma(can, a, b)/dgamma(x, 
                        a, b))/(dnorm(can, mu, sig)/dnorm(x, 
                        mu, sig)))
                u <- runif(1)
                if (u < aprob) 
                        x <- can
                vec[i] <- x
        }
        vec
}


vec<-gamm(10000,2.3,2.7)
par(mfrow=c(2,1))
plot(ts(vec))
hist(vec,30)
par(mfrow=c(1,1))

void gamm(long *np, double *ap, double *bp, double *vec)
{
  long n,i;
  double a,b,x,can,u,aprob,mu,sig;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_mt19937);
  n=*np;a=*ap;b=*bp;
  mu=a/b;
  sig=sqrt(a/(b*b));
  x=mu;
  vec[0]=x;
  for (i=1;i<n;i++)
    {
      can=mu+gsl_ran_gaussian(r,sig);
      aprob=min(1.0, (gsl_ran_gamma_pdf(can,a,1/b)/gsl_ran_gamma_pdf(x,a,1/b))/
		(gsl_ran_gaussian_pdf(can-mu,sig)/
		 gsl_ran_gaussian_pdf(x-mu,sig)));
      u=gsl_ran_flat(r,0.0,1.0);
      if (u < aprob)
	x=can;
      vec[i]=x;
    }
}
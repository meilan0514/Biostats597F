sim1 <- function(nreps)  {
  nb1<-10 #10bluemarblesinUrn1
  n1 <- 18 # number of marbles in Urn 1 at 1st pick
  n2 <- 13 # number of marbles in Urn 2 at 2nd pick
  count <- 0 # number of repetitions in which get blue from Urn 2 
  for (i in 1:nreps) {
    nb2<-6 #6bluemarblesorig.inUrn2
    # pick from Urn 1 and put in Urn 2; is it blue?
    if (runif(1) < nb1/n1) nb2 <- nb2 + 1
    # pick from Urn 2; is it blue?
    if (runif(1) < nb2/n2) count <- count + 1
  }
  return(count/nreps) # est. P(pick blue from Urn 2)
}

sim3 <- function(nreps) {
  nb1 <- 10
  nb2 <- 6
  n1 <- 18
  n2 <- 13
  u <- matrix(c(runif(2*nreps)),nrow=nreps,ncol=2)
  # set up the condition vector
  cndtn <- u[,1] <= nb1/n1 & u[,2] <= (nb2+1)/n2 |
    u[,1] > nb1/n1 & u[,2] <= nb2/n2
  return(mean(cndtn))
}


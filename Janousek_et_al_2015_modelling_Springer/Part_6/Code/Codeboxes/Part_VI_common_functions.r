####################################################
#
# Part VI : Worked examples
#
####################################################

################################
# (1) Functions definitions
################################

########## Reverse model - major elements ############

rev.maj <- function(c0.lab,cL.lab,min.set,norm=F){
    mincomp <- min.tab[min.set,mjrs]
    c0 <- WR[c0.lab,mjrs]
    cL <- WR[cL.lab,mjrs]
    if(norm){
        c0 <- c0/sum(c0)*100
        cL <- cL/sum(cL)*100
        mincomp <- t(apply(mincomp,1,FUN=function(z) {z <- z/sum(z)*100; return(z)}))
    }
  
    A <- t(rbind(mincomp,cL))
    colnames(A) <- c(rownames(mincomp),"F")
    ee <- lsfit(A,c0,intercept=F)

    ff <- ee$coef["F"]
    m <- ee$coef
    m <- m[1:length(m)-1]/(1-ff)
    m <- m/sum(m)
    cs <- m%*%as.matrix(mincomp)
    cLc <- (c0-(1-ff)*cs)/ff
    residue <- cLc-cL
    r.sq <- sum(residue^2)
  
    res <- list(ff=ff,cS=cs,r.sq=r.sq,m=m)
    return(res)
}

########## Reverse model (FC) - trace elements ############

rev.tr.fc <- function(c0.lab,cL.lab,min.set){
    kd <- kd.tab[min.set,trc]
    c0 <- WR[c0.lab,trc]
    cL <- WR[cL.lab,trc]
  
    dmat <- as.matrix(kd)-1
    cv <- log(cL/c0)
    ee <- lsfit(t(dmat),cv,intercept=F)
  
    mm <- ee$coeff
    ffc <- exp(sum(mm))
    m <- mm/log(ffc)*100
    dd <- m%*%as.matrix(kd)/100
    cLc <- c0*ffc^(dd-1)
    residue <- (cLc-cL)/cL
    r.sq <- sum(residue^2)
  
    res <- list(ff=ffc,dd=dd,r.sq=r.sq,m=m)
    return(res)
}

########## Forward model - both ############

fwd.mod <- function(c0.lab,m,ff,norm=F,eqn="FC"){ 
    min.set <- names(m)
    kd <- kd.tab[min.set,trc]
    c0t <- WR[c0.lab,trc]
    dd <- m%*%as.matrix(kd)
    cLc.t <- switch(eqn,FC=c0t*ff^(dd-1),PM=c0t/(dd+ff*(1-dd)))
  
    mincomp <- min.tab[min.set,mjrs]
    c0m <- WR[c0.lab,mjrs]
    if(norm){
        c0 <- c0m/sum(c0m)*100 # Fixed c0m instead of c0?
        mincomp <- t(apply(mincomp,1,FUN=function(z) {
            z <- z/sum(z)*100; return(z)
        }))
    }
    cs <- m%*%as.matrix(mincomp)
    cLc.m <- (c0m-(1-ff)*cs)/ff
  
    cLc <- c(cLc.m,cLc.t)
    names(cLc) <- c(mjrs,trc)
    res <- list(cS=cs,cL=cLc,dd=dd)
    return(res)
}

########## Plagioclase reconstruction ############
plagio <- function(an,ab){
    mw.an <- mw["Ca"]+2*mw["Al"]+2*mw["Si"]+8*mw["O"]
    mw.ab <- mw["Na"]+mw["Al"]+3*mw["Si"]+8*mw["O"]
    total.plag <- ab+an
    an_pct <- an/mw.an/(an/mw.an+ab/mw.ab)*100
    res <- c(total.plag,an_pct)
    names(res) <- c("total.plag","an_pct")
    return(res)
}

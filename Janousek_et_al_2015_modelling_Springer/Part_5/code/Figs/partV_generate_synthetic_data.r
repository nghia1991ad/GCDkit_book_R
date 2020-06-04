## Generate a synthetic dataset
# Losely based on the example of the Cayambe volcano 
# Samaniego P, Martin H, Robin C, Monzier M (2002) Transition from calc-alkalic to adakitic magmatism at Cayambe volcano, Ecuador: Insights into slab melts and mantle wedge interactions. Geology 30 (11):967-970
##########################################################

# Working dir, need to be adjusted to your system !!
setwd("C:/Users/moyen/Documents/Recherche/Springer Book")

#Definitions
############

mjrs<-c("SiO2","Al2O3","FeOt","MgO","MnO","CaO","Na2O","K2O","TiO2")
trc<-c("Rb","Sr","Ba","Zr","V","Ni","Y","La","Nb","Ta","X")
mins<-c("Pl","Hb","Mt","Zrc")

# Molecular weights, needed for M
# MW does exist in GCDkit, but it is (re)defined here to allow the code to run without GCDkit
MWt<-c(60.0848,101.9613,71.8464,40.3044,70.9374,56.0774,61.9789,94.1954,79.8658)
names(MWt)<-mjrs
mw.coefs<-c(1,2,1,1,1,1,2,2,1)
names(mw.coefs)<-mjrs

# C0 -- the most mafic lava of the suite (CAY56 from Samaniego et al.)
c0.maj<-c(56.5,17.08,6.90,4.24,0.11,7.08,3.54,1.31,0.88)
names(c0.maj)<-mjrs
c0.trc<-c(30,570,620,140,200,50,18,18,5.5,0.4,1)
names(c0.trc)<-trc
c0<-c(c0.maj,c0.trc)

# Mineral compositions
# We use only 4 minerals here (plag, amphibole, magnetite, zircon)
# As the "real" modelling shows that this the likely fractionnating assemblage is amp+pg+mt
min.comp<-matrix(c(53.11,28.98,0.57,0.08,0,11.71,4.8,0.32,0.09,
               42.24,12.15,12.5,14.04,0.2,11.39,2.63,0.61,2.82,
               0.6,1.29,94,0.39,0.14,0.01,0,0.02,3,
               50,0,0,0,0,0,0,0,0),
            nrow=4,byrow=T) 

colnames(min.comp)<-mjrs
rownames(min.comp)<-mins

# Partition coefs
Kd<-matrix(c(
0.04,4.4,0.5,0.01,0.01,0.38,0.055,0.4,0.025,0.05,0,
0.014,0.022,0.044,0.02,10,12,5,0.74,0.9,0.5,0,
0.00001,0.00001,0.00001,0.00001,8.6,8.6,0.00001,0.22,0.00001,0.04,0,
0.00001,0.00001,0.00001,3800,0.00001,0.00001,200,2,25,50,1000),
nrow=4,byrow=T)

colnames(Kd)<-trc
rownames(Kd)<-mins

# Mineral proportions (from actual model)
min.props<-c(0.45,0.5,0.05,0)
names(min.props)<-mins

# Range of F values (from actual model, tweaked to make sure no major gets < 0)
fmin<-0.46
fmax<-1
fstep<--0.01
ff<-seq(fmax,fmin,fstep)

# Temperature defined as a function of F
# Function adjusted so that we have 
# ~1020 °C at F = 1    (SiO2 = 56%)
#  ~720 °C at F = 0.45 (SiO2 = 72%)
# Probably realistic based on experimental literature

t.c<-function(f.n){
return(460+560*f.n)
}

# Water content
# Also tweaked as a function of F
# It's main use is to have totals < 100 % (LOI)

tot.anh<-function(f.n){
return(99.6-1.5*(1-f.n))
}

# Calculate the cumulate and D
cs<-min.props%*%min.comp
D<-min.props%*%Kd

# Theoretical evolution (fwd model)
###################################

# For each value of F, we calculate major, traces, and Zr saturation

qq<-sapply(ff,function(f.n){
    # Major elements
    maj<-(c0[mjrs]-(1-f.n)*cs[1,mjrs])/f.n
    maj<-maj/sum(maj)*tot.anh(f.n) # Majors renormalized to match the expected anydrous total
    
    # M parameter (Watson & Harrison)
    ee<-maj[mjrs]/MWt[mjrs]*mw.coefs[mjrs]
    ee<-ee/sum(ee)
    M<-(ee["Na2O"]+ee["K2O"]+2*ee["CaO"])/(ee["SiO2"]*ee["Al2O3"])
    
    # Traces (first pass, without Zr sat)
    tr<-c0[trc]*f.n^(D[1,trc]-1)
    
    # Zr sat
    DZr<-exp(-3.8-0.85*(M-1)+12900/(t.c(f.n)+273))
    Zr.sat<-497644/DZr
    
    # Correction for Zrc
    # Technically wrong, zrc amount treated as batch, probably close enough...
    if(tr["Zr"]<Zr.sat){
        tr.c<-tr
        zrc.prop<-0
    }else{
        zr.excess<-tr["Zr"]-Zr.sat
        zrc.prop<-zr.excess/497644
        m.pr<-min.props
        m.pr["Zrc"]<-zrc.prop
        m.pr<-m.pr/sum(m.pr)
        D.c<-m.pr%*%Kd
        tr.c<-c0[trc]*f.n^(D.c[1,trc]-1)
        tr.c["Zr"]<-Zr.sat
    }
    
    return(c(maj,M,tr,Zr.sat,zrc.prop,tr.c))
}
) # end of sapply

fwd.mod<-t(qq)
fwd.mod<-cbind(ff,t.c(ff),fwd.mod)
colnames(fwd.mod)<-c("ff","TC",mjrs,"M",paste(trc,"_or",sep=""),"Zr.sat","Zrc.prop",paste(trc,"_corr",sep=""))

# Generate a random dataset based on theoretical evolution
##########################################################

# Number of points to create
n.pts<-25

# Scatter is defined in terms of sigma, so sigma = 0.02 means 2s = 0.04
# ie 95% of the points will be within 4% of the real value.
# traces are more noisy than majors
scatter<-c(rep(0.02,length(mjrs)),rep(0.06,length(trc)))

# For each "data" point, we pick a spot on the real curve and jitter it away
f.samples<-runif(n.pts,min=fmin+0.00001,max=fmax-0.00001)

foo<-sapply(f.samples,function(f.n){
    # The random f value will never be an actual node of the curve, so we interpolate between neighbours
    qq<-findInterval(f.n,sort(ff))
    top<-ff[length(ff)-qq]
    bot<-ff[length(ff)-qq+1]
    mod.t<-fwd.mod[fwd.mod[,"ff"]==top,]
    mod.b<-fwd.mod[fwd.mod[,"ff"]==bot,]
    val<-mod.b+(mod.t-mod.b)*(f.n-bot)/(top-bot)
    
    # We want only data for major and traces
    extract<-c(mjrs,paste(trc,"corr",sep="_"))
    val<-val[extract]
    names(val)<-c(mjrs,trc)
    
    # Add gaussian noise
    jittr<-rnorm(length(val),mean=1,sd=scatter)
    val<-val*jittr
    
    return(val)
}
) # end of sapply

wr.data<-t(foo)

# Transfert to GCDkit
rownames(wr.data)<-paste("a",seq(1,nrow(wr.data)),sep="")
.loadData.process(wr.data, merging=FALSE, clipboard=F, GUI=F)

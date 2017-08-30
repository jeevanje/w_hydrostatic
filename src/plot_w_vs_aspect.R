projectdir = "~/Dropbox/w_hydrostatic"
load(paste(projectdir,"/data/D_H.Rdata",sep=""))
source(paste(projectdir,"/src/beta_formulae.R",sep=""))

dxlist   = c(0.0625, 0.125,0.25,0.5,1,2,4,8,16)
Ndx      = length(dxlist)
solvers  = c("nh","hyd")
varlist  = c("D","H","aspect","w")

#=======#
# Data  #
#=======#

for (solver in solvers){
    solver_data  = eval(as.name(paste("fv3_",solver,"_prod_data",sep="")))  
    aspectvec    = numeric(Ndx)
    wvec         = numeric(Ndx)
    Dvec         = numeric(Ndx)
    Hvec         = numeric(Ndx)
    for (i in 1:Ndx){
    	dx     = dxlist[i]
	case   = paste("dx",dx,sep="")
	dxdata = solver_data[[case]]
	Ddata  = dxdata$D
	Hdata  = dxdata$H
	wdata   = dxdata$wmax
	aspectdata = Ddata/Hdata
	Dvec[i]	   = mean(Ddata)
	Hvec[i]	   = mean(Hdata)
	aspectvec[i] = mean(aspectdata)
	wvec[i] = mean(wdata)
	}
    assign(paste("aspect",solver,"_vec",sep=""),aspectvec)
    assign(paste("w",solver,"_vec",sep=""),wvec)
    assign(paste("D",solver,"_vec",sep=""),Dvec)    
    assign(paste("H",solver,"_vec",sep=""),Hvec)
    }

# Calculate wnh_ref
lsfit  = lm(wnh_vec ~ 0 + sqrt(beta_nh(aspectnh_vec,1)))
wnhref = round(lsfit$coefficients[1],digits=2)

#=======#
# Plots #
#=======#

cex=1.5
cex_pch  = 3/4
colvec   = c("black","red")
dxlim    = range(dxlist)
DHlim	 = c(0.1,500)
wlim  	 = c(0.01,3.5) 
pch      = 8 
lwd      = 3
H        = 1 
nref	 = 1 #index of run to use for wcref
#wnhref   = wnh_vec[nref]/sqrt(beta_nh(aspectnh_vec[nref],H))  #m/s
whydref  = wnhref  #m/s, tuneable
Dvals    = exp(seq(log(DHlim[1]*H),log(DHlim[2]*H),length.out=100))  # H
plot_points = function(x,y,col){
                points(x,y,type="p",pch=pch,lwd=1,col=col,cex=cex_pch)
		}

plotfile=paste(projectdir,"/plots/w_vs_aspect.pdf",sep="")
pdf(plotfile,width = 7, height =5 )
par(mfrow=c(1,1),mar=c(5,5,5,3))
plot(1,type="n",xlim=DHlim, ylim = wlim,
	#main="Proxy vs. Simulations",
	log = "x",
	xlab = "D/H",
	ylab = expression(w[c]/w[c0]),
	cex.main = cex,
	cex.lab = cex,
	cex.axis = cex,
	lab  = c(5,3,7),
    xaxt = "n",
    yaxs = "i",
    xaxs = "i"
	)
axis(1, at=c(0.1,1,10,100), labels=c(0.1,1,10,100),cex.axis = cex)
	
# proxies, black over red
points(Dvals/H,sqrt(beta_hyd(Dvals,H)),type="l",lwd=lwd,col=colvec[2])
points(Dvals/H,sqrt(beta_nh(Dvals,H)),type="l",lwd=lwd,col=colvec[1])

#FV3, points over proxies
plot_points(aspecthyd_vec,whyd_vec/whydref,col=colvec[2])
plot_points(aspectnh_vec, wnh_vec/wnhref,col=colvec[1])

legend("topright",
	legend=c(expression(sqrt(beta[nh]/B[0])),
		expression(sqrt(beta[hyd]/B[0])),
		"FV3 nh","FV3 hyd"),
	pch = c(NA,NA,pch,pch),
	col = c("black","red","black","red"),
	#pt.cex = c(1,1, cex_pch,cex_pch),
	lwd = c(2,2,NA,NA)
	)		
abline(v=1,lty="dashed",lwd=1)		
dev.off()


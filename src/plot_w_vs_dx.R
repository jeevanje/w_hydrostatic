projectdir = "~/Dropbox/w_hydrostatic"
load(paste(projectdir,"/data/D_H.Rdata",sep=""))
source(paste(projectdir,"/src/beta_formulae.R",sep=""))

dxlist   = c(0.0625,0.125,0.25,0.5,1,2,4,8,16)
Ndx      = length(dxlist)
solvers  = c("nh","hyd")

#=======#
# Data  #
#=======#

for (solver in solvers){
    solver_data  = eval(as.name(paste("fv3_",solver,"_prod_data",sep="")))  
    Dsolver_data = list()
    Dsolver_data[['mean']] <- numeric(Ndx) 
    Dsolver_data[['sd']]   <- numeric(Ndx) 
    Hsolver_data = list()
    Hsolver_data[['mean']] <- numeric(Ndx) 
    Hsolver_data[['sd']]   <- numeric(Ndx) 
    wsolver_data = list()
    wsolver_data[['mean']] <- numeric(Ndx) 
    wsolver_data[['sd']]   <- numeric(Ndx) 
    Bsolver_data = list()
    Bsolver_data[['mean']] <- numeric(Ndx) 
    Bsolver_data[['sd']]   <- numeric(Ndx) 

    for (i in 1:Ndx){
    	dx     = dxlist[i]
	case   = paste("dx",dx,sep="")
	dxdata = solver_data[[case]]

	Ddata  = dxdata$D
	Dsolver_data[['mean']][i] = mean(Ddata)
	Dsolver_data[['sd']][i]   = sd(Ddata)

	Hdata  = dxdata$H
	Hsolver_data[['mean']][i] = mean(Hdata)
	Hsolver_data[['sd']][i]   = sd(Hdata)

	wdata  = dxdata$wmax
	wsolver_data[['mean']][i] = mean(wdata)
	wsolver_data[['sd']][i]   = sd(wdata)

	Bdata  = dxdata$Bmax
	Bsolver_data[['mean']][i] = mean(Bdata)
	Bsolver_data[['sd']][i]   = sd(Bdata)
	}
    assign(paste("D",solver,"_data",sep=""),Dsolver_data)
    assign(paste("H",solver,"_data",sep=""),Hsolver_data)
    assign(paste("w",solver,"_data",sep=""),wsolver_data)
    assign(paste("B",solver,"_data",sep=""),Bsolver_data)
    }


#=======#
# Plots #
#=======#

cex=1.5
colvec = c("black","red")
dxlim = range(dxlist)
dxvals = seq(from=dxlim[1],to=dxlim[2],length.out=100)    # km
n 	  = 2
Dvals = n*dxvals  # km
H	  = 1         # km
wlim  = c(0,25) 
lwd   = 2
wscale = wnh_data[['mean']][1]
legendvec = c("Non-hydrostatic","Hydrostatic")
plot_points = function(x,y,col){
                points(x,y,type="b",pch=16,lwd=lwd,col=col,cex=1.25)
		}


plotfile=paste(projectdir,"/plots/w_vs_dx.pdf",sep="")
pdf(plotfile,width = 5, height =5 )
par(mfrow=c(1,1),mar=c(5,5,5,3))
plot(1,type="n",xlim=dxlim, ylim = wlim,
	main="",
	log = "x",
	xlab = "dx (km)",
	ylab = expression(w[c]~~"(m/s)"),
	cex.main = cex,
	cex.lab = cex,
	cex.axis = cex
	)
plot_points(dxlist,wnh_data[['mean']],col=colvec[1])
plot_points(dxlist,whyd_data[['mean']],col=colvec[2])
#points(dxvals,wscale*sqrt(beta_nh(Dvals,1)),col=colvec[1],type="l",lty="dashed")
#points(dxvals,wscale*sqrt(beta_hyd(Dvals,1)),col=colvec[2],type="l",lty="dashed")
legend("topright", legend = legendvec ,lwd=lwd, col = colvec,cex =1)
dev.off()


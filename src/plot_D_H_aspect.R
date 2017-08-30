projectdir = "~/Dropbox/w_hydrostatic"
load(paste(projectdir,"/data/D_H.Rdata",sep=""))
source(paste(projectdir,"/src/beta_formulae.R",sep=""))

dxlist   = c(0.0625, 0.125,0.25,0.5,1,2,4,8,16)
Ndx      = length(dxlist)
solvers  = c("nh","hyd")
Dplotdata = list()
Hplotdata = list()
Dsdvec   = numeric(Ndx) 
Hsdvec   = numeric(Ndx) 

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
    Bsolver_data = list()
    Bsolver_data[['mean']] <- numeric(Ndx) 
    Bsolver_data[['sd']]   <- numeric(Ndx) 
    aspect_solver_data = list()
    aspect_solver_data[['mean']] <- numeric(Ndx) 
    aspect_solver_data[['sd']]   <- numeric(Ndx) 
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

	Bdata  = dxdata$B
	Bsolver_data[['mean']][i] = mean(Bdata)
	Bsolver_data[['sd']][i]   = sd(Bdata)

	aspect_solver_data[['mean']][i] = mean(Ddata/Hdata)
	aspect_solver_data[['sd']][i]   = sd(Ddata/Hdata)
	}
    assign(paste("D",solver,"_data",sep=""),Dsolver_data)
    assign(paste("H",solver,"_data",sep=""),Hsolver_data)
    assign(paste("B",solver,"_data",sep=""),Bsolver_data)
    assign(paste("aspect_",solver,"_data",sep=""),aspect_solver_data)
    }

#============#
# Plot setup #
#============#

cex = 1.8
cex_leg = 1.6
cex_pts = 1.75
lwd = 2.25
colvec = c("black","red")
dxlim = range(dxlist)
Dlim  = 1e-3*range(c(Dnh_data[['mean']]+Dnh_data[['sd']],
		   Dhyd_data[['mean']]+Dhyd_data[['sd']]))  # km
Hlim  = c(0,2000)
plot_frame = function(ylim,ylab,main,log="x"){
		plot(1,type="n", 
		     xlim = dxlim,
		     xlab = "dx (km)",
		     ylim = ylim,
		     ylab = ylab,
		     main = main,
		     log  = log,
		     cex.main = cex,
		     cex.axis = cex,
		     cex.lab  =  cex
		     ) 
		 }		     

plot_points = function(y,col){
		points(dxlist,y,type="b",pch=16,lwd=lwd,col=col,cex=cex_pts)
		}

#=======#
# Plots #
#=======#

plotfile=paste(projectdir,"/plots/D_H_aspect.pdf",sep="")
pdf(plotfile,width = 12, height = 4 )
par(mfrow=c(1,3),mar=c(5,5,5,4))

# D
plot_frame(ylim=c(0,3), ylab = 'n = D/dx ', 
	  main="(a)  Parcel width (no. of grid points)")
for (i in 1:2){
    solver = solvers[i]
    col  = colvec[i]
    Davg = eval(as.name(paste("D",solver,"_data",sep="")))[['mean']]
    Dsd  = eval(as.name(paste("D",solver,"_data",sep="")))[['sd']]
    plot_points(1e-3*Davg/dxlist,col)
    #arrows(dxlist, 1e-3*(Davg-Dsd)/dxlist, dxlist, 1e-3*(Davg+Dsd)/dxlist, 
    # 	   length=0.05, angle=90, code=3,col=col)
    }

# H
plot_frame(ylim=Hlim, ylab = 'H (m) ', main="(b)  Parcel height")
for (i in 1:2){
    solver = solvers[i]
    col  = colvec[i]
    Havg = eval(as.name(paste("H",solver,"_data",sep="")))[['mean']]
    Hsd  = eval(as.name(paste("H",solver,"_data",sep="")))[['sd']]
    plot_points(Havg,col)
    #arrows(dxlist, Havg-Hsd, dxlist, Havg+Hsd,
    #	   length=0.05, angle=90, code=3,col=col)
    }
legendvec = c("Non-hydrostatic","Hydrostatic")
legend("topleft", legend = legendvec ,lwd=lwd, col = colvec,cex =cex_leg)


# D/H
plot_frame(ylim=c(0.1,30), ylab = 'D/H', main="(c)  Parcel aspect ratio",log="xy")
for (i in 1:2){
    solver = solvers[i]
    col  = colvec[i]
    Davg = eval(as.name(paste("D",solver,"_data",sep="")))[['mean']]
    Havg = eval(as.name(paste("H",solver,"_data",sep="")))[['mean']]
    aspect_avg = eval(as.name(paste("aspect_",solver,"_data",sep="")))[['mean']]
#    plot_points(Davg/Havg,col)
    plot_points(aspect_avg,col)
    #arrows(dxlist, Havg-Hsd, dxlist, Havg+Hsd,
    #	   length=0.05, angle=90, code=3,col=col)
    }
abline(h=1,lty="dashed",lwd=1.25)
text(10,1.25,"D/H = 1",cex=1.5)
dev.off()


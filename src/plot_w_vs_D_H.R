library(ncdf)
projectdir = "~/Dropbox/w_hydrostatic"
load(paste(projectdir,"/data/D_H.Rdata",sep=""))
source(paste(projectdir,"/src/beta_formulae.R",sep=""))
source("~/Dropbox/Rtools/thermo_tools.R")

dx  = 2
solver = "nh"
output_period = 1    # days
avg_period    = 20   # days
nt_avg        = avg_period/output_period


#======#
# Data #
#======#
dxcase = paste("dx",dx,sep="")    
        suite_data  = eval(as.name(paste("fv3_",solver,"_prod_data",sep="")))
case_data = suite_data[[dxcase]]
varlist = c("D","H","Bmax","wmax")
    	for (var in varlist) {
    assign(substr(var,1,1),case_data[[var]])
    }
assign("beta",eval(as.name(paste("beta_",solver,sep=""))))
wsticky_pre = sqrt(2*H*B*beta(D,H))
lsfit = lm(w ~ 0 + wsticky_pre)
rsquared = round(summary(lsfit)$r.squared,digits=2)    
Cd    = round(1/(lsfit$coefficients[1])^2,digits=2)	
wsticky = sqrt(2*H/Cd*B*beta(D,H))
    
#=======#
# Plots #
#=======#
cex=1.5
wnhlim  = c(0,25) 
whydlim = wnhlim
wlim    = c(0,max(w))
wvals   = seq(wlim[1],wlim[2],length.out=100)
main	= paste("Non-hydrostatic, dx = ",dx," km",sep="")
plotfile=paste(projectdir,"/plots/w_vs_D_H.pdf",sep="")
plot_it = function(x,y,xlab,ylab){
			plot(x,y,
				main = main,
				xlab = xlab,
				ylab = ylab,
				pch  = 16,
				cex	 = 0.7,
				cex.main = 1.35,
				cex.lab = cex,
				cex.axis = cex
				)
		
			}
			
pdf(plotfile,width = 9, height = 4.5 )
par(mfrow=c(1,2),mar=c(5,5,3,2))
plot_it(1e-3*D,w, xlab = "D (km)",ylab = expression(w[c]~~"(m/s)") )
lsfit = lm(w ~ D )
rsquared = round(summary(lsfit)$r.squared,digits=2)  
text(2.5,max(w),bquote(R^2 == .(rsquared)))

plot_it(1e-3*H,w,xlab = "H (km)",ylab = expression(w[c]~~"(m/s)") )
lsfit = lm(w ~ H )
rsquared = round(summary(lsfit)$r.squared,digits=2)  
text(0.1*max(1e-3*H),max(w),bquote(R^2 == .(rsquared)))

dev.off()


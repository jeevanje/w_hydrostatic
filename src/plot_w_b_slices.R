library(ncdf)
library(fields)
projectdir = "~/Dropbox/w_hydrostatic"
load(paste(projectdir,"/data/D_H.Rdata",sep=""))
source("~/Dropbox/Rtools/my_image_plot.R")

dx     = 0.25
solver = "nh"
ndays  = 30
lparcel= 11
l      = lparcel + ndays - 21 
j      = 27  # 22, 64 ?
# dx   = 2
# ndays=60
#l      = 40
#j      = 42  # 44?

#============#
# field data #
#============#

ncpath = paste("/work/Nadir.Jeevanjee/w_hydrostatic/fv3_nh_prod/dx",dx,
		"_nh_3d_inst.nc",sep="")
nc     = open.ncdf(ncpath)
x      = get.var.ncdf(nc,"grid_xt")
y      = get.var.ncdf(nc,"grid_yt")
pfull  = get.var.ncdf(nc,"pfull")
np     = length(pfull)
nx     = length(x)
start  = c(1,j,1,l)
count  = c(nx,1,np,1)
B      = get.var.ncdf(nc,"B",start = start, count = count)
w      = get.var.ncdf(nc,"w",start = start, count = count)
qn      = get.var.ncdf(nc,"qn",start = start, count = count)

#==============#
# parcel data  #
#==============#

dxcase = paste("dx",dx,sep="")    
suite_data  = eval(as.name(paste("fv3_",solver,"_prod_data",sep="")))
case_data = suite_data[[dxcase]]
varlist = c("wmax","Bmax","H","D","xind","yind","zind","tind","kbot","ktop")
        for (var in varlist) {
    assign(var,case_data[[var]])
    }
parcel_indices = which(tind == lparcel & (yind == j | yind == (j-1) | yind== (j+1)) )
Nparcels = length(parcel_indices)
iparcels = xind[parcel_indices]
Dparcels = D[parcel_indices]
kbots    = kbot[parcel_indices]
ktops    = ktop[parcel_indices]
xlists   = list()
plists   = list()

# polygon points, start at upper left, proceed clockwise
for (n in 1:Nparcels){
    iparcel = iparcels[n]
    Dparcel = Dparcels[n]
    kbot    = kbots[n]
    ktop    = ktops[n]

    xleft  = iparcel - Dparcel/2/(1e3*dx) - 0.5
    xright = iparcel + Dparcel/2/(1e3*dx) - 0.5
    xlist  = c(xleft, xright, xright, xleft)
    ptop   = pfull[ktop]
    pbot   = pfull[kbot]
    plist  = c(ptop,ptop,pbot,pbot)
    xlists[[toString(n)]] = xlist    
    plists[[toString(n)]] = plist    
    }

#======#
# Plot #
#======#
xvec = (min(iparcels)-10):(max(iparcels) + 10)
pmin = 200
kmin = which.min((abs(pfull-pmin)))
pvec = kmin:np
#xvec = 1:nx
wlim = c(-5,max(w[xvec,pvec]))
cex  = 1.25
plot_it = function(field,main,zlim=range(field[xvec,pvec])) {
	my.image.plot(x[xvec],pfull[pvec],field[xvec,pvec], 
		    xlab = "nx", 
		    ylab = "Pressure (mb)",
		    ylim = rev(range(pfull[pvec])),
	            main = main,
		    zlim = zlim, 
		    cex.axis = cex,
		    cex.main = cex,
		    cex.lab  = cex,
		    cex.legend = cex
	     	    )	
        }
do_poly = function(xlist,plist){
	polygon(xlist,plist,lwd=2,border="black")
	}

pdf(file="~/Dropbox/w_hydrostatic/plots/w_b_slices.pdf",width = 10, height = 6)
par(mfrow=c(1,2),mar=c(5,5,5,7))
plot_it(w,"Vertical velocity (m/s)",wlim)
for (n in 1:Nparcels){
    xlist = xlists[[toString(n)]]
    plist = plists[[toString(n)]]
    do_poly(xlist,plist)
    }
plot_it(B,expression(bold("Buoyancy ("*m/s^2*")")))
for (n in 1:Nparcels){
    xlist = xlists[[toString(n)]]
    plist = plists[[toString(n)]]
    do_poly(xlist,plist)
    }
#plot_it(1e3*qn,"Non-precipitating condensate (g/kg)")
#for (n in 1:Nparcels){
#    xlist = xlists[[toString(n)]]
#    plist = plists[[toString(n)]]
#    do_poly(xlist,plist)
#    }
dev.off()


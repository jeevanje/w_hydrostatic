projectdir = "~/Dropbox/w_hydrostatic"
load(paste(projectdir,"/data/D_H.Rdata",sep=""))
source(paste(projectdir,"/src/beta_formulae.R",sep=""))

dxlist   = c(0.0625, 0.125,0.25,0.5,1,2,4,8,16)
Ndx      = length(dxlist)
solvers  = c("nh","hyd")
varlist  = c("D","H","aspect","w")
proxy_list = c("M16","PG06","W97")
Nproxy   = length(proxy_list)

#=======#
# Data  #
#=======#

for (solver in solvers){
    solver_data  = eval(as.name(paste("fv3_",solver,"_prod_data",sep="")))  
    aspectvec    = numeric(Ndx)
    wvec         = numeric(Ndx)
    for (i in 1:Ndx){
    	dx     = dxlist[i]
	case   = paste("dx",dx,sep="")
	dxdata = solver_data[[case]]
	Ddata  = dxdata$D
	Hdata  = dxdata$H
	wdata   = dxdata$wmax
	aspectdata = Ddata/Hdata
	aspectvec[i] = mean(aspectdata)
	wvec[i] = mean(wdata)
	}
    assign(paste("aspect",solver,"_vec",sep=""),aspectvec)
    assign(paste("w",solver,"_vec",sep=""),wvec)
    }

#========#
# Params #
#========#

cex 	 = 2.25
cex_pch  = 1
colvec   = c("black","red")
dxlim    = range(dxlist)
DHlim	 = c(0.1,500)
wlim  	 = c(0.01,3.5) 
pch      = 8 
lwd      = 3
H        = 1
nref	 = 2 # index of run to use for wcref
Dvals    = exp(seq(log(DHlim[1]*H),log(DHlim[2]*H),length.out=100))
lty_list = c("dashed","dotted","dotdash")
plot_points = function(x,y,col){
                points(x,y,type="p",pch=pch,lwd=1,col=col,cex=cex_pch)
		}

#=======#
# Plots #
#=======#

plotfile=paste(projectdir,"/plots/w_vs_aspect_others.pdf",sep="")
pdf(plotfile,width = 15, height =4 )
par(mfrow=c(1,3),mar=c(5,5,5,3))
for (i in 1:Nproxy){
    name    = proxy_list[i]
    w_nh    = eval(as.name(paste("w_nh_",name,sep="")))
    w_hyd   = eval(as.name(paste("w_hyd_",name,sep="")))
    lsfit  = lm(wnh_vec ~ 0 + w_nh(aspectnh_vec,1))
    wnhref = round(lsfit$coefficients[1],digits=2)
#    wnhref  = wnh_vec[nref]/w_nh(aspectnh_vec[nref],H)  #m/s
    whydref = wnhref  

    plot(1,type="n",xlim=DHlim, ylim = wlim,
	    main = name,
	    log  = "x",
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

    #proxies
    points(Dvals/H,w_hyd(Dvals,H),type="l",lwd=lwd,col=colvec[2])
    points(Dvals/H,w_nh(Dvals,H),type="l",lwd=lwd,col=colvec[1])
    
    # FV3
    plot_points(aspecthyd_vec,whyd_vec/whydref,col=colvec[2])
    plot_points(aspectnh_vec, wnh_vec/wnhref,col=colvec[1])


    if (i > 0 ){
	legend("topright",
		legend=c(paste(bquote(.(name))," nh",sep=""),
			paste(bquote(.(name))," hyd",sep=""),
			"FV3 nh","FV3 hyd"),
		pch = c(NA,NA,pch,pch),
		col = c("black","red","black","red"),
		#pt.cex = c(1,1, cex_pch,cex_pch),
		cex = 1.75,
		lwd = c(2,2,NA,NA)
		)		
    }		
    abline(v=1,lty="dashed",lwd=1)		
}
dev.off()


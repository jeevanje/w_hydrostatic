pdf(file="~/Dropbox/w_hydrostatic/plots/gray_zone_schem.pdf",width = 9,height =6)
par(mar=c(5,3,5,2.5))
cex = 1.75
lwd =2.5
xmin = 1e-1
xmax = 250
xgray_min = 0.250  # km
xgray_max = 100 
wc = 10  # m/s
wmax = 4/3*wc
wmin = 2e-2*wmax
wc_col = "blue"
x_poly = c(xgray_min,xgray_max,xgray_max,xgray_min)
y_poly = c(-1,-1,wmax+1,wmax+1)
plot(1,type="n",xlab = "dx (km)", ylab = "",
		main= "The Gray Zone of Resolved-Scale Vertical Velocity",
		xlim = c(xmin,xmax),
		ylim = c(0,wmax),
		log  = "x",
		xaxs = "i",
		yaxs = "i",
		xaxt = "n",
		yaxt = "n",
		cex.main = cex,
		cex.lab = cex)
axis(1, at=c(xmin,xgray_min,1,10,100), 
	labels=c("","?",1,10,100),cex.axis = cex)
axis(2, at=wc, labels=bquote(w[c]~~"~"~~.(wc)~"m/s"),
	cex.axis = cex,hadj =0,col.axis = wc_col,lwd.ticks =0)
axis(4, at=wmin, labels=expression(w[c]~~"~ 1 cm/s"),
	cex.axis = cex,hadj =0,col.axis = wc_col,lwd.ticks =0)
polygon(x_poly,y_poly,col="gray",border=NA)		
lines(c(xgray_min,xgray_max),c(wc,wmin),lty="dashed",lwd=lwd)
text(5,0.5*wmax,labels="?",cex=3)

points(c(0.1*xmin,xgray_min),c(wc,wc),type="l",col="blue",lwd=lwd)
points(c(xgray_max,xmax),c(wmin,wmin),type="l",col="blue",lwd=lwd)
		
		
dev.off()
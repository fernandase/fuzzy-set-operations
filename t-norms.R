source("membership-functions.R")

#MINIMUM T-NORM

min.tnorm = function(fa, fb){
	m = c()
	for(i in 1:length(fa)){
		m[i] = min(fa[i], fb[i])
	}
	m;
}

#LUKASIEWICZ T-NORM

lukasiewicz.tnorm = function(fa, fb){
	m = c()
	for(i in 1:length(fa)){
		m[i] = max(0, (fa[i] + fb[i] - 1))
	}
	m;
}

#DRASTIC PRODUCT T-NORM

drastic.tnorm = function(fa, fb){
	m = c()
	for(i in 1:length(fa)){
		if (is.element(1, c(fa[i], fb[i]))){
			if(fa[i] == 1){
				m[i] = fb[i];
			}else{
				m[i] = fa[i];
			}
		}else{
			m[i] = 0;
		}
	}
	m;
}

plot.tnorms = function(fuzzy.number.a, fuzzy.number.b, tnorm){
  
  tnorms = list("Minimum T-norm" = min.tnorm, "Product T-norm" = function(a, b) a * b, 
                "Lukasiewicz T-norm" = lukasiewicz.tnorm, "Drastic Product T-norm" = drastic.tnorm)
  
  if (!missing(tnorm))
    tnorm = tnorms[pmatch(tolower(tnorm), tolower(names(tnorms)))]
  else
    tnorm = tnorms
  
  base.set.a = seq(fuzzy.number.a["min.value"], fuzzy.number.a["max.value"], by=0.1)
  base.set.b = seq(fuzzy.number.b["min.value"], fuzzy.number.b["max.value"], by=0.1)
  
  fuzzy.set.a = triangular.membership.function(base.set.a, fuzzy.number.a)
  fuzzy.set.b = triangular.membership.function(base.set.b, fuzzy.number.b)
  
  base.set.inter = intersect(base.set.a, base.set.b)
  
  fuzzy.set.a.inter = triangular.membership.function(base.set.inter, fuzzy.number.a)
  fuzzy.set.b.inter = triangular.membership.function(base.set.inter, fuzzy.number.b)
  
  xlim.plot = c(min(fuzzy.number.a["min.value"], fuzzy.number.b["min.value"]), 
                max(fuzzy.number.a["max.value"], fuzzy.number.b["max.value"]))
  
  if (length(tnorm) > 1){
    par(mfrow = c(2, 2), mai=c(0.45,0.45,0.55,0.25))
  }
  
  for(i in 1:length(tnorm)){
    
    #plot fuzzy.set.a
    plot(base.set.a, fuzzy.set.a, type = "l", xlim = xlim.plot, ylim = c(0,1), lty = 2, xlab = "", ylab = "", main = names(tnorm[i]))
    #axis(1, at=xlim.plot[1]:xlim.plot[2], labels=xlim.plot[1]:xlim.plot[2], las = 1)
    
    #plot fuzzy.set.b
    points(base.set.b, fuzzy.set.b, type = "l", ylim = c(0,1), col = "red", xaxt = "n", lty = 2)
    
    #add a fuzzy t-norm
    points(base.set.inter, tnorm[[i]](fuzzy.set.a.inter, fuzzy.set.b.inter), type = "l", col="blue", lwd = 3)
    
    legend("topleft", legend=c(names(tnorm[i])), col=c('blue'), lwd = 3, cex=0.9, bty='n')
  }
  
}
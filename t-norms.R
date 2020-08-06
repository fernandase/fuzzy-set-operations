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

plot.tnorms = function(fuzzy.set.a, fuzzy.set.b, tnorm){
    
    tnorms = list("Minimum T-norm" = min.tnorm, "Product T-norm" = function(a, b) a * b, 
                  "Lukasiewicz T-norm" = lukasiewicz.tnorm, "Drastic Product T-norm" = drastic.tnorm)
    
    if (!missing(tnorm))
        tnorm = tnorms[pmatch(tolower(tnorm), tolower(names(tnorms)))]
    else
        tnorm = tnorms

    base.set.a = seq(fuzzy.set.a["min.value.a"], fuzzy.set.a["max.value.a"], by=0.1)
    base.set.b = seq(fuzzy.set.b["min.value.b"], fuzzy.set.b["max.value.b"], by=0.1)

    fuzzy.number.a = triangular.membership.function(base.set.a, fuzzy.set.a)
    fuzzy.number.b = triangular.membership.function(base.set.b, fuzzy.set.b)
    
    base.set.inter = intersect(base.set.a, base.set.b)

    fuzzy.number.a.inter = triangular.membership.function(base.set.inter, fuzzy.set.a)
    fuzzy.number.b.inter = triangular.membership.function(base.set.inter, fuzzy.set.b)
    
    xlim.plot = c(min(fuzzy.set.a["min.value.a"], fuzzy.set.b["min.value.b"]) - 1, 
                  max(fuzzy.set.a["max.value.a"], fuzzy.set.b["max.value.b"]) + 1)
    
    if (length(tnorm) > 1){
        par(mfrow=c(2,2))
    }
    
    for(i in 1:length(tnorm)){
        
        #plot fuzzy.number.a
        plot(base.set.a, fuzzy.number.a, type = "l", xlim = xlim.plot, ylim = c(0,1), lty = 2, xlab = "", ylab = "", 
             main = names(tnorm[i]))
        axis(1, at=xlim.plot[1]:xlim.plot[2], labels=xlim.plot[1]:xlim.plot[2], las = 1)
        
        #plot fuzzy.number.b
        points(base.set.b, fuzzy.number.b, type = "l", xlim = xlim.plot, ylim = c(0,1), col = "red", xaxt = "n", lty = 2)
        
        #add a fuzzy t-norm
        points(base.set.inter, tnorm[[i]](fuzzy.number.a.inter, fuzzy.number.b.inter), type = "l", col="blue", lwd = 3)
        legend("topleft", legend=c("Fuzzy Set A", "Fuzzy Set B"), col=c("black", "red"), lty=1, cex=0.7)
    }
    
}
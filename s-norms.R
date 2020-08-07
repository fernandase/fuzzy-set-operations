source("membership-functions.R")

#MAXIMUM S-NORM

max.snorm = function(fa, fb){
	m = c()
	for(i in 1:length(fa)){
		m[i] = max(fa[i], fb[i])
	}
	m;
}

#BOUNDED SUM (LUKASIEWICZ)

bsum.snorm = function(fa, fb){
	m = c()
	for(i in 1:length(fa)){
		m[i] = min(1, fa[i] + fb[i])
	}
	m;
}

#DRASTIC SUM

drastic.snorm = function(fa, fb){
	m = c()
	for(i in 1:length(fa)){
		if (is.element(0, c(fa[i], fb[i]))){
			if(fa[i] == 0){
				m[i] = fb[i];
			}else{
				m[i] = fa[i];
			}
		}else{
			m[i] = 1;
		}
	}
	m;
}

plot.snorms = function(fuzzy.set.a, fuzzy.set.b, snorm){
    
    snorms = list("Maximum S-norm" = max.snorm, "Probabilistic S-norm" = function(a, b) a + b - a * b, 
                  "Bounded S-norm" = bsum.snorm, "Drastic S-norm" = drastic.snorm)
    
    if (!missing(snorm))
        snorm = snorms[pmatch(tolower(snorm), tolower(names(snorms)))]
    else
        snorm = snorms

    base.set.a = seq(fuzzy.set.a["min.value.a"], fuzzy.set.a["max.value.a"], by=0.1)
    base.set.b = seq(fuzzy.set.b["min.value.b"], fuzzy.set.b["max.value.b"], by=0.1)

    fuzzy.number.a = triangular.membership.function(base.set.a, fuzzy.set.a)
    fuzzy.number.b = triangular.membership.function(base.set.b, fuzzy.set.b)
    
    base.set.union = union(round(base.set.a, 5), round(base.set.b, 5))

    fuzzy.number.a.union = triangular.membership.function(base.set.union, fuzzy.set.a)
    fuzzy.number.b.union = triangular.membership.function(base.set.union, fuzzy.set.b)

    xlim.plot = c(min(fuzzy.set.a["min.value.a"], fuzzy.set.b["min.value.b"]) - 1, 
                  max(fuzzy.set.a["max.value.a"], fuzzy.set.b["max.value.b"]) + 1)
    
    if (length(snorm) > 1){
        par(mfrow=c(2,2))
    }
    
    for(i in 1:length(snorm)){
        
        #plot fuzzy.number.a
        plot(base.set.a, fuzzy.number.a, type = "l", xlim = xlim.plot, ylim = c(0,1), lty = 2, xlab = "", ylab = "", 
             main = names(snorm[i]))
        axis(1, at=xlim.plot[1]:xlim.plot[2], labels=xlim.plot[1]:xlim.plot[2], las = 1)
        
        #plot fuzzy.number.b
        points(base.set.b, fuzzy.number.b, type = "l", xlim = xlim.plot, ylim = c(0,1), col = "red", xaxt = "n", lty = 2)
        
        #add a fuzzy s-norm
        points(base.set.union, snorm[[i]](fuzzy.number.a.union, fuzzy.number.b.union), type = "l", col="blue", lwd = 3)
        legend("topleft", legend=c("Fuzzy Set A", "Fuzzy Set B"), col=c("black", "red"), lty=1, cex=0.7)
    }
    
}
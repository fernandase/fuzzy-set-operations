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

plot.snorms = function(fuzzy.number.a, fuzzy.number.b, snorm){
  
  snorms = list("Maximum S-norm" = max.snorm, "Probabilistic S-norm" = function(a, b) a + b - a * b, 
                "Bounded S-norm" = bsum.snorm, "Drastic S-norm" = drastic.snorm)
  if (!missing(snorm))
    snorm = snorms[pmatch(tolower(snorm), tolower(names(snorms)))]
  else
    snorm = snorms
  
  base.set.a = seq(fuzzy.number.a["min.value"], fuzzy.number.a["max.value"], by=0.1)
  base.set.b = seq(fuzzy.number.b["min.value"], fuzzy.number.b["max.value"], by=0.1)
  
  fuzzy.set.a = triangular.membership.function(base.set.a, fuzzy.number.a)
  fuzzy.set.b = triangular.membership.function(base.set.b, fuzzy.number.b)
  
  base.set.union = union(round(base.set.a, 5), round(base.set.b, 5))
  
  fuzzy.set.a.union = triangular.membership.function(base.set.union, fuzzy.number.a)
  fuzzy.set.b.union = triangular.membership.function(base.set.union, fuzzy.number.b)
  
  xlim.plot = c(min(fuzzy.number.a["min.value"], fuzzy.number.b["min.value"]) - 1, 
                max(fuzzy.number.a["max.value"], fuzzy.number.b["max.value"]) + 1)
  
  if (length(snorm) > 1){
    par(mfrow = c(2, 2), mai=c(0.45,0.45,0.55,0.25))
  }
  
  for(i in 1:length(snorm)){
    
    #plot fuzzy.number.a
    plot(base.set.a, fuzzy.set.a, type = "l", xlim = xlim.plot, ylim = c(0,1), lty = 2, xlab = "", ylab = "", main = names(snorm[i]))
    #axis(1, at=xlim.plot[1]:xlim.plot[2], labels=xlim.plot[1]:xlim.plot[2], las = 1)
    
    #plot fuzzy.number.b
    points(base.set.b, fuzzy.set.b, type = "l", ylim = c(0,1), col = "red", lty = 2)
    
    #add a fuzzy s-norm
    points(base.set.union, snorm[[i]](fuzzy.set.a.union, fuzzy.set.b.union), type = "l", col="blue", lwd = 3)
    legend("topleft", legend=c(names(snorm[i]), 'Fuzzy Set A', 'Fuzzy Set B'), col=c("blue", 'black', 'red'), lwd = c(3, 1, 1), lty = c(1, 2, 2), cex=0.8, bty = 'n')
  }
  
}
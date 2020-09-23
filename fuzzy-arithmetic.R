source("membership-functions.R")

add = function(fa, fb){
    out = list()
    
    out$base.set = seq(fa["min.value"] + fb["min.value"], 
                   fa["max.value"] + fb["max.value"], by=0.1)
    
    out$fuzzy.set = triangular.membership.function(out$base.set, c(min(out$base.set),
                                                    fa["mean.value"] + fb["mean.value"],
                                                    max(out$base.set)))
    
    out$mean = fa["mean.value"] + fb["mean.value"]
    
    out$simb = "+"
    
    out
}

sub = function(fa, fb){
    out = list()
    out$base.set = seq(fa["min.value"] - fb["max.value"], 
                   fa["max.value"] - fb["min.value"], by=0.1)

    out$fuzzy.set = triangular.membership.function(out$base.set, c(min(out$base.set),
                                                    fa["mean.value"] - fb["mean.value"],
                                                    max(out$base.set)))
    out$mean = fa["mean.value"] - fb["mean.value"]
    out$simb = "-"
    out
}

mult = function(fa, fb){
    out = list()
    
    mult = function(x, y) c(x * y, rev(x) * y)

    mult.ab = mult(c(fa["min.value"], fa["max.value"]),
                  c(fb["min.value"], fb["max.value"]))

    out$base.set = seq(min(mult.ab), max(mult.ab), by=0.1)

    #Multiplication approach given that the multiplication of two fuzzy triangular numbers does not result in a triangular number
    #Because of that we need to find the mean value of the resulting fuzzy triangular number by setting alfa = 1
    mean.base.set.mult = alfa_corte(fa, 1)[1] * alfa_corte(fb, 1)[1]

    out$fuzzy.set = triangular.membership.function(out$base.set, c(min(out$base.set), 
                                                                       mean.base.set.mult, max(out$base.set)))
    
    out$mean = mean.base.set.mult
    
    out$simb = "*"
    out
}

div = function(fa, fb){
    out = list()
    
    div = function(x, y) c(x / y, rev(x) / y)

    div.ab = div(c(fa["min.value"], fa["max.value"]),
                  c(fb["min.value"], fb["max.value"]))

    out$base.set = seq(min(div.ab), max(div.ab), by=0.01)

    #Division approach given that the diviplication of two fuzzy triangular numbers do not result in a triangular number
    #Because of that we need to find the mean value of the resulting fuzzy triangular number by setting alfa = 1
    mean.base.set.div = alfa_corte(fa, 1)[1] / alfa_corte(fb, 1)[1]

    out$fuzzy.set = triangular.membership.function(out$base.set, c(min(out$base.set), mean.base.set.div, max(out$base.set)))

    out$mean = mean.base.set.div    
    out$simb = "/"
    out
}

plot.ops = function(fuzzy.number.a, fuzzy.number.b, operation){
    
    operations = list("Addition" = add, "Subtraction" = sub, 
                      "Multiplication" = mult, "Division" = div)
    
    if (!missing(operation))
        op = operations[pmatch(tolower(operation), tolower(names(operations)))]
    else
        op = operations
    
    base.set.a = seq(fuzzy.number.a["min.value"], fuzzy.number.a["max.value"], by=0.1)
    base.set.b = seq(fuzzy.number.b["min.value"], fuzzy.number.b["max.value"], by=0.1)
    
    fuzzy.set.a = triangular.membership.function(base.set.a, fuzzy.number.a)
    fuzzy.set.b = triangular.membership.function(base.set.b, fuzzy.number.b)
    
    if (length(op) > 1){
        par(mfrow = c(2, 2), mai=c(0.45,0.45,0.55,0.25))
    }
    
    for(i in 1:length(op)){
        
        op.func = op[[i]](fuzzy.number.a, fuzzy.number.b)
        
        xlim.plot = c(min(fuzzy.number.a["min.value"], fuzzy.number.b["min.value"], 
                          min(op.func$base.set)), 
                      max(fuzzy.number.a["max.value"], fuzzy.number.b["max.value"],
                          max(op.func$base.set)))
        
        #plot fuzzy.number.a
        plot(base.set.a, fuzzy.set.a, type = "l", xlim = xlim.plot, ylim = c(0,1), xlab = "", ylab = "", lty = 2, lwd = 2, main = names(op[i]))
        
        #plot fuzzy.number.b
        points(base.set.b, fuzzy.set.b, type = "l", ylim = c(0,1), col = "red", xaxt = "n", lwd = 2, lty = 2)
        
        #add the fuzzy operation
        points(op.func$base.set, op.func$fuzzy.set, type = "l", ylim = c(0,1), lwd = 2, col = "brown")
        
        lines(rep(op.func$mean, 2), c(0, 1), col = 'brown', lwd = 1.5)
        text(op.func$mean + 1/4, 0.5, expression(italic(mu)))
        
        legend('topright', legend=c(paste("A", op.func$simb, "B")), 
               col=c("brown"), lwd = rep(2, 3), cex=0.9, bty="n")
    }
    
}
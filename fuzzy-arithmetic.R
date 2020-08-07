source("membership-functions.R")

add = function(fa, fb){
    out = list()
    out$base.set = seq(fa["min.value"] + fb["min.value"], 
                   fa["max.value"] + fb["max.value"], by=0.1)
    
    out$fuzzy.number = triangular.membership.function(out$base.set, c(min(out$base.set),
                                                    fa["mean.value"] + fb["mean.value"],
                                                    max(out$base.set)))
    out$simb = "+"
    out
}

sub = function(fa, fb){
    out = list()
    out$base.set = seq(fa["min.value"] - fb["max.value"], 
                   fa["max.value"] - fb["min.value"], by=0.1)

    out$fuzzy.number = triangular.membership.function(out$base.set, c(min(out$base.set),
                                                    fa["mean.value"] - fb["mean.value"],
                                                    max(out$base.set)))
    out$simb = "-"
    out
}

mult = function(fa, fb){
    out = list()
    
    mult = function(x, y) c(x * y, rev(x) * y)

    mult.ab = mult(c(fa["min.value"], fa["max.value"]),
                  c(fb["min.value"], fb["max.value"]))

    out$base.set = seq(min(mult.ab), max(mult.ab), by=0.1)

    #Multiplication approach given that the multiplication of two fuzzy triangular numbers do not result in a triangular number
    #Because of that we need to find the mean value of the resulting fuzzy triangular number by setting alfa = 1
    mean.base.set.mult = alfa_corte(fa, 1)[1] * alfa_corte(fb, 1)[1]

    out$fuzzy.number = triangular.membership.function(out$base.set, c(min(out$base.set), 
                                                                       mean.base.set.mult, max(out$base.set)))
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

    out$fuzzy.number = triangular.membership.function(out$base.set, c(min(out$base.set), mean.base.set.div, max(out$base.set)))

    out$simb = "/"
    out
}

plot.ops = function(fuzzy.set.a, fuzzy.set.b, operation){
    
    operations = list("Addition" = add, "Subtraction" = sub, 
                  "Multiplication" = mult, "Division" = div)
    
    if (!missing(operation))
        op = operations[pmatch(tolower(operation), tolower(names(operations)))]
    else
        op = operations

    base.set.a = seq(fuzzy.set.a["min.value"], fuzzy.set.a["max.value"], by=0.1)
    base.set.b = seq(fuzzy.set.b["min.value"], fuzzy.set.b["max.value"], by=0.1)

    fuzzy.number.a = triangular.membership.function(base.set.a, fuzzy.set.a)
    fuzzy.number.b = triangular.membership.function(base.set.b, fuzzy.set.b)
    
    if (length(op) > 1){
        par(mfrow=c(2,2))
    }
    
    for(i in 1:length(op)){
        
        op.func = op[[i]](fuzzy.set.a, fuzzy.set.b)
        
        xlim.plot = c(min(fuzzy.set.a["min.value"], fuzzy.set.b["min.value"], 
                  min(op.func$base.set)), 
                  max(fuzzy.set.a["max.value"], fuzzy.set.b["max.value"],
                      max(op.func$base.set)))

        if(length(xlim.plot[1]:xlim.plot[2]) > 30){
            axis.x = round(seq(xlim.plot[1], xlim.plot[2], 2), 2)
        }else{
            axis.x = round(xlim.plot[1]:xlim.plot[2], 1)
        }
        
        #plot fuzzy.number.a
        plot(base.set.a, fuzzy.number.a, type = "l", xlim = xlim.plot, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "", 
             lty = 2, main = names(op[i]))

        axis(1, at=axis.x, labels=axis.x, las = 1)
        
        #plot fuzzy.number.b
        points(base.set.b, fuzzy.number.b, type = "l", ylim = c(0,1), col = "red", xaxt = "n", lty = 2)

        #add the fuzzy operation
        points(op.func$base.set, op.func$fuzzy.number, type = "l", ylim = c(0,1), lwd = 3, col = "brown")

        legend("topright", legend=c("Fuzzy Set A", "Fuzzy Set B", paste("A", op.func$simb, "B")), 
               col=c("black", "red", "brown"), lty=1, cex=0.65)
    }
    
}
triangular.membership.function = function(base.set, set.values){

	set.values = sort(set.values)
	names(set.values) = c("min.value", "mean.value", "max.value")

	u = c();
	for (i in 1:length(base.set)){
		x = base.set[i];
		if (x < set.values["min.value"] || x > set.values["max.value"]){
			u[i] = 0;
		}
		else{
			if (x >= set.values["min.value"] && x < set.values["mean.value"]){
				u[i] = (x - set.values["min.value"])/(set.values["mean.value"] - set.values["min.value"]);
			}
			else{
				if(x >= set.values["mean.value"] && x <= set.values["max.value"]){
					u[i] = (set.values["max.value"] - x)/(set.values["max.value"] - set.values["mean.value"]);
				}
			}	
		}	
	}
	u;
}
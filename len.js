function len(received){
	var modifiable = received;
	var result = 0;
	var breaker = 0;
	for(;breaker==0;){
		if(modifiable!=[]){
			modifiable = tail(modifiable);
			result++;
		}else{
			breaker = 1;
		}
	}
	return result;
}

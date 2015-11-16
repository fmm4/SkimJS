//Conta até numero d e duplica ele.
var d = 400;
//


var a = 0;
var c = 999;

for(;a < c;){
	if(a == d){
		break;
	}else{
		a++;
	}
}

function paraRetornarAlgo(num){
	if(num < 20) {
		return num;
	}	

	inalcancavel = 9999;
	for (var i=0; i<num; i++){
		if(num == i+1) {
			break;
		}
		
		if(num < 3) {
			return num;
		}
	}
}

var algo = paraRetornarAlgo(15);


function k(){
	a = a*2;
}

k();


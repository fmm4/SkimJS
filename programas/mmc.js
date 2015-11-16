//Find index


function mmc(p1,p2)
{
	if(p2==0){
		return p1;
	}else{
		var v = p1%p2;
		return mmc(p2,v);
	}
}

var x = 48;	
var y = 18;
var mmcv = mmc(x,y);
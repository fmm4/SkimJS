function qs(ar,lf,rg){
	var lft = lf;
	var rgh = rg;
	var pivot = (lft+rgh)/2;
	for(;lft < rgh;){
		// for(;ar[lft]<ar[pivot];){
		// 	// lft++;
		// }
		// for(;ar[rgh]>ar[pivot];){
		// 	// rgh--;
		// }
		if(lft <= rgh){
			// var temp = ar[lft];
			// ar[lft] = ar[rgh];
			 ar[rgh] = temp;
			// lft++;
			// rgh--;
		}
	}
	if(lft > rg)
		qs(ar,lf,rgh);
	if(rgh < lf)
		qs(ar,lft,rg);
}

var lista = [3,4,1,2,3,5,6,9,12]
var start = 0;
var end = len(lista)-1;
qs(lista,start,end);

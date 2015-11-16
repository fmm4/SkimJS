function qsort(inici,fim) {
	var pEsq = inici;
	var pDir = fim;
	var meio = (inici+fim)/2;
	var sbk = 0;
	for(;pEsq < pDir;){
		for(;b[pEsq] < b[meio];){
            pEsq++;
        }
        for(;b[pDir] > b[meio];){
            pDir--;
        }
        if(pEsq<pDir){
        	b = swap(b,pEsq,pDir);
        	sbk += 1;
        	pDir--;
        	pEsq++;
        }
	}
	if(sbk==0){
	 	return sbk;
	}
	qsort(inici,pDir);
	qsort(pEsq,fim);
}


function swap(swappedArray, fPos,sPos){
	var transfArray = swappedArray;
	var temp = transfArray[fPos];
	transfArray[fPos] = transfArray[sPos];
	transfArray[sPos] = temp;
	return transfArray;
}

b = [4,2,3,1,5,9,6,7,0,8];
qsort(0,len(b)-1);
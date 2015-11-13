function qs(vet, esq, dir){
    var ce = esq;
    var cd = dir;
    var meio = (ce + cd)/ 2;
    for(;ce < cd;){
        for(;vet[ce] < vet[meio];){
            ce++;
        }
       	for(;vet[cd] > vet[meio];){
            cd--;
        }
        if(ce <= cd){
            var temp = vet[ce];
            vet[ce] = vet[cd];
            vet[cd] = temp;
            ce++;
            cd--;
        }
    }
    // if(cd > esq)
    //     qs(vet, esq, cd);

    // if(ce < dir)
    //     qs(vet, ce, dir);
}

var lista = [3,4,1,2,3,5,6,9,12]
var start = 0;
var end = len(lista)-1;
qs(lista,start,end);
// var cuck = 1;

// function mem(a1){
// 	a1++;
// }

// mem(cuck);


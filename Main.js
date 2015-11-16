// teste 1
// variaveis globais, automaticamente globais e locais
var a=0, b=1;
c=3;

function testaGlobal(){
	a++;
	d = a;
	e=0;
}

testaGlobal();

// teste 2
// testa if e else
function testaPositivo(num){
	if (num > 0){
		return 1;
	} else {
		return -1;
	}
}

var numero = testaPositivo(77);

// teste 3
// testa "for"

function contaAteNumero(num){
	var resultado;
	
	for (var i=1; i <= num; i++){
		resultado = i;
	}
	
	return resultado;
}

var meuValor = contaAteNumero(150);

// teste 4
// testa funcoes recursivas
function fat(num){
	if (num == 1){
		return num;
	}

	return fat(num-1) * num;
}

var fatorial = fat(5);

// teste 5
//

// teste 10
/*
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

*/

// var cuck = 1;

// function mem(a1){
// 	a1++;
// }

// mem(cuck);


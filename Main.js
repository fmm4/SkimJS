// teste 1
// variaveis globais, automaticamente globais e locais
// var a=0, b=1;
// c=3;

// function testaGlobal(){
// 	a++;
// 	d = a;
// 	e=0;
// }

// testaGlobal();

// teste 2
// testa if e else
// function testaPositivo(num){
// 	if (num > 0){
// 		return 1;
// 	} else {
// 		return -1;
// 	}
// }

// var numero = testaPositivo(77);

// // teste 3
// // testa "for"

// function contaAteNumero(num){
// 	var resultado;
	
// 	for (var i=1; i <= num; i++){
// 		resultado = i;
// 	}
	
// 	return resultado;
// }

// var meuValor = contaAteNumero(150);

// // teste 4
// // testa funcoes recursivas
// function fat(num){
// 	if (num == 1){
// 		return num;
// 	}

// 	return fat(num-1) * num;
// }

//var fatorial = fat(5);

// function fibonacci(num){
// 	if(num==1 || num==2) {
//        return 1;
// 	} else {
//        return fibonacci(num-1) + fibonacci(num-2);
// 	}
// }

// var fib = fibonacci(8);

// // teste 5
// // testando break

// function paraRetornarAlgo(num){
// 	if(num < 20) {
// 		return num;
// 	}	

// 	inalcancavel = 9999;
// 	for (var i=0; i<num; i++){
// 		if(num == i+1) {
// 			break;
// 		}
		
// 		if(num < 3) {
// 			return num;
// 		}
// 	}
// }

// var algo = paraRetornarAlgo(15);

// // teste 6
// // testando listas

var lista = [3,4,1,2,3,5,6,9,12]
var start = 0;
var end = len(lista)-1;
qs(lista,start,end);
// var cuck = 1;
var start = 0;
var end = len(lista)-1;
qs(lista,start,end);
// var cuck = 1;
var start = 0;
var end = len(lista)-1;
qs(lista,start,end);

// var comprimento = len(lista);

// var cabeca = head(lista);

// var rabo = tail(lista);

// var terceiroElemento = lista[2];

// // teste 7

// function mistura(posicao){
// 	b= 0;
// 	var comprimento = len(lista);
	
// 	if (posicao >= comprimento){
// 		return -1;
// 	}
	
// 	b = b + 1;
// 	c--;
	
// 	// nao sei se esse comportamento eh realmente executado
// 	var listaResult = concat([8,5,7,6,7,12,0], lista);
	
// 	for (var a = 0; a<comprimento; a++){
// 		automaGlobal = listaResult[a];
// 	}
	
// 	var valResult = listaResult[posicao];
	
// 	listaResult[posicao] = 99999999;
	
// 	return valResult;
// }

// f = mistura(4);

// // teste 8

// function closure(num){
// 	var fat = fatorial(num);
// 	var fib = fibonacci(num);
// 	var comp = len(lista);
// 	mashup = mistura(num);
// 	zero=0;
	
// 	if(comp > fib){
// 		return fat + fib + mashup;
// 	}
	
// 	if(comp > 1) {
// 		return 1 + fat + list[comp-1];
// 	} else {
// 		return 1 + fib + fat;
// 	}
// }

// // teste 9

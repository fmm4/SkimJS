//Fatorial de lista
//Pega uma lista, faz uma lista que contem seu fatorial.


function fat(a){
    if(a==0 || a ==1){
        return 1;
    }else{
        return a * fat(a-1);
    }
}

var b = [1,2,3,4,5];
var c = b;
var size = len(b);

for(var a = 0; a<size; a++){
    b[a] = fat(b[a]);
}
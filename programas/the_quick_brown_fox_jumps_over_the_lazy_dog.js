//Testing how globals behave and how string concatenate



var stack = [1,2,3];
var text = true;

function concatenate(input1,input2){
	var first = input1;
	var second = input2;
	return first+second;
}

var fullstring = concatenate("The quick brown fox ","jumps over the lazy dog");

var first = ["the","quick","brown","fox"];
var second = ["the","lazy","dog"];

var fulllist = concat(first,second);
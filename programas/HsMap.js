//Haskell Map


var original = [[8,4,7,10],[9,9,7,7],[2,4,3,5],[7,7,7,7]];

function avgGrades(vetRec)
{
	var size = len(vetRec);
	var sum = 0;
	for(var i = 0; i < size; i++)
	{
		sum = sum + vetRec[i];
	}
	sum = sum / size;
	return sum;
}

var size = len(original);
var modified = original;

for(var z = 0; z < size; z++)
{
	modified[z] = avgGrades(original[z]);
}


//Split

function popHalf(array)
{
	size = len(array);
	for(var i = 0; i < size/2; i++)
	{
		array = tail(array);
	}
	return array;
}


function revert(array)
{
	size = len(array);
	for(var i = 0; i < (size/2); i++)
	{
		array = swap(array,i, (size-1)-i);
	}
	return array;
}

function swap(swappedArray, fPos,sPos){
	var transfArray = swappedArray;
	var temp = transfArray[fPos];
	transfArray[fPos] = transfArray[sPos];
	transfArray[sPos] = temp;
	return transfArray;
}

var array = [1,2,3,4,5,6];
var rvrt = revert(array);
var leftPart = revert(popHalf(revert(array)));
var rightPart = popHalf(array);


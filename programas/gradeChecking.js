//Extract from list



var grades = [[9,5,9,9],[4,9,7,6],[10,7,7,6]];

var value1stTest = head(grades);
var value2ndTestAndProject = tail(grades);

function checkGrade(a){
	var grading = "A+";
	for(var f = 0; f < 4; f++){
		if(a < 10 && a >= 9 && grading == "A+"){
			grading = "A";
		}
		if(a < 9 && a >= 8 && (grading == "A+" || grading == "A")){
			grading = "B";
		}
		if(a < 8 && a >= 7 && (grading == "A+" || grading == "A" ||  grading == "B")){
			grading = "C";
		}
		if(a < 7 && a >= 6 && (grading == "A+" || grading == "A" ||  grading == "B" ||  grading == "C")){
			grading = "D";
		}
		if(a < 6){
			grading = "F";
		}
	}
	return grading;
}

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

var Test1 = checkGrade(avgGrades(value1stTest));
var Test2 = checkGrade(avgGrades(head(value2ndTestAndProject)))
var Test3 = checkGrade(avgGrades(head(tail(value2ndTestAndProject))))
var result;

if(Test1 == "D" || Test2 == "D" || Test3 == "D" || Test1 == "F" || Test2 == "F" || Test3 == "F"){
	result = "FAIL";
}else{
	result = "PASS";
}


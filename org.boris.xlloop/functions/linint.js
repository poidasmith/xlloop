var x = Number(args[2]);
var res = 0;
var xl = args[0].length;

if(xl == 0) {
	res = "#Invalid args";
} else if(xl == 1) {
	res = args[1][0];
} else if(x < args[0][0]) {
	var dy = args[1][1] - args[1][0];
	var dx = args[0][1] - args[0][0];
	res = args[1][0] - (args[0][0] - x) * (dy/dx);
} else if(x > Number(args[0][xl - 1])) {
	var dy = args[1][xl - 1] - args[1][xl - 2];
	var dx = args[0][xl - 1] - args[0][xl - 2];
	res = (x - args[0][xl - 1]) * (dy/dx) + Number(args[1][xl - 1]); 
} else {
	for(var i = 0; i < xl; i++) {
		if(i > 0 && args[0][i] >= x) {
			var dy = args[1][i] - args[1][i - 1];
			var dx = args[0][i] - args[0][i - 1];
			res = (x - args[0][i - 1]) * (dy/dx) + Number(args[1][i - 1]);
			break;
		}
	}
}

res;
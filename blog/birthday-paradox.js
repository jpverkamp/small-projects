var results = {};

function randomBirthday() {
	var d = new Date((new Date().getTime()) + 
		Math.floor(Math.random() * 365) * 
		24 * 60 * 60 * 1000);
	d.setHours(0);
	d.setMinutes(14);
	d.setSeconds(0);
	d.setMilliseconds(0);
	return d;
}

function date_sort(date1, date2) {
	if (date1 > date2) return 1;
	if (date1 < date2) return -1;
	return 0;
};

function b(n) {
	var prod = 1;
	for (var i = 1; i < n; i++)
		prod *= (1 - i / 365);
	return Math.round(10000 * (1 - prod)) / 100;
}

function doRun(n) {
	var dates = new Array();
	for (var i = 0; i < n; i++)
		dates.push(randomBirthday());

	dates.sort(date_sort);
	
	var found = false;
	for (var i = 0; i < n; i++)
		dates[i] = dates[i].toLocaleDateString();

	for (var i = 0; i < n; i++) {
		if ((i > 0 && dates[i] == dates[i - 1]) || 
				(i < n - 1 && dates[i] == dates[i + 1])) {
			found = true;
			break;
		}
	}

	if (!(n in results)) results[n] = {runs: 0, good: 0};
	results[n].runs += 1;
	if (found) results[n].good += 1;
}

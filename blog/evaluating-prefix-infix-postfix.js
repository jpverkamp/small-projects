// known operators
var operators = {
	'+': function(a, b) { return a + b; },
	'-': function(a, b) { return a - b; },
	'*': function(a, b) { return a * b; },
	'/': function(a, b) { return a / b; },
};
var precedence = [
	['*', '/'],
	['+', '-']
]

// evalute a list assuming prefix notation
function evalPrefix(input) {
	// process at this level
	// return the result of this level and what we didn't use
	// return a null value if we fail at any point
	function step(current) {
		// directly return numbers
		if (!isNaN(parseFloat(current[0]))) {
			return {
				value: parseFloat(current[0]),
				rest: current.slice(1)
			};
		}
	 
		// otherwise, we're going to have to recur
		else {
			var f = operators[current[0]];
	 
			// recur for left, use that rest for right
			var left = step(current.slice(1));
			if (left.value == null) return {value: null, rest: []};
			var right = step(left.rest);
			if (right.value == null) return {value: null, rest: []};
	 
			// return at my level
			return {
				value: f(left.value, right.value),
				rest: right.rest
			};
		}
	}
	return step(input).value;
}

// evaluate a list assuming infix notation
function evalInfix(input) {
	// process until we are done
	while (input.length > 1) {
		// find the first operator at the lowest level
		var reduceAt = 0;
		var found = false;
		for (var i = 0; i < precedence.length; i++) {
			for (var j = 1; j < input.length - 1; j++) {
				if ($.inArray(input[j], precedence[i]) >= 0) {
					reduceAt = j;
					found = true;
					break;
				}
			}
			if (found) break;
		}
	 
		// if we didn't find one, bail
		if (!found) return;
	 
		// otherwise, reduce that operator
		var newInput = [];
		var f = operators[input[reduceAt]];
	 
		for (var i = 0; i < reduceAt - 1; i++)
			newInput.push(input[i]);
	 
		newInput.push("" + f(
			parseFloat(input[reduceAt - 1]),
			parseFloat(input[reduceAt + 1])
		));
	 
		for (var i = reduceAt + 2; i < input.length; i++)
			newInput.push(input[i]);
	 
		input = newInput;
	}
	return input;
}

// evaluate a list assuming postfix notation
function evalPostfix(intpu) {
	// run through all commands in the input
	var stack = [];
	for (var i = 0; i < input.length; i++) {
		var cmd = input[i];
	 
		// known operator
		if (cmd in operators) {
			// get the function
			var f = operators[cmd];
	 
			// sanity check
			if (stack.length < f.length) {
				error = 'not enough arguments';
				break;
			}
	 
			// get the correct number of arguments
			var args = [];
			for (var j = 0; j < f.length; j++)
				args.unshift(stack.shift());
	 
			// apply and push back onto the stack
			// note: the first argument to apply is 'this'
			stack.unshift(f.apply(undefined, args));
		}
	 
		// anything else, push onto the stack as either a number or string
		else {
			stack.unshift(isNaN(parseFloat(cmd)) ? cmd : parseFloat(cmd));
		}
	}
	return stack[0];
}

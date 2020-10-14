(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.bT.aU === region.cg.aU)
	{
		return 'on line ' + region.bT.aU;
	}
	return 'on lines ' + region.bT.aU + ' through ' + region.cg.aU;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ef,
		impl.fg,
		impl.eV,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		S: func(record.S),
		bU: record.bU,
		bQ: record.bQ
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.S;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.bU;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.bQ) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ef,
		impl.fg,
		impl.eV,
		function(sendToApp, initialModel) {
			var view = impl.fh;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ef,
		impl.fg,
		impl.eV,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.bS && impl.bS(sendToApp)
			var view = impl.fh;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.dI);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.fa) && (_VirtualDom_doc.title = title = doc.fa);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.es;
	var onUrlRequest = impl.et;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		bS: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.cT === next.cT
							&& curr.ct === next.ct
							&& curr.cN.a === next.cN.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		ef: function(flags)
		{
			return A3(impl.ef, flags, _Browser_getUrl(), key);
		},
		fh: impl.fh,
		fg: impl.fg,
		eV: impl.eV
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { d9: 'hidden', dR: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { d9: 'mozHidden', dR: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { d9: 'msHidden', dR: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { d9: 'webkitHidden', dR: 'webkitvisibilitychange' }
		: { d9: 'hidden', dR: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		c1: _Browser_getScene(),
		dh: {
			dn: _Browser_window.pageXOffset,
			$7: _Browser_window.pageYOffset,
			dk: _Browser_doc.documentElement.clientWidth,
			cp: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		dk: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		cp: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			c1: {
				dk: node.scrollWidth,
				cp: node.scrollHeight
			},
			dh: {
				dn: node.scrollLeft,
				$7: node.scrollTop,
				dk: node.clientWidth,
				cp: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			c1: _Browser_getScene(),
			dh: {
				dn: x,
				$7: y,
				dk: _Browser_doc.documentElement.clientWidth,
				cp: _Browser_doc.documentElement.clientHeight
			},
			d3: {
				dn: x + rect.left,
				$7: y + rect.top,
				dk: rect.width,
				cp: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.bb.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.bb.b, xhr)); });
		$elm$core$Maybe$isJust(request.db) && _Http_track(router, xhr, request.db.a);

		try {
			xhr.open(request.en, request.n, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.n));
		}

		_Http_configureRequest(xhr, request);

		request.dI.a && xhr.setRequestHeader('Content-Type', request.dI.a);
		xhr.send(request.dI.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.co; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.e9.a || 0;
	xhr.responseType = request.bb.d;
	xhr.withCredentials = request.dA;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		n: xhr.responseURL,
		eP: xhr.status,
		eQ: xhr.statusText,
		co: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			eI: event.loaded,
			c4: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			ez: event.loaded,
			c4: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $author$project$Main$LinkClicked = function (a) {
	return {$: 26, a: a};
};
var $author$project$Main$UrlChanged = function (a) {
	return {$: 27, a: a};
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.g) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.j);
		} else {
			var treeLen = builder.g * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.k) : builder.k;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.g);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.j);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{k: nodeList, g: (len / $elm$core$Array$branchFactor) | 0, j: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {cm: fragment, ct: host, aW: path, cN: port_, cT: protocol, cU: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Main$GotTime = function (a) {
	return {$: 25, a: a};
};
var $author$project$Main$Male = 0;
var $author$project$Main$Ready = {$: 2};
var $author$project$Main$ValidInt = F3(
	function (value, valid, comment) {
		return {cc: comment, y: valid, ac: value};
	});
var $billstclair$elm_sortable_table$Table$State = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $billstclair$elm_sortable_table$Table$initialSort = function (header) {
	return A2($billstclair$elm_sortable_table$Table$State, header, false);
};
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $author$project$Main$init = F3(
	function (flags, url, key) {
		return _Utils_Tuple2(
			{
				H: A3($author$project$Main$ValidInt, $elm$core$Maybe$Nothing, false, 'Please enter an age'),
				ba: _List_fromArray(
					['']),
				au: $elm$core$Maybe$Nothing,
				av: $elm$core$Maybe$Nothing,
				aR: _List_Nil,
				ae: false,
				az: 0,
				R: key,
				T: '',
				aC: false,
				cH: $elm$core$Maybe$Nothing,
				cI: $billstclair$elm_sortable_table$Table$initialSort('Title'),
				bi: $elm$core$Maybe$Just('$144.60'),
				aX: $elm$core$Maybe$Nothing,
				U: $elm$core$Maybe$Nothing,
				bN: 2020,
				bO: 2021,
				V: false,
				W: false,
				X: false,
				am: '',
				bj: $elm$core$Maybe$Nothing,
				l: $author$project$Main$Ready,
				h: $elm$core$Maybe$Nothing,
				aa: $billstclair$elm_sortable_table$Table$initialSort('Category'),
				bY: $elm$core$Maybe$Nothing,
				aF: false,
				bZ: $elm$core$Maybe$Nothing,
				n: url,
				y: false,
				aI: false,
				aJ: false,
				aK: true,
				z: A3($author$project$Main$ValidInt, $elm$core$Maybe$Nothing, false, 'Please enter a 5-digit ZIP')
			},
			A2($elm$core$Task$perform, $author$project$Main$GotTime, $elm$time$Time$now));
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$none;
};
var $author$project$Main$Counties = 0;
var $author$project$MyDate$CustomDate = F2(
	function (month, year) {
		return {bg: month, dp: year};
	});
var $author$project$Main$Failure = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$Female = 1;
var $author$project$Main$Loading = {$: 1};
var $author$project$Main$NonPreferred = 1;
var $author$project$Main$Output = {$: 4};
var $author$project$Main$Outside = 2;
var $author$project$Main$PDP = 1;
var $author$project$Main$Plan = 2;
var $author$project$Main$Preferred = 0;
var $author$project$Main$Results = {$: 3};
var $elm$time$Time$Apr = 3;
var $elm$time$Time$Aug = 7;
var $elm$time$Time$Dec = 11;
var $elm$time$Time$Feb = 1;
var $elm$time$Time$Jan = 0;
var $elm$time$Time$Jul = 6;
var $elm$time$Time$Jun = 5;
var $elm$time$Time$Mar = 2;
var $elm$time$Time$May = 4;
var $elm$time$Time$Nov = 10;
var $elm$time$Time$Oct = 9;
var $elm$time$Time$Sep = 8;
var $author$project$MyDate$intMonth = function (i) {
	switch (i) {
		case 1:
			return 0;
		case 2:
			return 1;
		case 3:
			return 2;
		case 4:
			return 3;
		case 5:
			return 4;
		case 6:
			return 5;
		case 7:
			return 6;
		case 8:
			return 7;
		case 9:
			return 8;
		case 10:
			return 9;
		case 11:
			return 10;
		case 0:
			return 11;
		default:
			return 0;
	}
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$MyDate$monthInt = function (m) {
	switch (m) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		case 7:
			return 8;
		case 8:
			return 9;
		case 9:
			return 10;
		case 10:
			return 11;
		default:
			return 0;
	}
};
var $author$project$MyDate$addMonth = F2(
	function (i, cd) {
		var di = $author$project$MyDate$monthInt(cd.bg) + i;
		var newMonth = $author$project$MyDate$intMonth(
			A2($elm$core$Basics$modBy, 12, di));
		var newYear = cd.dp + (((di - 1) / 12) | 0);
		return A2($author$project$MyDate$CustomDate, newMonth, newYear);
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Main$errorToString = function (error) {
	switch (error.$) {
		case 0:
			var url = error.a;
			return 'The URL ' + (url + ' was invalid');
		case 1:
			return 'Unable to reach the server, try again';
		case 2:
			return 'Unable to reach the server, check your network connection';
		case 3:
			switch (error.a) {
				case 500:
					return 'The server had a problem, try again later';
				case 400:
					return 'Verify your information and try again';
				default:
					return 'Unknown error';
			}
		default:
			var errorMessage = error.a;
			return errorMessage;
	}
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Main$PDPResponse = function (a) {
	return {$: 17, a: a};
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.eP));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {cX: reqs, c8: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.db;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.cX));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.c8)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					dA: r.dA,
					dI: r.dI,
					bb: A2(_Http_mapExpect, func, r.bb),
					co: r.co,
					en: r.en,
					e9: r.e9,
					db: r.db,
					n: r.n
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{dA: false, dI: r.dI, bb: r.bb, co: r.co, en: r.en, e9: r.e9, db: r.db, n: r.n}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{dI: $elm$http$Http$emptyBody, bb: r.bb, co: _List_Nil, en: 'GET', e9: $elm$core$Maybe$Nothing, db: $elm$core$Maybe$Nothing, n: r.n});
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Main$PdpRecord = F3(
	function (plan, rate, year) {
		return {aY: plan, aD: rate, dp: year};
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$pdpPlanDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Main$PdpRecord,
	A2($elm$json$Json$Decode$field, 'Plan Name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'rate', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'year', $elm$json$Json$Decode$int));
var $author$project$Main$pdpDecoder = A2(
	$elm$json$Json$Decode$field,
	'body',
	$elm$json$Json$Decode$list($author$project$Main$pdpPlanDecoder));
var $author$project$Main$safeString = function (ms) {
	if (!ms.$) {
		var s = ms.a;
		return s;
	} else {
		return '';
	}
};
var $author$project$Main$getPDP = function (model) {
	var zip5 = $author$project$Main$safeString(model.z.ac);
	var year2 = $elm$core$String$fromInt(model.bO);
	var year1 = $elm$core$String$fromInt(model.bN);
	var base_url = 'https://medicare-school-quote-tool.herokuapp.com/api/pdp?';
	return $elm$http$Http$get(
		{
			bb: A2($elm$http$Http$expectJson, $author$project$Main$PDPResponse, $author$project$Main$pdpDecoder),
			n: base_url + ('zip=' + (zip5 + ('&year1=' + (year1 + ('&year2=' + year2)))))
		});
};
var $author$project$Main$PlanResponse = function (a) {
	return {$: 16, a: a};
};
var $author$project$Main$boolString = function (b) {
	return b ? 'True' : 'False';
};
var $author$project$Main$checkAddPlan = F3(
	function (b, plan, str) {
		return b ? (str + ('&plan=' + plan)) : str;
	});
var $author$project$Main$genderString = function (gender) {
	if (!gender) {
		return 'M';
	} else {
		return 'F';
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$PlanQuote = F5(
	function (company, fRate, gRate, nRate, naic) {
		return {at: company, aS: fRate, aT: gRate, aV: nRate, aA: naic};
	});
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Main$planRateDecoder = A6(
	$elm$json$Json$Decode$map5,
	$author$project$Main$PlanQuote,
	A2($elm$json$Json$Decode$field, 'company', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'F Rate',
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'G Rate',
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'N Rate',
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'naic', $elm$json$Json$Decode$int));
var $author$project$Main$planXDecoder = $elm$json$Json$Decode$list($author$project$Main$planRateDecoder);
var $author$project$Main$strCounty = function (c) {
	if (!c.$) {
		var s = c.a;
		return s;
	} else {
		return '';
	}
};
var $author$project$MyDate$formatRequest = function (cd) {
	var ys = $elm$core$String$fromInt(cd.dp);
	var mi = $author$project$MyDate$monthInt(cd.bg);
	var ms = (mi < 10) ? ('0' + $elm$core$String$fromInt(mi)) : $elm$core$String$fromInt(mi);
	return ys + ('-' + (ms + '-01'));
};
var $author$project$Main$strMaybeDate = function (ccd) {
	if (!ccd.$) {
		var cd = ccd.a;
		return $author$project$MyDate$formatRequest(cd);
	} else {
		return '';
	}
};
var $author$project$Main$getPlans = function (model) {
	if (model.y) {
		var url1 = 'https://medicare-school-quote-tool.herokuapp.com/api/plans?';
		var url2 = url1 + ('zip=' + ($author$project$Main$safeString(model.z.ac) + ('&age=' + ($author$project$Main$safeString(model.H.ac) + ('&county=' + ($author$project$Main$strCounty(model.au) + ('&gender=' + ($author$project$Main$genderString(model.az) + ('&tobacco=' + ($author$project$Main$boolString(model.aF) + ('&discounts=' + ($author$project$Main$boolString(model.ae) + ('&date=' + $author$project$Main$strMaybeDate(model.av))))))))))))));
		var url3 = A3($author$project$Main$checkAddPlan, model.X, 'N', url2);
		var url4 = A3($author$project$Main$checkAddPlan, model.V, 'F', url3);
		var url5 = A3($author$project$Main$checkAddPlan, model.W, 'G', url4);
		return $elm$http$Http$get(
			{
				bb: A2($elm$http$Http$expectJson, $author$project$Main$PlanResponse, $author$project$Main$planXDecoder),
				n: url5
			});
	} else {
		return $elm$core$Platform$Cmd$none;
	}
};
var $author$project$Main$ZipResponse = function (a) {
	return {$: 15, a: a};
};
var $author$project$Main$countyDecoder = A2(
	$elm$json$Json$Decode$field,
	'zip',
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string));
var $author$project$Main$getZip = function (model) {
	var zip = model.z.ac;
	if (!zip.$) {
		var z = zip.a;
		return $elm$http$Http$get(
			{
				bb: A2($elm$http$Http$expectJson, $author$project$Main$ZipResponse, $author$project$Main$countyDecoder),
				n: 'https://medicare-school-quote-tool.herokuapp.com/api/counties?zip=' + z
			});
	} else {
		return $elm$http$Http$get(
			{
				bb: A2($elm$http$Http$expectJson, $author$project$Main$ZipResponse, $author$project$Main$countyDecoder),
				n: 'https://medicare-school-quote-tool.herokuapp.com/api/counties?zip=' + ''
			});
	}
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$String$endsWith = _String_endsWith;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$String$trimRight = _String_trimRight;
var $author$project$Main$pdpFullString = function (pr) {
	var y_val = $elm$core$String$fromInt(pr.dp);
	var r_val = pr.aD;
	var p_name = A2(
		$elm$core$String$endsWith,
		'(PDP)',
		$elm$core$String$trimRight(pr.aY)) ? A3($elm$core$String$slice, 0, -6, pr.aY) : pr.aY;
	return y_val + ('   |   ' + (p_name + ('   |   ' + r_val)));
};
var $author$project$Main$TableRow = function (company) {
	return function (displayName) {
		return function (fRate) {
			return function (gRate) {
				return function (nRate) {
					return function (naic) {
						return function (uid) {
							return function (selected) {
								return function (category) {
									return function (priority) {
										return {a4: category, at: company, af: displayName, aS: fRate, aT: gRate, aV: nRate, aA: naic, cP: priority, F: selected, aG: uid};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Presets$naicCategory = {
	cD: _List_fromArray(
		[79413001, 79413002, 88366001, 31119, 61859, 63967, 64211, 65641, 65870, 66214, 66281, 67059, 67326, 68462, 68845, 76112, 79987, 86231]),
	cJ: _List_fromArray(
		[11121, 12358, 14933, 19178, 25178, 26581, 26921, 29076, 47027, 47051, 47058, 47350, 53139, 53872, 54704, 54771, 55891, 56014, 56383, 56499, 56693, 56820, 57053, 57347, 57991, 60016, 60128, 60176, 60183, 60836, 61115, 61239, 61700, 61751, 61999, 62065, 62146, 62553, 66141, 66583, 66828, 67539, 67628, 67679, 67784, 67814, 68420, 68543, 68802, 69132, 69663, 69698, 70122, 70408, 70769, 70939, 70998, 71390, 71773, 71919, 73504, 77216, 77828, 77950, 78743, 80578, 81043, 81200, 81701, 81779, 82538, 82880, 85189, 89005, 90212, 91472, 91785, 92916, 94587, 95561, 95683, 95725, 95796, 95839, 95844, 95923]),
	cO: _List_fromArray(
		[10345, 12321, 13100, 28207, 38520, 47171, 47570, 52618, 53228, 53287, 53473, 53589, 53902, 54631, 54720, 54828, 54933, 55026, 60093, 60131, 60219, 61557, 62825, 63444, 65722, 67369, 68500, 69868, 70580, 70670, 71412, 71835, 72052, 72850, 73288, 77780, 78700, 79413, 88366, 95120, 95610, 96016, 96202, 98167])
};
var $author$project$Main$findCategory = function (i) {
	return A2($elm$core$List$member, i, $author$project$Presets$naicCategory.cO) ? 0 : (A2($elm$core$List$member, i, $author$project$Presets$naicCategory.cD) ? 1 : 2);
};
var $author$project$Presets$displayNames = {
	cD: _List_fromArray(
		[
			_Utils_Tuple2(31119, 'Medico'),
			_Utils_Tuple2(61859, 'Christian Fidelity'),
			_Utils_Tuple2(63967, 'GPM'),
			_Utils_Tuple2(64211, 'GTL'),
			_Utils_Tuple2(65641, 'Medico'),
			_Utils_Tuple2(65870, 'Manhattan Life'),
			_Utils_Tuple2(66214, 'Heartland'),
			_Utils_Tuple2(66281, 'Transamerica'),
			_Utils_Tuple2(67059, 'GPM'),
			_Utils_Tuple2(67326, 'Old Surety'),
			_Utils_Tuple2(68462, 'Reserve National'),
			_Utils_Tuple2(68845, 'Shenandoah'),
			_Utils_Tuple2(76112, 'Oxford'),
			_Utils_Tuple2(79987, 'Medico'),
			_Utils_Tuple2(86231, 'Transamerica')
		]),
	cJ: _List_fromArray(
		[
			_Utils_Tuple2(11121, 'UNIFIED LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(12358, 'Avalon Insurance Company'),
			_Utils_Tuple2(14933, 'Montana Health Cooperative'),
			_Utils_Tuple2(19178, 'Southern Guaranty Insurance Company'),
			_Utils_Tuple2(25178, 'State Farm Mutual Automobile Insurance Company'),
			_Utils_Tuple2(26581, 'INDEPENDENCE AMERICAN INSURANCE COMPANY'),
			_Utils_Tuple2(26921, 'EVEREST REINSURANCE COMPANY'),
			_Utils_Tuple2(29076, 'Medical Mutual of Ohio'),
			_Utils_Tuple2(47027, 'CDPHP Universal Benefits, Inc.'),
			_Utils_Tuple2(47051, 'FirstCommunity Health Plan Inc.'),
			_Utils_Tuple2(47058, 'CAREFIRST OF MARYLAND, INC.'),
			_Utils_Tuple2(47350, 'ASURIS NORTHWEST HEALTH'),
			_Utils_Tuple2(53139, 'Wisconsin Physicians Service Insurance Corporation'),
			_Utils_Tuple2(53872, 'KPS Health Plans'),
			_Utils_Tuple2(54704, 'Independence Hospital Indemnity Plan, Inc.'),
			_Utils_Tuple2(54771, 'Highmark Inc.'),
			_Utils_Tuple2(55891, 'Noridian Mutual Insurance Company'),
			_Utils_Tuple2(56014, 'Thrivent Financial for Lutherans'),
			_Utils_Tuple2(56383, 'The Order Of United Commercial Travelers Of America'),
			_Utils_Tuple2(56499, 'Assured Life Association'),
			_Utils_Tuple2(56693, 'Greek Catholic Union Of The USA'),
			_Utils_Tuple2(56820, 'Polish Falcons of America'),
			_Utils_Tuple2(57053, 'Catholic United Financial'),
			_Utils_Tuple2(57347, 'Catholic Life Insurance'),
			_Utils_Tuple2(57991, 'Everence Association Inc'),
			_Utils_Tuple2(60016, 'THP Insurance Company'),
			_Utils_Tuple2(60128, 'Wellmark of South Dakota, Inc.'),
			_Utils_Tuple2(60176, 'Prosperity Life Group'),
			_Utils_Tuple2(60183, 'Prosperity Life Group'),
			_Utils_Tuple2(60836, 'AMERICAN REPUBLIC INSURANCE COMPANY'),
			_Utils_Tuple2(61115, 'ATLANTIC COAST LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(61239, 'BANKERS FIDELITY LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(61700, 'Renaissance Life & Health Insurance Company of America'),
			_Utils_Tuple2(61751, 'Central States Health and Life Co. of Omaha'),
			_Utils_Tuple2(61999, 'Great Southern Life Insurance Company'),
			_Utils_Tuple2(62065, 'COLONIAL PENN LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(62146, 'Combined Insurance Company of America'),
			_Utils_Tuple2(62553, 'COUNTRY Life Insurance Company'),
			_Utils_Tuple2(66141, 'HEALTH NET LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(66583, 'National Guardian Life Insurance Company'),
			_Utils_Tuple2(66828, 'FALLON HEALTH AND LIFE ASSURANCE CO. INC.'),
			_Utils_Tuple2(67539, 'Pan American Life Insurance Company'),
			_Utils_Tuple2(67628, 'PEKIN LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(67679, 'AMERICAN REPUBLIC CORP INSURANCE COMPANY'),
			_Utils_Tuple2(67784, 'Philadelphia American Life Insurance Company'),
			_Utils_Tuple2(67814, 'Nassau Life Insurance Company'),
			_Utils_Tuple2(68420, 'WMI MUTUAL INSURANCE COMPANY'),
			_Utils_Tuple2(68543, 'Liberty Bankers Life Insurance Company'),
			_Utils_Tuple2(68802, 'Sentinel Security Life Insurance Company'),
			_Utils_Tuple2(69132, 'STATE MUTUAL INSURANCE COMPANY'),
			_Utils_Tuple2(69663, 'USAA LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(69698, 'New Era Life Insurance Company of the Midwest'),
			_Utils_Tuple2(70122, 'UNIVERSAL FIDELITY LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(70408, 'UNION SECURITY INSURANCE COMPANY'),
			_Utils_Tuple2(70769, 'Erie Family Life Insurance Company'),
			_Utils_Tuple2(70939, 'GERBER LIFE INSURANCE COMPANY'),
			_Utils_Tuple2(70998, 'QualChoice Life and Health Insurance Company, Inc.'),
			_Utils_Tuple2(71390, 'Puritan Life Insurance Company of America'),
			_Utils_Tuple2(71773, 'American National Life Insurance Company of Texas'),
			_Utils_Tuple2(71919, 'Bankers Fidelity Assurance Company'),
			_Utils_Tuple2(73504, 'Lumico Life Insurance Company'),
			_Utils_Tuple2(77216, 'Aultcare Insurance Company'),
			_Utils_Tuple2(77828, 'Companion Life Insurance Company'),
			_Utils_Tuple2(77950, 'HEALTH ALLIANCE MEDICAL PLANS, INC.'),
			_Utils_Tuple2(78743, 'New Era Life Insurance Company'),
			_Utils_Tuple2(80578, 'PHYSICIANS MUTUAL INSURANCE COMPANY'),
			_Utils_Tuple2(81043, 'Bankers Life Insurance Company'),
			_Utils_Tuple2(81200, 'Louisiana Health Service and Indemnity Company'),
			_Utils_Tuple2(81701, 'Educators Mutual Insurance Association'),
			_Utils_Tuple2(81779, 'INDIVIDUAL ASSURANCE COMPANY, LIFE, HEALTH & ACCIDENT'),
			_Utils_Tuple2(82538, 'National Health Insurance Company'),
			_Utils_Tuple2(82880, 'CSI Life Insurance Company'),
			_Utils_Tuple2(85189, 'Western United Life Assurance Company'),
			_Utils_Tuple2(89005, 'Farm Bureau Health Plans'),
			_Utils_Tuple2(90212, 'Great Southern Life Insurance Company (Americo)'),
			_Utils_Tuple2(91472, 'GLOBE LIFE AND ACCIDENT INSURANCE COMPANY'),
			_Utils_Tuple2(91785, 'Equitable National Life Insurance Company'),
			_Utils_Tuple2(92916, 'UNITED AMERICAN INSURANCE COMPANY'),
			_Utils_Tuple2(94587, 'Members Health Insurance Company'),
			_Utils_Tuple2(95561, 'Priority Health'),
			_Utils_Tuple2(95683, 'Sanford Health Plan'),
			_Utils_Tuple2(95725, 'SANFORD HEALTH PLAN OF MINNESOTA'),
			_Utils_Tuple2(95796, 'Unity Health Plans Insurance Corporation'),
			_Utils_Tuple2(95839, 'AVERA HEALTH PLANS, INC.'),
			_Utils_Tuple2(95844, 'Health Alliance Plan of Michigan'),
			_Utils_Tuple2(95923, 'Geisinger Health Plan')
		]),
	cO: _List_fromArray(
		[
			_Utils_Tuple2(10345, 'Anthem'),
			_Utils_Tuple2(12321, 'AETNA'),
			_Utils_Tuple2(13100, 'Mutual of Omaha'),
			_Utils_Tuple2(28207, 'Anthem'),
			_Utils_Tuple2(38520, 'BCBS of SC'),
			_Utils_Tuple2(47171, 'BCBS of KC'),
			_Utils_Tuple2(47570, 'BCBS Premera'),
			_Utils_Tuple2(52618, 'Anthem of ME'),
			_Utils_Tuple2(53228, 'BCBS of MA'),
			_Utils_Tuple2(53287, 'BCBS Highmark'),
			_Utils_Tuple2(53473, 'BCBS of RI'),
			_Utils_Tuple2(53589, 'BCBS of AZ'),
			_Utils_Tuple2(53902, 'BCBS Regence'),
			_Utils_Tuple2(54631, 'BCBS of NC'),
			_Utils_Tuple2(54720, 'BCBS of PA'),
			_Utils_Tuple2(54828, 'BCBS of WV'),
			_Utils_Tuple2(54933, 'BCBS of OR'),
			_Utils_Tuple2(55026, 'BCBS of MN'),
			_Utils_Tuple2(60093, 'AARP of NY'),
			_Utils_Tuple2(60131, 'BCBS of ID'),
			_Utils_Tuple2(60219, 'Humana'),
			_Utils_Tuple2(61557, 'BCBS of CA'),
			_Utils_Tuple2(62825, 'Anthem BCBS'),
			_Utils_Tuple2(63444, 'Accendo'),
			_Utils_Tuple2(65722, 'CIGNA'),
			_Utils_Tuple2(67369, 'CIGNA'),
			_Utils_Tuple2(68500, 'AETNA'),
			_Utils_Tuple2(69868, 'Mutual of Omaha'),
			_Utils_Tuple2(70580, 'Humana'),
			_Utils_Tuple2(70670, 'BCBS IL/TX/NM/OK'),
			_Utils_Tuple2(71412, 'Mutual of Omaha'),
			_Utils_Tuple2(71835, 'Anthem VA/NV'),
			_Utils_Tuple2(72052, 'AETNA'),
			_Utils_Tuple2(72850, 'Mutual of Omaha'),
			_Utils_Tuple2(73288, 'Humana'),
			_Utils_Tuple2(77780, 'BCBS of NE'),
			_Utils_Tuple2(78700, 'AETNA'),
			_Utils_Tuple2(79413, 'AARP / UHC'),
			_Utils_Tuple2(88366, 'CIGNA'),
			_Utils_Tuple2(95120, 'Anthem of KY'),
			_Utils_Tuple2(95610, 'BCBS of MI'),
			_Utils_Tuple2(96016, 'AARP of AZ'),
			_Utils_Tuple2(96202, 'BCBS CareFirst'),
			_Utils_Tuple2(98167, 'BCBS of FL')
		])
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Main$findDisplayNameUtil = F2(
	function (i, tpls) {
		var ls = A2(
			$elm$core$List$filter,
			function (a) {
				return _Utils_eq(a.a, i);
			},
			tpls);
		var _v0 = $elm$core$List$head(ls);
		if (!_v0.$) {
			var tup = _v0.a;
			return $elm$core$Maybe$Just(tup.b);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Main$findDisplayName = F2(
	function (i, cat) {
		switch (cat) {
			case 0:
				return A2($author$project$Main$findDisplayNameUtil, i, $author$project$Presets$displayNames.cO);
			case 1:
				return A2($author$project$Main$findDisplayNameUtil, i, $author$project$Presets$displayNames.cD);
			default:
				return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$planToRow = F2(
	function (ii, pq) {
		var category = $author$project$Main$findCategory(pq.aA);
		var displayName = A2(
			$elm$core$Maybe$withDefault,
			pq.at,
			A2($author$project$Main$findDisplayName, pq.aA, category));
		var priority = function () {
			switch (category) {
				case 0:
					return 1;
				case 1:
					return 2;
				default:
					return 3;
			}
		}();
		var showRowInit = !category;
		return $author$project$Main$TableRow(pq.at)(displayName)(
			$author$project$Main$safeString(pq.aS))(
			$author$project$Main$safeString(pq.aT))(
			$author$project$Main$safeString(pq.aV))(pq.aA)(ii)(showRowInit)(category)(priority);
	});
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $author$project$Main$selectByUID = F2(
	function (ls, tablerow) {
		return A2($elm$core$List$member, tablerow.aG, ls) ? _Utils_update(
			tablerow,
			{F: true}) : tablerow;
	});
var $author$project$Main$setRows = F3(
	function (cat, b, trls) {
		return A2(
			$elm$core$List$map,
			function (a) {
				return _Utils_eq(a.a4, cat) ? _Utils_update(
					a,
					{F: b}) : a;
			},
			trls);
	});
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$toLower = _String_toLower;
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.bT, posixMinutes) < 0) {
					return posixMinutes + era.b;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		ce: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		bg: month,
		dp: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).bg;
		switch (_v0) {
			case 1:
				return 0;
			case 2:
				return 1;
			case 3:
				return 2;
			case 4:
				return 3;
			case 5:
				return 4;
			case 6:
				return 5;
			case 7:
				return 6;
			case 8:
				return 7;
			case 9:
				return 8;
			case 10:
				return 9;
			case 11:
				return 10;
			default:
				return 11;
		}
	});
var $author$project$MyDate$toString = function (cd) {
	var yearString = $elm$core$String$fromInt(cd.dp);
	var monthString = function () {
		var _v0 = cd.bg;
		switch (_v0) {
			case 0:
				return 'January';
			case 1:
				return 'February';
			case 2:
				return 'March';
			case 3:
				return 'April';
			case 4:
				return 'May';
			case 5:
				return 'June';
			case 6:
				return 'July';
			case 7:
				return 'August';
			case 8:
				return 'September';
			case 9:
				return 'October';
			case 10:
				return 'November';
			default:
				return 'December';
		}
	}();
	return monthString + (' ' + yearString);
};
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 1) {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 1) {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.cT;
		if (!_v0) {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.cm,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.cU,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.cN,
					_Utils_ap(http, url.ct)),
				url.aW)));
};
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).dp;
	});
var $author$project$Main$toggle = F2(
	function (i, tablerow) {
		return _Utils_eq(tablerow.aG, i) ? _Utils_update(
			tablerow,
			{F: !tablerow.F}) : tablerow;
	});
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {ag: frag, ak: params, ab: unvisited, ac: value, ao: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.ab;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.ac);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.ac);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 1) {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 1) {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 1) {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 1) {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.aW),
					$elm$url$Url$Parser$prepareQuery(url.cU),
					url.cm,
					$elm$core$Basics$identity)));
	});
var $elm$url$Url$Parser$Parser = $elm$core$Basics$identity;
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.ao;
		var unvisited = _v0.ab;
		var params = _v0.ak;
		var frag = _v0.ag;
		var value = _v0.ac;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0;
		return function (_v1) {
			var visited = _v1.ao;
			var unvisited = _v1.ab;
			var params = _v1.ak;
			var frag = _v1.ag;
			var value = _v1.ac;
			return A2(
				$elm$core$List$map,
				$elm$url$Url$Parser$mapState(value),
				parseArg(
					A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
		};
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return function (state) {
		return A2(
			$elm$core$List$concatMap,
			function (_v0) {
				var parser = _v0;
				return parser(state);
			},
			parsers);
	};
};
var $elm$url$Url$Parser$s = function (str) {
	return function (_v0) {
		var visited = _v0.ao;
		var unvisited = _v0.ab;
		var params = _v0.ak;
		var frag = _v0.ag;
		var value = _v0.ac;
		if (!unvisited.b) {
			return _List_Nil;
		} else {
			var next = unvisited.a;
			var rest = unvisited.b;
			return _Utils_eq(next, str) ? _List_fromArray(
				[
					A5(
					$elm$url$Url$Parser$State,
					A2($elm$core$List$cons, next, visited),
					rest,
					params,
					frag,
					value)
				]) : _List_Nil;
		}
	};
};
var $author$project$Main$routeParser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Ready,
			$elm$url$Url$Parser$s('home')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Results,
			$elm$url$Url$Parser$s('results')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Output,
			$elm$url$Url$Parser$s('output'))
		]));
var $author$project$Main$urlToRoute = function (url) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Main$Ready,
		A2($elm$url$Url$Parser$parse, $author$project$Main$routeParser, url));
};
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Main$isValid = function (model) {
	var _v0 = model.av;
	if (!_v0.$) {
		var validList = _List_fromArray(
			[
				$elm$core$String$length(model.T) > 0,
				model.H.y,
				model.z.y,
				model.X || (model.V || model.W),
				!_Utils_eq(model.au, $elm$core$Maybe$Nothing)
			]);
		var newModel = _Utils_update(
			model,
			{
				y: A3($elm$core$List$foldl, $elm$core$Basics$and, true, validList)
			});
		return newModel.y;
	} else {
		return false;
	}
};
var $author$project$Main$validateModel = function (model) {
	return _Utils_update(
		model,
		{
			y: $author$project$Main$isValid(model)
		});
};
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 28:
				var curl = model.n;
				var nurl = _Utils_update(
					curl,
					{aW: '/output'});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{l: $author$project$Main$Output, n: nurl}),
					A2(
						$elm$browser$Browser$Navigation$pushUrl,
						model.R,
						$elm$url$Url$toString(nurl)));
			case 29:
				var curl = model.n;
				var nurl = _Utils_update(
					curl,
					{aW: '/'});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{l: $author$project$Main$Ready, n: nurl}),
					A2(
						$elm$browser$Browser$Navigation$pushUrl,
						model.R,
						$elm$url$Url$toString(nurl)));
			case 30:
				var curl = model.n;
				var nurl = _Utils_update(
					curl,
					{aW: '/results'});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{l: $author$project$Main$Results, n: nurl}),
					A2(
						$elm$browser$Browser$Navigation$pushUrl,
						model.R,
						$elm$url$Url$toString(nurl)));
			case 26:
				var urlRequest = msg.a;
				if (!urlRequest.$) {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.R,
							$elm$url$Url$toString(url)));
				} else {
					var href = urlRequest.a;
					return _Utils_Tuple2(
						model,
						$elm$browser$Browser$Navigation$load(href));
				}
			case 27:
				var url = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							l: $author$project$Main$urlToRoute(url)
						}),
					$elm$core$Platform$Cmd$none);
			case 19:
				var newState = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aa: newState}),
					$elm$core$Platform$Cmd$none);
			case 18:
				var td = msg.a;
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{
								bZ: $elm$core$Maybe$Just(td)
							})),
					$elm$core$Platform$Cmd$none);
			case 0:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 1:
				var vModel = $author$project$Main$validateModel(model);
				return vModel.y ? _Utils_Tuple2(
					_Utils_update(
						vModel,
						{aC: false, bj: $elm$core$Maybe$Nothing, l: $author$project$Main$Loading}),
					$author$project$Main$getPlans(vModel)) : _Utils_Tuple2(
					_Utils_update(
						vModel,
						{l: $author$project$Main$Ready}),
					$elm$core$Platform$Cmd$none);
			case 2:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{l: $author$project$Main$Loading}),
					$author$project$Main$getPDP(model));
			case 3:
				var str = msg.a;
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{T: str})),
					$elm$core$Platform$Cmd$none);
			case 4:
				var str = msg.a;
				var _v2 = $elm$core$String$toInt(str);
				if (!_v2.$) {
					var i = _v2.a;
					var minAge = 65;
					var maxAge = 120;
					var errorMessage = (_Utils_cmp(i, minAge) < 1) ? 'Age must be 65 or older' : ((_Utils_cmp(i, maxAge) > 0) ? 'Seems too old; check age' : '');
					var ageTest = (_Utils_cmp(i, minAge) > -1) && (_Utils_cmp(i, maxAge) < 1);
					return _Utils_Tuple2(
						$author$project$Main$validateModel(
							_Utils_update(
								model,
								{
									H: A3(
										$author$project$Main$ValidInt,
										$elm$core$Maybe$Just(str),
										ageTest,
										errorMessage)
								})),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						$author$project$Main$validateModel(
							_Utils_update(
								model,
								{
									H: A3($author$project$Main$ValidInt, $elm$core$Maybe$Nothing, false, 'Must enter a valid integer')
								})),
						$elm$core$Platform$Cmd$none);
				}
			case 5:
				var str = msg.a;
				var _v3 = $elm$core$String$toInt(str);
				if (!_v3.$) {
					var i = _v3.a;
					if ($elm$core$String$length(str) === 5) {
						var newModel = $author$project$Main$validateModel(
							_Utils_update(
								model,
								{
									l: $author$project$Main$Ready,
									z: A3(
										$author$project$Main$ValidInt,
										$elm$core$Maybe$Just(str),
										true,
										'')
								}));
						return _Utils_Tuple2(
							newModel,
							$author$project$Main$getZip(newModel));
					} else {
						return _Utils_Tuple2(
							$author$project$Main$validateModel(
								_Utils_update(
									model,
									{
										z: A3(
											$author$project$Main$ValidInt,
											$elm$core$Maybe$Just(str),
											false,
											'Zip must be 5 digits long')
									})),
							$elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(
						$author$project$Main$validateModel(
							_Utils_update(
								model,
								{
									z: A3($author$project$Main$ValidInt, $elm$core$Maybe$Nothing, false, 'Zip must be a number')
								})),
						$elm$core$Platform$Cmd$none);
				}
			case 6:
				var gender = msg.a;
				var lg = $elm$core$String$toLower(gender);
				var g = (lg === 'female') ? 1 : 0;
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{az: g})),
					$elm$core$Platform$Cmd$none);
			case 7:
				var str = msg.a;
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{
								au: $elm$core$Maybe$Just(str)
							})),
					$elm$core$Platform$Cmd$none);
			case 8:
				var cds = msg.a;
				var choices_ = model.aR;
				var choiceTuple = $elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (a) {
							return _Utils_eq(a.a, cds);
						},
						choices_));
				var choice = A2($elm$core$Maybe$map, $elm$core$Tuple$second, choiceTuple);
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{av: choice})),
					$elm$core$Platform$Cmd$none);
			case 9:
				var prstr = msg.a;
				var prf = function () {
					var _v5 = model.aX;
					if (!_v5.$) {
						var pl = _v5.a;
						return $elm$core$Maybe$Just(
							A2(
								$elm$core$List$filter,
								function (a) {
									return _Utils_eq(
										$author$project$Main$pdpFullString(a),
										prstr);
								},
								pl));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				var pr = function () {
					if (!prf.$) {
						var l = prf.a;
						return $elm$core$List$head(l);
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{U: pr}),
					$elm$core$Platform$Cmd$none);
			case 10:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aF: !model.aF}),
					$elm$core$Platform$Cmd$none);
			case 11:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ae: !model.ae}),
					$elm$core$Platform$Cmd$none);
			case 12:
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{X: !model.X})),
					$elm$core$Platform$Cmd$none);
			case 13:
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{V: !model.V})),
					$elm$core$Platform$Cmd$none);
			case 14:
				return _Utils_Tuple2(
					$author$project$Main$validateModel(
						_Utils_update(
							model,
							{W: !model.W})),
					$elm$core$Platform$Cmd$none);
			case 20:
				var newBool = !model.aK;
				var newRows = function () {
					var _v6 = model.h;
					if (!_v6.$) {
						var tr = _v6.a;
						return $elm$core$Maybe$Just(
							A3($author$project$Main$setRows, 0, newBool, tr));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							h: newRows,
							aa: $billstclair$elm_sortable_table$Table$initialSort('category'),
							aK: newBool
						}),
					$elm$core$Platform$Cmd$none);
			case 21:
				var newRows = function () {
					var _v7 = model.h;
					if (!_v7.$) {
						var tr = _v7.a;
						return $elm$core$Maybe$Just(
							A3($author$project$Main$setRows, 1, false, tr));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				var newBool = !model.aI;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							h: newRows,
							aa: $billstclair$elm_sortable_table$Table$initialSort('category'),
							aI: newBool
						}),
					$elm$core$Platform$Cmd$none);
			case 22:
				var newRows = function () {
					var _v8 = model.h;
					if (!_v8.$) {
						var tr = _v8.a;
						return $elm$core$Maybe$Just(
							A3($author$project$Main$setRows, 2, false, tr));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				var newBool = !model.aJ;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							h: newRows,
							aa: $billstclair$elm_sortable_table$Table$initialSort('category'),
							aJ: newBool
						}),
					$elm$core$Platform$Cmd$none);
			case 31:
				var i = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							h: A2(
								$elm$core$Maybe$map,
								$elm$core$List$map(
									$author$project$Main$toggle(i)),
								model.h)
						}),
					$elm$core$Platform$Cmd$none);
			case 23:
				var newRows = function () {
					var _v9 = model.h;
					if (!_v9.$) {
						var tr = _v9.a;
						return $elm$core$Maybe$Just(
							A2(
								$elm$core$List$map,
								function (a) {
									return _Utils_update(
										a,
										{F: false});
								},
								tr));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{h: newRows}),
					$elm$core$Platform$Cmd$none);
			case 24:
				var ils = msg.a;
				var newRows = function () {
					var _v10 = model.h;
					if (!_v10.$) {
						var tr = _v10.a;
						if (!ils.$) {
							var ii = ils.a;
							return $elm$core$Maybe$Just(
								A2(
									$elm$core$List$map,
									$author$project$Main$selectByUID(ii),
									tr));
						} else {
							return $elm$core$Maybe$Nothing;
						}
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{h: newRows}),
					$elm$core$Platform$Cmd$none);
			case 15:
				var rmsg = msg.a;
				if (!rmsg.$) {
					var response = rmsg.a;
					return _Utils_Tuple2(
						$author$project$Main$validateModel(
							_Utils_update(
								model,
								{
									ba: response,
									au: $elm$core$List$head(response),
									l: $author$project$Main$Ready
								})),
						$author$project$Main$getPDP(model));
				} else {
					var error = rmsg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								l: $author$project$Main$Failure(0)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 16:
				var rmsg = msg.a;
				if (!rmsg.$) {
					var response = rmsg.a;
					var newRows = A2($elm$core$List$indexedMap, $author$project$Main$planToRow, response);
					var curl = model.n;
					var nurl = _Utils_update(
						curl,
						{aW: '/results'});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								aC: true,
								bj: $elm$core$Maybe$Just(response),
								l: $author$project$Main$Results,
								h: $elm$core$Maybe$Just(newRows),
								n: nurl
							}),
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.R,
							$elm$url$Url$toString(nurl)));
				} else {
					var error = rmsg.a;
					var curl = model.n;
					var eurl = _Utils_update(
						curl,
						{aW: '/error'});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								am: $author$project$Main$errorToString(error),
								l: $author$project$Main$Failure(2),
								n: eurl
							}),
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.R,
							$elm$url$Url$toString(eurl)));
				}
			case 17:
				var rmsg = msg.a;
				if (!rmsg.$) {
					var response = rmsg.a;
					var pr_sort = A2(
						$elm$core$List$sortBy,
						function ($) {
							return $.aY;
						},
						response);
					var prs = function () {
						var _v15 = $elm$core$List$head(pr_sort);
						if (!_v15.$) {
							var pr = _v15.a;
							return $elm$core$Maybe$Just(pr);
						} else {
							return $elm$core$Maybe$Nothing;
						}
					}();
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								aX: $elm$core$Maybe$Just(pr_sort),
								U: prs
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var error = rmsg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								am: $author$project$Main$errorToString(error),
								l: $author$project$Main$Failure(1)
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var timenow = msg.a;
				var td = A2(
					$author$project$MyDate$CustomDate,
					A2($elm$time$Time$toMonth, $elm$time$Time$utc, timenow),
					A2($elm$time$Time$toYear, $elm$time$Time$utc, timenow));
				var choices_ = A2(
					$elm$core$List$map,
					function (a) {
						return A2(
							$elm$core$Tuple$pair,
							$author$project$MyDate$toString(
								A2($author$project$MyDate$addMonth, a, td)),
							A2($author$project$MyDate$addMonth, a, td));
					},
					_List_fromArray(
						[0, 1, 2, 3]));
				var choiceVals = A2($elm$core$List$map, $elm$core$Tuple$second, choices_);
				var firstChoice = A2(
					$elm$core$Maybe$andThen,
					$elm$core$List$head,
					$elm$core$List$tail(choiceVals));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							av: firstChoice,
							aR: choices_,
							bY: $elm$core$Maybe$Just(td)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $author$project$Main$ShowOutput = {$: 28};
var $author$project$Main$ShowResults = {$: 30};
var $author$project$Main$ShowSubmitForm = {$: 29};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Main$navButton = F3(
	function (clist, msg, tx) {
		var classList = A2(
			$elm$core$List$map,
			function (a) {
				return $elm$html$Html$Attributes$class(a);
			},
			_Utils_ap(
				_List_fromArray(
					['two columns']),
				clist));
		return A2(
			$elm$html$Html$div,
			classList,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(msg),
							$elm$html$Html$Attributes$class('button-nav'),
							A2($elm$html$Html$Attributes$attribute, 'margin', '1em')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(tx)
						]))
				]));
	});
var $author$project$Main$navBar = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('row')
			]),
		_List_fromArray(
			[
				A3(
				$author$project$Main$navButton,
				_List_fromArray(
					['offset-by-three columns']),
				$author$project$Main$ShowSubmitForm,
				'Edit Info'),
				A3($author$project$Main$navButton, _List_Nil, $author$project$Main$ShowResults, 'Edit Plans'),
				A3($author$project$Main$navButton, _List_Nil, $author$project$Main$ShowOutput, 'Results')
			]));
};
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Main$RequestPDP = {$: 2};
var $author$project$Main$SubmitForm = {$: 1};
var $author$project$Main$SelectCounty = function (a) {
	return {$: 7, a: a};
};
var $author$project$Main$SelectDate = function (a) {
	return {$: 8, a: a};
};
var $author$project$Main$SelectGender = function (a) {
	return {$: 6, a: a};
};
var $author$project$Main$SetAge = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$SetName = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$SetZip = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$ToggleDiscounts = {$: 11};
var $author$project$Main$ToggleF = {$: 13};
var $author$project$Main$ToggleG = {$: 14};
var $author$project$Main$ToggleN = {$: 12};
var $author$project$Main$ToggleTobacco = {$: 10};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$checkbox = F4(
	function (title_, fvalue, handle, class_) {
		var cl = A2(
			$elm$core$List$map,
			function (a) {
				return $elm$html$Html$Attributes$class(a);
			},
			class_);
		return A2(
			$elm$html$Html$div,
			cl,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$label,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('checkbox'),
									$elm$html$Html$Attributes$checked(fvalue),
									$elm$html$Html$Events$onClick(handle)
								]),
							_List_Nil),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('label-body')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(title_)
								]))
						]))
				]));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$form = _VirtualDom_node('form');
var $author$project$Main$fullGenderString = function (gender) {
	if (!gender) {
		return 'Male';
	} else {
		return 'Female';
	}
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$genderselectbox = F5(
	function (title_, selectedG, handle, class_, i) {
		var cl = A2(
			$elm$core$List$map,
			function (a) {
				return $elm$html$Html$Attributes$class(a);
			},
			class_);
		var choices = _List_fromArray(
			['Male', 'Female']);
		var nls = A2(
			$elm$core$List$map,
			function (a) {
				return A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value(a),
							$elm$html$Html$Attributes$selected(
							_Utils_eq(
								a,
								$author$project$Main$fullGenderString(selectedG)))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(a)
						]));
			},
			choices);
		return A2(
			$elm$html$Html$div,
			cl,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$label,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title_),
							A2(
							$elm$html$Html$select,
							_List_fromArray(
								[
									$elm$html$Html$Events$onInput(handle),
									$elm$html$Html$Attributes$class('u-full-width')
								]),
							nls)
						]))
				]));
	});
var $elm$html$Html$h5 = _VirtualDom_node('h5');
var $elm$html$Html$Events$alwaysPreventDefault = function (msg) {
	return _Utils_Tuple2(msg, true);
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $elm$html$Html$Events$onSubmit = function (msg) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'submit',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysPreventDefault,
			$elm$json$Json$Decode$succeed(msg)));
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $author$project$Main$selectbox = F5(
	function (title_, choices, handle, class_, i) {
		var def = $elm$core$List$head(
			A2($elm$core$List$drop, i, choices));
		var nls = A2(
			$elm$core$List$map,
			function (a) {
				return A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value(a),
							$elm$html$Html$Attributes$selected(
							_Utils_eq(
								$elm$core$Maybe$Just(a),
								def))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(a)
						]));
			},
			choices);
		var cl = A2(
			$elm$core$List$map,
			function (a) {
				return $elm$html$Html$Attributes$class(a);
			},
			class_);
		return A2(
			$elm$html$Html$div,
			cl,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$label,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title_),
							A2(
							$elm$html$Html$select,
							_List_fromArray(
								[
									$elm$html$Html$Events$onInput(handle),
									$elm$html$Html$Attributes$class('u-full-width')
								]),
							nls)
						]))
				]));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $author$project$Main$textbox = F5(
	function (title_, placeholder_, fvalue, handle, classLs) {
		var cl = A2(
			$elm$core$List$map,
			function (a) {
				return $elm$html$Html$Attributes$class(a);
			},
			classLs);
		return A2(
			$elm$html$Html$div,
			cl,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$label,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title_),
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('text'),
									$elm$html$Html$Attributes$class('u-full-width'),
									$elm$html$Html$Attributes$id(
									$elm$core$String$toLower(title_)),
									$elm$html$Html$Attributes$placeholder(placeholder_),
									$elm$html$Html$Attributes$value(fvalue),
									$elm$html$Html$Events$onInput(handle)
								]),
							_List_Nil)
						]))
				]));
	});
var $author$project$Main$textboxCheck = F6(
	function (title_, placeholder_, fvalue, handle, validator, class_) {
		var cl = A2(
			$elm$core$List$map,
			function (a) {
				return $elm$html$Html$Attributes$class(a);
			},
			class_);
		var _v0 = fvalue.ac;
		if (!_v0.$) {
			var i = _v0.a;
			return A2(
				$elm$html$Html$div,
				cl,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(title_),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('text'),
										$elm$html$Html$Attributes$class('u-full-width'),
										$elm$html$Html$Attributes$placeholder(placeholder_),
										$elm$html$Html$Attributes$value(i),
										$elm$html$Html$Events$onInput(handle)
									]),
								_List_Nil),
								validator
							]))
					]));
		} else {
			return A2(
				$elm$html$Html$div,
				cl,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(title_),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('text'),
										$elm$html$Html$Attributes$class('u-full-width'),
										$elm$html$Html$Attributes$placeholder(placeholder_),
										$elm$html$Html$Attributes$value(''),
										$elm$html$Html$Events$onInput(handle)
									]),
								_List_Nil)
							]))
					]));
		}
	});
var $author$project$Main$validateVI = function (field) {
	var _v0 = field.y;
	if (_v0) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'color', 'green')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('')
				]));
	} else {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'color', 'red')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(field.cc)
				]));
	}
};
var $author$project$Main$renderForm = F3(
	function (model, func, buttonLabel) {
		var submitButton = _Utils_eq(model.l, $author$project$Main$Loading) ? A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('button'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					$elm$html$Html$Attributes$disabled(true)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Loading')
				])) : A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('button-primary'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					$elm$html$Html$Attributes$disabled(!model.y)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Submit')
				]));
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$form,
							_List_fromArray(
								[
									$elm$html$Html$Events$onSubmit(func)
								]),
							A2(
								$elm$core$List$map,
								function (a) {
									return A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('row')
											]),
										_List_fromArray(
											[a]));
								},
								_List_fromArray(
									[
										A5(
										$author$project$Main$textbox,
										'Name',
										'John Smith',
										model.T,
										$author$project$Main$SetName,
										_List_fromArray(
											['four columns', 'offset-by-four columns'])),
										A6(
										$author$project$Main$textboxCheck,
										'Age',
										'65',
										model.H,
										$author$project$Main$SetAge,
										$author$project$Main$validateVI(model.H),
										_List_fromArray(
											['two columns', 'offset-by-four columns'])),
										A6(
										$author$project$Main$textboxCheck,
										'ZIP',
										'12345',
										model.z,
										$author$project$Main$SetZip,
										$author$project$Main$validateVI(model.z),
										_List_fromArray(
											['two columns', 'offset-by-four columns'])),
										A5(
										$author$project$Main$selectbox,
										'County',
										model.ba,
										$author$project$Main$SelectCounty,
										_List_fromArray(
											['three columns', 'offset-by-four columns']),
										0),
										A5(
										$author$project$Main$genderselectbox,
										'Gender',
										model.az,
										$author$project$Main$SelectGender,
										_List_fromArray(
											['three columns', 'offset-by-four columns']),
										0),
										A5(
										$author$project$Main$selectbox,
										'Effective Date',
										A2($elm$core$List$map, $elm$core$Tuple$first, model.aR),
										$author$project$Main$SelectDate,
										_List_fromArray(
											['three columns', 'offset-by-four columns']),
										1),
										A4(
										$author$project$Main$checkbox,
										'Tobacco User?',
										model.aF,
										$author$project$Main$ToggleTobacco,
										_List_fromArray(
											['four columns', 'offset-by-four columns'])),
										A4(
										$author$project$Main$checkbox,
										'Apply Household Discount?',
										model.ae,
										$author$project$Main$ToggleDiscounts,
										_List_fromArray(
											['four columns', 'offset-by-four columns'])),
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('four columns'),
												$elm$html$Html$Attributes$class('offset-by-four columns')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$h5,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('u-full-width'),
														A2($elm$html$Html$Attributes$style, 'margin-top', '1rem')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Which Plans?')
													]))
											])),
										A4(
										$author$project$Main$checkbox,
										'Plan G',
										model.W,
										$author$project$Main$ToggleG,
										_List_fromArray(
											['four columns', 'offset-by-four columns'])),
										A4(
										$author$project$Main$checkbox,
										'Plan N',
										model.X,
										$author$project$Main$ToggleN,
										_List_fromArray(
											['four columns', 'offset-by-four columns'])),
										A4(
										$author$project$Main$checkbox,
										'Plan F',
										model.V,
										$author$project$Main$ToggleF,
										_List_fromArray(
											['four columns', 'offset-by-four columns'])),
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('four columns'),
												$elm$html$Html$Attributes$class('offset-by-four columns')
											]),
										_List_fromArray(
											[submitButton]))
									])))
						]))
				]));
	});
var $author$project$Main$F = 2;
var $author$project$Main$G = 0;
var $author$project$Main$N = 1;
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			$elm$core$List$any,
			function (c) {
				return (c !== '0') && (c !== '.');
			},
			$elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$String$cons = _String_cons;
var $elm$core$Char$fromCode = _Char_fromCode;
var $myrho$elm_round$Round$increaseNum = function (_v0) {
	var head = _v0.a;
	var tail = _v0.b;
	if (head === '9') {
		var _v1 = $elm$core$String$uncons(tail);
		if (_v1.$ === 1) {
			return '01';
		} else {
			var headtail = _v1.a;
			return A2(
				$elm$core$String$cons,
				'0',
				$myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = $elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			$elm$core$String$cons,
			$elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$String$reverse = _String_reverse;
var $myrho$elm_round$Round$splitComma = function (str) {
	var _v0 = A2($elm$core$String$split, '.', str);
	if (_v0.b) {
		if (_v0.b.b) {
			var before = _v0.a;
			var _v1 = _v0.b;
			var after = _v1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _v0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $myrho$elm_round$Round$toDecimal = function (fl) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(
			$elm$core$Basics$abs(fl)));
	if (_v0.b) {
		if (_v0.b.b) {
			var num = _v0.a;
			var _v1 = _v0.b;
			var exp = _v1.a;
			var e = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(
					A2($elm$core$String$startsWith, '+', exp) ? A2($elm$core$String$dropLeft, 1, exp) : exp));
			var _v2 = $myrho$elm_round$Round$splitComma(num);
			var before = _v2.a;
			var after = _v2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					function (_v3) {
						var a = _v3.a;
						var b = _v3.b;
						return a + ('.' + b);
					},
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$mapFirst($elm$core$String$fromChar),
						$elm$core$String$uncons(
							_Utils_ap(
								A2(
									$elm$core$String$repeat,
									$elm$core$Basics$abs(e),
									'0'),
								total))))) : A3($elm$core$String$padRight, e + 1, '0', total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _v0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var $myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if ($elm$core$Basics$isInfinite(fl) || $elm$core$Basics$isNaN(fl)) {
			return $elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _v0 = $myrho$elm_round$Round$splitComma(
				$myrho$elm_round$Round$toDecimal(
					$elm$core$Basics$abs(fl)));
			var before = _v0.a;
			var after = _v0.b;
			var r = $elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2($elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					$elm$core$String$padRight,
					r,
					'0',
					_Utils_ap(before, after)));
			var totalLen = $elm$core$String$length(normalized);
			var roundDigitIndex = A2($elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3($elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3($elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? $elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'1',
					A2(
						$elm$core$Maybe$map,
						$myrho$elm_round$Round$increaseNum,
						$elm$core$String$uncons(
							$elm$core$String$reverse(remains))))) : remains;
			var numLen = $elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					$elm$core$String$repeat,
					$elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				$elm$core$String$length(after)) < 0) ? (A3($elm$core$String$slice, 0, numLen - s, num) + ('.' + A3($elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3($elm$core$String$padRight, s, '0', after))));
			return A2($myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var $myrho$elm_round$Round$round = $myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _v0 = $elm$core$String$uncons(str);
			if (_v0.$ === 1) {
				return false;
			} else {
				if ('5' === _v0.a.a) {
					if (_v0.a.b === '') {
						var _v1 = _v0.a;
						return !signed;
					} else {
						var _v2 = _v0.a;
						return true;
					}
				} else {
					var _v3 = _v0.a;
					var _int = _v3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						$elm$core$Char$toCode(_int));
				}
			}
		}));
var $author$project$Main$currencyAddThree = F3(
	function (a, b, c) {
		return (!c) ? '$ ---.--' : ('$' + A2($myrho$elm_round$Round$round, 2, (a + b) + c));
	});
var $author$project$Main$currencyAddTwo = F2(
	function (a, b) {
		return (!b) ? '$ ---.--' : ('$' + A2($myrho$elm_round$Round$round, 2, a + b));
	});
var $elm$html$Html$hr = _VirtualDom_node('hr');
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $author$project$Main$getEnrollLink = function (tr) {
	var dd = $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(79413, 'https://www.uhcjarvis.com/content/jarvis/en/sign_in.html#/sign_in'),
				_Utils_Tuple2(12321, 'https://www.aetnaseniorproducts.com/'),
				_Utils_Tuple2(68500, 'https://www.aetnaseniorproducts.com/'),
				_Utils_Tuple2(72052, 'https://www.aetnaseniorproducts.com/'),
				_Utils_Tuple2(78700, 'https://www.aetnaseniorproducts.com/'),
				_Utils_Tuple2(47171, 'http://bluekc.com/'),
				_Utils_Tuple2(65722, 'http://agentviewcigna.com/'),
				_Utils_Tuple2(67369, 'http://agentviewcigna.com/'),
				_Utils_Tuple2(88366, 'http://agentviewcigna.com/'),
				_Utils_Tuple2(60219, 'https://www.humana.com/logon'),
				_Utils_Tuple2(70580, 'https://www.humana.com/logon'),
				_Utils_Tuple2(73288, 'https://www.humana.com/logon'),
				_Utils_Tuple2(13100, 'https://accounts.mutualofomaha.com/?r=https%3A%2F%2Fproducer.mutualofomaha.com%2Fenterprise%2Fmyportal%2Fhome%2F#login'),
				_Utils_Tuple2(69868, 'https://accounts.mutualofomaha.com/?r=https%3A%2F%2Fproducer.mutualofomaha.com%2Fenterprise%2Fmyportal%2Fhome%2F#login'),
				_Utils_Tuple2(71412, 'https://accounts.mutualofomaha.com/?r=https%3A%2F%2Fproducer.mutualofomaha.com%2Fenterprise%2Fmyportal%2Fhome%2F#login'),
				_Utils_Tuple2(72850, 'https://accounts.mutualofomaha.com/?r=https%3A%2F%2Fproducer.mutualofomaha.com%2Fenterprise%2Fmyportal%2Fhome%2F#login'),
				_Utils_Tuple2(66281, 'https://www.taagentnetinfo.com/login.aspx'),
				_Utils_Tuple2(86231, 'https://www.taagentnetinfo.com/login.aspx'),
				_Utils_Tuple2(10345, 'https://brokerportal.anthem.com/apps/ptb/login'),
				_Utils_Tuple2(28207, 'https://brokerportal.anthem.com/apps/ptb/login'),
				_Utils_Tuple2(62825, 'https://brokerportal.anthem.com/apps/ptb/login'),
				_Utils_Tuple2(95120, 'https://brokerportal.anthem.com/apps/ptb/login'),
				_Utils_Tuple2(52618, 'https://brokerportal.anthem.com/apps/ptb/login'),
				_Utils_Tuple2(71835, 'https://brokerportal.anthem.com/apps/ptb/login'),
				_Utils_Tuple2(31119, 'http://micapps.gomedico.com/'),
				_Utils_Tuple2(65641, 'http://micapps.gomedico.com/'),
				_Utils_Tuple2(79987, 'http://micapps.gomedico.com/')
			]));
	return A2($elm$core$Dict$get, tr.aA, dd);
};
var $elm$html$Html$Attributes$target = $elm$html$Html$Attributes$stringProperty('target');
var $author$project$Main$makeEnrollButton = function (tr) {
	var _v0 = $author$project$Main$getEnrollLink(tr);
	if (!_v0.$) {
		var l = _v0.a;
		return A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$href(l),
					$elm$html$Html$Attributes$target('_blank')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('button-primary')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Enroll')
						]))
				]));
	} else {
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('button-primary'),
					$elm$html$Html$Attributes$disabled(true)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Enroll')
				]));
	}
};
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Main$makeEnrollRow = function (ls) {
	var ll = A2(
		$elm$core$List$sortBy,
		function ($) {
			return $.af;
		},
		A2(
			$elm$core$List$filter,
			function ($) {
				return $.F;
			},
			ls));
	var lb = _List_fromArray(
		[
			A2(
			$elm$html$Html$td,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('')
				]))
		]);
	var eb = A2(
		$elm$core$List$map,
		function (a) {
			return A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Main$makeEnrollButton(a)
					]));
		},
		ll);
	return A2(
		$elm$html$Html$tr,
		_List_Nil,
		_Utils_ap(lb, eb));
};
var $author$project$Main$pTextUtil = function (pt) {
	switch (pt) {
		case 0:
			return 'Plan G Monthly Premium';
		case 1:
			return 'Plan N Monthly Premium';
		default:
			return 'Plan F Monthly Premium';
	}
};
var $author$project$Main$rateUtil = F2(
	function (pt, ls) {
		switch (pt) {
			case 0:
				return A2(
					$elm$core$List$map,
					function ($) {
						return $.aT;
					},
					ls);
			case 1:
				return A2(
					$elm$core$List$map,
					function ($) {
						return $.aV;
					},
					ls);
			default:
				return A2(
					$elm$core$List$map,
					function ($) {
						return $.aS;
					},
					ls);
		}
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Main$safeCurrencyFloat = function (ss) {
	if (!ss.$) {
		var s = ss.a;
		var _v1 = $elm$core$String$toFloat(
			A3($elm$core$String$replace, '$', '', s));
		if (!_v1.$) {
			var f = _v1.a;
			return f;
		} else {
			return 0.0;
		}
	} else {
		return 0.0;
	}
};
var $author$project$Main$simpleTotalRow = F3(
	function (rowname, las, l) {
		var ls = _Utils_ap(
			_List_fromArray(
				[rowname]),
			l);
		var attrss = _Utils_ap(
			las,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'background', '#d3d3d3')
				]));
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			A2(
				$elm$core$List$map,
				function (a) {
					return A2(
						$elm$html$Html$td,
						attrss,
						_List_fromArray(
							[
								$elm$html$Html$text(a)
							]));
				},
				ls));
	});
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $author$project$Main$toBodyRow = F3(
	function (rowname, attrs, l) {
		var ls = _Utils_ap(
			_List_fromArray(
				[rowname]),
			l);
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			A2(
				$elm$core$List$map,
				function (a) {
					return A2(
						$elm$html$Html$td,
						attrs,
						_List_fromArray(
							[
								$elm$html$Html$text(a)
							]));
				},
				ls));
	});
var $elm$html$Html$th = _VirtualDom_node('th');
var $author$project$Main$toHeadRow = F2(
	function (rowname, l) {
		var lsh = _List_fromArray(
			[
				A2(
				$elm$html$Html$th,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('out-th'),
						A2($elm$html$Html$Attributes$style, 'font-size', '2.0rem')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(rowname)
					]))
			]);
		var ls = A2(
			$elm$core$List$map,
			function (a) {
				return A2(
					$elm$html$Html$th,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('out-th')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(a)
						]));
			},
			l);
		var lcomb = _Utils_ap(lsh, ls);
		return A2($elm$html$Html$tr, _List_Nil, lcomb);
	});
var $author$project$Main$outputTable = F2(
	function (model, pt) {
		var _v0 = model.h;
		if (!_v0.$) {
			var tr = _v0.a;
			var vr = A2(
				$elm$core$List$sortBy,
				function (a) {
					return a.af;
				},
				A2(
					$elm$core$List$filter,
					function (a) {
						return a.F;
					},
					tr));
			var rates = A2($author$project$Main$rateUtil, pt, vr);
			var rateRow = A3(
				$author$project$Main$toBodyRow,
				$author$project$Main$pTextUtil(pt),
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('out-td')
					]),
				rates);
			var pdpString = function () {
				var _v3 = model.U;
				if (!_v3.$) {
					var pr = _v3.a;
					return $elm$core$Maybe$Just(pr.aD);
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}();
			var pdpRow = A3(
				$author$project$Main$toBodyRow,
				'Drug Plan Monthly Premium',
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('out-td')
					]),
				A2(
					$elm$core$List$map,
					function (a) {
						return $author$project$Main$safeString(pdpString);
					},
					vr));
			var pdp = function () {
				var _v2 = model.U;
				if (!_v2.$) {
					var pr = _v2.a;
					return $author$project$Main$safeCurrencyFloat(
						$elm$core$Maybe$Just(pr.aD));
				} else {
					return 0.0;
				}
			}();
			var partb = $author$project$Main$safeCurrencyFloat(model.bi);
			var partBRow = A3(
				$author$project$Main$toBodyRow,
				'Part B Monthly Premium',
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('out-td')
					]),
				A2(
					$elm$core$List$map,
					function (a) {
						return $author$project$Main$safeString(model.bi);
					},
					vr));
			var pText = function () {
				switch (pt) {
					case 0:
						return 'PLAN G';
					case 1:
						return 'PLAN N';
					default:
						return 'PLAN F';
				}
			}();
			var insuranceTotal = A2(
				$elm$core$List$map,
				function (r) {
					return A2(
						$author$project$Main$currencyAddTwo,
						pdp,
						$author$project$Main$safeCurrencyFloat(
							$elm$core$Maybe$Just(r)));
				},
				rates);
			var insuranceTotalRow = A3(
				$author$project$Main$simpleTotalRow,
				'Insurance Monthly Total',
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('out-td')
					]),
				insuranceTotal);
			var grandTotal = A2(
				$elm$core$List$map,
				function (t) {
					return A3(
						$author$project$Main$currencyAddThree,
						pdp,
						partb,
						$author$project$Main$safeCurrencyFloat(
							$elm$core$Maybe$Just(t)));
				},
				rates);
			var grandTotalRow = A3(
				$author$project$Main$simpleTotalRow,
				'Grand Monthly Total',
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('out-td')
					]),
				grandTotal);
			var enrollRow = $author$project$Main$makeEnrollRow(tr);
			var companyNames = A2(
				$author$project$Main$toHeadRow,
				pText,
				A2(
					$elm$core$List$map,
					function ($) {
						return $.af;
					},
					vr));
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('twelve columns'),
						A2($elm$html$Html$Attributes$style, 'overflow', 'hidden'),
						A2($elm$html$Html$Attributes$style, 'overflow-x', 'scroll')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$table,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id('output-table')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$thead,
								_List_Nil,
								_List_fromArray(
									[companyNames])),
								A2(
								$elm$html$Html$tbody,
								_List_Nil,
								_List_fromArray(
									[rateRow, pdpRow, insuranceTotalRow, partBRow, grandTotalRow, enrollRow]))
							])),
						A2($elm$html$Html$hr, _List_Nil, _List_Nil)
					]));
		} else {
			return $elm$html$Html$text('No Output Available');
		}
	});
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $author$project$Main$personalInfo = function (model) {
	var zipText = function () {
		var _v2 = model.z.ac;
		if (!_v2.$) {
			var v = _v2.a;
			return v;
		} else {
			return '';
		}
	}();
	var pdpText = function () {
		var _v1 = model.U;
		if (!_v1.$) {
			var pr = _v1.a;
			return $author$project$Main$pdpFullString(pr);
		} else {
			return '';
		}
	}();
	var dsc = model.ae ? 'Yes' : 'No';
	var docusignLink = 'https://account.docusign.com';
	var dentalLink = 'https://www.securitylife.com/personal-plans?agnt=010U3815';
	var ageText = function () {
		var _v0 = model.H.ac;
		if (!_v0.$) {
			var a = _v0.a;
			return a;
		} else {
			return '';
		}
	}();
	var row2 = ageText + (' yrs' + ('   |   ' + (zipText + ('   |   ' + ($author$project$Main$genderString(model.az) + ('   |   ' + ('Discount Applied: ' + dsc)))))));
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('seven columns')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('row')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$strong,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(model.T)
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('row')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(row2)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('row')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(pdpText)
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('five columns')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('row')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('button'),
												$elm$html$Html$Attributes$target('_blank'),
												$elm$html$Html$Attributes$href(dentalLink),
												A2($elm$html$Html$Attributes$style, 'width', '50%')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Dental Quote')
											])),
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('button'),
												$elm$html$Html$Attributes$target('_blank'),
												$elm$html$Html$Attributes$href(docusignLink),
												A2($elm$html$Html$Attributes$style, 'width', '50%')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Docusign')
											]))
									]))
							]))
					]))
			]));
};
var $author$project$Main$renderOutput = function (model) {
	var tl = A3(
		$elm$core$List$map2,
		$elm$core$Tuple$pair,
		_List_fromArray(
			[model.W, model.X, model.V]),
		_List_fromArray(
			[0, 1, 2]));
	var tlf = A2(
		$elm$core$List$filter,
		function (a) {
			return a.a;
		},
		tl);
	var pl = A2($elm$core$List$map, $elm$core$Tuple$second, tlf);
	var tables = A2(
		$elm$core$List$map,
		$author$project$Main$outputTable(model),
		pl);
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_Utils_ap(
			_List_fromArray(
				[
					$author$project$Main$personalInfo(model)
				]),
			tables));
};
var $author$project$Main$DeselectAll = {$: 23};
var $author$project$Main$SelectAll = function (a) {
	return {$: 24, a: a};
};
var $author$project$Main$SelectPDP = function (a) {
	return {$: 9, a: a};
};
var $author$project$Main$ToggleNonPreferred = {$: 21};
var $author$project$Main$ToggleOutside = {$: 22};
var $author$project$Main$TogglePreferred = {$: 20};
var $author$project$Main$SetTableState = function (a) {
	return {$: 19, a: a};
};
var $author$project$Main$categoryLabel = function (r) {
	switch (r) {
		case 0:
			return 'Preferred';
		case 1:
			return 'Non-Preferred';
		default:
			return 'Outside';
	}
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $billstclair$elm_sortable_table$Table$Column = $elm$core$Basics$identity;
var $billstclair$elm_sortable_table$Table$ColumnData = F3(
	function (name, viewData, sorter) {
		return {T: name, c6: sorter, dg: viewData};
	});
var $billstclair$elm_sortable_table$Table$HtmlDetails = F2(
	function (attributes, children) {
		return {aN: attributes, aP: children};
	});
var $billstclair$elm_sortable_table$Table$textDetails = function (str) {
	return A2(
		$billstclair$elm_sortable_table$Table$HtmlDetails,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $billstclair$elm_sortable_table$Table$customColumn = function (_v0) {
	var name = _v0.T;
	var viewData = _v0.dg;
	var sorter = _v0.c6;
	return A3(
		$billstclair$elm_sortable_table$Table$ColumnData,
		name,
		A2($elm$core$Basics$composeL, $billstclair$elm_sortable_table$Table$textDetails, viewData),
		sorter);
};
var $billstclair$elm_sortable_table$Table$IncOrDec = function (a) {
	return {$: 3, a: a};
};
var $billstclair$elm_sortable_table$Table$increasingOrDecreasingBy = function (toComparable) {
	return $billstclair$elm_sortable_table$Table$IncOrDec(
		$elm$core$List$sortBy(toComparable));
};
var $author$project$Main$categoryColumn = $billstclair$elm_sortable_table$Table$customColumn(
	{
		T: 'Category',
		c6: $billstclair$elm_sortable_table$Table$increasingOrDecreasingBy(
			function ($) {
				return $.cP;
			}),
		dg: A2(
			$elm$core$Basics$composeL,
			$author$project$Main$categoryLabel,
			function ($) {
				return $.a4;
			})
	});
var $billstclair$elm_sortable_table$Table$None = {$: 0};
var $billstclair$elm_sortable_table$Table$unsortable = $billstclair$elm_sortable_table$Table$None;
var $billstclair$elm_sortable_table$Table$veryCustomColumn = $elm$core$Basics$identity;
var $author$project$Main$viewCheckbox = function (_v0) {
	var selected = _v0.F;
	return A2(
		$billstclair$elm_sortable_table$Table$HtmlDetails,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('checkbox'),
						$elm$html$Html$Attributes$checked(selected)
					]),
				_List_Nil)
			]));
};
var $author$project$Main$checkboxColumn = $billstclair$elm_sortable_table$Table$veryCustomColumn(
	{T: '', c6: $billstclair$elm_sortable_table$Table$unsortable, dg: $author$project$Main$viewCheckbox});
var $billstclair$elm_sortable_table$Table$Config = $elm$core$Basics$identity;
var $billstclair$elm_sortable_table$Table$customConfig = function (_v0) {
	var toId = _v0.fb;
	var toMsg = _v0.fc;
	var columns = _v0.dV;
	var customizations = _v0.d_;
	return {
		dV: A2(
			$elm$core$List$map,
			function (_v1) {
				var cData = _v1;
				return cData;
			},
			columns),
		d_: customizations,
		fb: toId,
		fc: toMsg
	};
};
var $billstclair$elm_sortable_table$Table$simpleRowAttrs = function (_v0) {
	return _List_Nil;
};
var $elm$core$String$fromList = _String_fromList;
var $billstclair$elm_sortable_table$Table$nbsp = $elm$core$String$fromList(
	_List_fromArray(
		[
			$elm$core$Char$fromCode(160)
		]));
var $billstclair$elm_sortable_table$Table$darkGrey = function (symbol) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'color', '#555')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				_Utils_ap($billstclair$elm_sortable_table$Table$nbsp, symbol))
			]));
};
var $billstclair$elm_sortable_table$Table$lightGrey = function (symbol) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'color', '#ccc')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				_Utils_ap($billstclair$elm_sortable_table$Table$nbsp, symbol))
			]));
};
var $billstclair$elm_sortable_table$Table$simpleTheadHelp = function (_v0) {
	var name = _v0.a;
	var status = _v0.b;
	var click = _v0.c;
	var content = function () {
		switch (status.$) {
			case 0:
				return _List_fromArray(
					[
						$elm$html$Html$text(name)
					]);
			case 1:
				var selected = status.a;
				return _List_fromArray(
					[
						$elm$html$Html$text(name),
						selected ? $billstclair$elm_sortable_table$Table$darkGrey('↓') : $billstclair$elm_sortable_table$Table$lightGrey('↓')
					]);
			default:
				if (status.a.$ === 1) {
					var _v2 = status.a;
					return _List_fromArray(
						[
							$elm$html$Html$text(name),
							$billstclair$elm_sortable_table$Table$lightGrey('↕')
						]);
				} else {
					var isReversed = status.a.a;
					return _List_fromArray(
						[
							$elm$html$Html$text(name),
							$billstclair$elm_sortable_table$Table$darkGrey(
							isReversed ? '↑' : '↓')
						]);
				}
		}
	}();
	return A2(
		$elm$html$Html$th,
		_List_fromArray(
			[click]),
		content);
};
var $billstclair$elm_sortable_table$Table$simpleThead = function (headers) {
	return A2(
		$billstclair$elm_sortable_table$Table$HtmlDetails,
		_List_Nil,
		A2($elm$core$List$map, $billstclair$elm_sortable_table$Table$simpleTheadHelp, headers));
};
var $billstclair$elm_sortable_table$Table$defaultCustomizations = {br: $elm$core$Maybe$Nothing, eC: $billstclair$elm_sortable_table$Table$simpleRowAttrs, eW: _List_Nil, bV: _List_Nil, bW: $elm$core$Maybe$Nothing, bX: $billstclair$elm_sortable_table$Table$simpleThead};
var $billstclair$elm_sortable_table$Table$stringColumn = F2(
	function (name, toStr) {
		return {
			T: name,
			c6: $billstclair$elm_sortable_table$Table$increasingOrDecreasingBy(toStr),
			dg: A2($elm$core$Basics$composeL, $billstclair$elm_sortable_table$Table$textDetails, toStr)
		};
	});
var $author$project$Main$ToggleSelect = function (a) {
	return {$: 31, a: a};
};
var $author$project$Main$toRowAttrs = function (tablerow) {
	return _List_fromArray(
		[
			$elm$html$Html$Events$onClick(
			$author$project$Main$ToggleSelect(tablerow.aG)),
			A2(
			$elm$html$Html$Attributes$style,
			'background',
			tablerow.F ? '#CEFAF8' : 'white')
		]);
};
var $author$project$Main$config = $billstclair$elm_sortable_table$Table$customConfig(
	{
		dV: _List_fromArray(
			[
				$author$project$Main$checkboxColumn,
				A2(
				$billstclair$elm_sortable_table$Table$stringColumn,
				'Company',
				function ($) {
					return $.af;
				}),
				A2(
				$billstclair$elm_sortable_table$Table$stringColumn,
				'Full Name',
				function ($) {
					return $.at;
				}),
				A2(
				$billstclair$elm_sortable_table$Table$stringColumn,
				'G Rate',
				function ($) {
					return $.aT;
				}),
				A2(
				$billstclair$elm_sortable_table$Table$stringColumn,
				'N Rate',
				function ($) {
					return $.aV;
				}),
				A2(
				$billstclair$elm_sortable_table$Table$stringColumn,
				'F Rate',
				function ($) {
					return $.aS;
				}),
				$author$project$Main$categoryColumn
			]),
		d_: _Utils_update(
			$billstclair$elm_sortable_table$Table$defaultCustomizations,
			{
				eC: $author$project$Main$toRowAttrs,
				eW: _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'margin-left', 'auto'),
						A2($elm$html$Html$Attributes$style, 'margin-right', 'auto')
					])
			}),
		fb: function ($) {
			return $.at;
		},
		fc: $author$project$Main$SetTableState
	});
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$Main$pdpOption = F2(
	function (def, pr) {
		var p_text = $author$project$Main$pdpFullString(pr);
		var def_text = function () {
			if (!def.$) {
				var d = def.a;
				return $elm$core$Maybe$Just(
					$author$project$Main$pdpFullString(d));
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}();
		return A2(
			$elm$html$Html$option,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$value(p_text),
					$elm$html$Html$Attributes$selected(
					_Utils_eq(
						$elm$core$Maybe$Just(p_text),
						def_text))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(p_text)
				]));
	});
var $author$project$Main$pdpSelectBox = F3(
	function (mplist, selectedPdp, handle) {
		if (!mplist.$) {
			var plist = mplist.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('six columns'),
						$elm$html$Html$Attributes$class('offset-by-three column')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('label-body')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Prescription Dug Plan:')
									])),
								A2(
								$elm$html$Html$select,
								_List_fromArray(
									[
										$elm$html$Html$Events$onInput(handle),
										$elm$html$Html$Attributes$class('u-full-width'),
										$elm$html$Html$Attributes$id('pdp-select')
									]),
								A2(
									$elm$core$List$map,
									$author$project$Main$pdpOption(selectedPdp),
									plist))
							]))
					]));
		} else {
			return A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$select,
								_List_fromArray(
									[
										$elm$html$Html$Events$onInput(handle)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('')
									]))
							]))
					]));
		}
	});
var $elm$html$Html$caption = _VirtualDom_node('caption');
var $billstclair$elm_sortable_table$Table$applySorter = F3(
	function (isReversed, sorter, data) {
		switch (sorter.$) {
			case 0:
				return data;
			case 1:
				var srt = sorter.a;
				return srt(data);
			case 2:
				var srt = sorter.a;
				return $elm$core$List$reverse(
					srt(data));
			case 3:
				var srt = sorter.a;
				return isReversed ? $elm$core$List$reverse(
					srt(data)) : srt(data);
			default:
				var srt = sorter.a;
				return isReversed ? srt(data) : $elm$core$List$reverse(
					srt(data));
		}
	});
var $billstclair$elm_sortable_table$Table$findSorter = F2(
	function (selectedColumn, columnData) {
		findSorter:
		while (true) {
			if (!columnData.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var name = columnData.a.T;
				var sorter = columnData.a.c6;
				var remainingColumnData = columnData.b;
				if (_Utils_eq(name, selectedColumn)) {
					return $elm$core$Maybe$Just(sorter);
				} else {
					var $temp$selectedColumn = selectedColumn,
						$temp$columnData = remainingColumnData;
					selectedColumn = $temp$selectedColumn;
					columnData = $temp$columnData;
					continue findSorter;
				}
			}
		}
	});
var $billstclair$elm_sortable_table$Table$sort = F3(
	function (_v0, columnData, data) {
		var selectedColumn = _v0.a;
		var isReversed = _v0.b;
		var _v1 = A2($billstclair$elm_sortable_table$Table$findSorter, selectedColumn, columnData);
		if (_v1.$ === 1) {
			return data;
		} else {
			var sorter = _v1.a;
			return A3($billstclair$elm_sortable_table$Table$applySorter, isReversed, sorter, data);
		}
	});
var $billstclair$elm_sortable_table$Table$getSortedData = F3(
	function (_v0, state, data) {
		var toId = _v0.fb;
		var toMsg = _v0.fc;
		var columns = _v0.dV;
		var customizations = _v0.d_;
		return A3($billstclair$elm_sortable_table$Table$sort, state, columns, data);
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm$html$Html$tfoot = _VirtualDom_node('tfoot');
var $billstclair$elm_sortable_table$Table$Reversible = function (a) {
	return {$: 2, a: a};
};
var $billstclair$elm_sortable_table$Table$Sortable = function (a) {
	return {$: 1, a: a};
};
var $billstclair$elm_sortable_table$Table$Unsortable = {$: 0};
var $billstclair$elm_sortable_table$Table$onClick = F3(
	function (name, isReversed, toMsg) {
		return A2(
			$elm$html$Html$Events$on,
			'click',
			A2(
				$elm$json$Json$Decode$map,
				toMsg,
				A3(
					$elm$json$Json$Decode$map2,
					$billstclair$elm_sortable_table$Table$State,
					$elm$json$Json$Decode$succeed(name),
					$elm$json$Json$Decode$succeed(isReversed))));
	});
var $billstclair$elm_sortable_table$Table$toHeaderInfo = F3(
	function (_v0, toMsg, _v1) {
		var sortName = _v0.a;
		var isReversed = _v0.b;
		var name = _v1.T;
		var sorter = _v1.c6;
		switch (sorter.$) {
			case 0:
				return _Utils_Tuple3(
					name,
					$billstclair$elm_sortable_table$Table$Unsortable,
					A3($billstclair$elm_sortable_table$Table$onClick, sortName, isReversed, toMsg));
			case 1:
				return _Utils_Tuple3(
					name,
					$billstclair$elm_sortable_table$Table$Sortable(
						!_Utils_eq(name, sortName)),
					A3($billstclair$elm_sortable_table$Table$onClick, name, false, toMsg));
			case 2:
				return _Utils_Tuple3(
					name,
					$billstclair$elm_sortable_table$Table$Sortable(
						_Utils_eq(name, sortName)),
					A3($billstclair$elm_sortable_table$Table$onClick, name, false, toMsg));
			case 3:
				return _Utils_eq(name, sortName) ? _Utils_Tuple3(
					name,
					$billstclair$elm_sortable_table$Table$Reversible(
						$elm$core$Maybe$Just(!isReversed)),
					A3($billstclair$elm_sortable_table$Table$onClick, name, !isReversed, toMsg)) : _Utils_Tuple3(
					name,
					$billstclair$elm_sortable_table$Table$Reversible($elm$core$Maybe$Nothing),
					A3($billstclair$elm_sortable_table$Table$onClick, name, false, toMsg));
			default:
				return _Utils_eq(name, sortName) ? _Utils_Tuple3(
					name,
					$billstclair$elm_sortable_table$Table$Reversible(
						$elm$core$Maybe$Just(isReversed)),
					A3($billstclair$elm_sortable_table$Table$onClick, name, !isReversed, toMsg)) : _Utils_Tuple3(
					name,
					$billstclair$elm_sortable_table$Table$Reversible($elm$core$Maybe$Nothing),
					A3($billstclair$elm_sortable_table$Table$onClick, name, false, toMsg));
		}
	});
var $elm$virtual_dom$VirtualDom$lazy3 = _VirtualDom_lazy3;
var $elm$html$Html$Lazy$lazy3 = $elm$virtual_dom$VirtualDom$lazy3;
var $billstclair$elm_sortable_table$Table$viewCell = F2(
	function (data, _v0) {
		var viewData = _v0.dg;
		var details = viewData(data);
		return A2($elm$html$Html$td, details.aN, details.aP);
	});
var $billstclair$elm_sortable_table$Table$viewRowHelp = F3(
	function (columns, toRowAttrs, data) {
		return A2(
			$elm$html$Html$tr,
			toRowAttrs(data),
			A2(
				$elm$core$List$map,
				$billstclair$elm_sortable_table$Table$viewCell(data),
				columns));
	});
var $billstclair$elm_sortable_table$Table$viewRow = F4(
	function (toId, columns, toRowAttrs, data) {
		return _Utils_Tuple2(
			toId(data),
			A4($elm$html$Html$Lazy$lazy3, $billstclair$elm_sortable_table$Table$viewRowHelp, columns, toRowAttrs, data));
	});
var $billstclair$elm_sortable_table$Table$view = F3(
	function (conf, state, data) {
		var toId = conf.fb;
		var toMsg = conf.fc;
		var columns = conf.dV;
		var customizations = conf.d_;
		var theadDetails = customizations.bX(
			A2(
				$elm$core$List$map,
				A2($billstclair$elm_sortable_table$Table$toHeaderInfo, state, toMsg),
				columns));
		var thead = A2(
			$elm$html$Html$thead,
			theadDetails.aN,
			_List_fromArray(
				[
					A2($elm$html$Html$tr, _List_Nil, theadDetails.aP)
				]));
		var sortedData = A3($billstclair$elm_sortable_table$Table$getSortedData, conf, state, data);
		var tbody = A3(
			$elm$html$Html$Keyed$node,
			'tbody',
			customizations.bV,
			A2(
				$elm$core$List$map,
				A3($billstclair$elm_sortable_table$Table$viewRow, toId, columns, customizations.eC),
				sortedData));
		var withFoot = function () {
			var _v1 = customizations.bW;
			if (_v1.$ === 1) {
				return A2($elm$core$List$cons, tbody, _List_Nil);
			} else {
				var attributes = _v1.a.aN;
				var children = _v1.a.aP;
				return A2(
					$elm$core$List$cons,
					A2($elm$html$Html$tfoot, attributes, children),
					A2($elm$core$List$cons, tbody, _List_Nil));
			}
		}();
		return A2(
			$elm$html$Html$table,
			customizations.eW,
			function () {
				var _v0 = customizations.br;
				if (_v0.$ === 1) {
					return A2($elm$core$List$cons, thead, withFoot);
				} else {
					var attributes = _v0.a.aN;
					var children = _v0.a.aP;
					return A2(
						$elm$core$List$cons,
						A2($elm$html$Html$caption, attributes, children),
						A2($elm$core$List$cons, thead, withFoot));
				}
			}());
	});
var $author$project$Main$safeAppend = F2(
	function (a, b) {
		if (!a.$) {
			var aa = a.a;
			if (!b.$) {
				var bb = b.a;
				return $elm$core$Maybe$Just(
					A2($elm$core$List$append, aa, bb));
			} else {
				return a;
			}
		} else {
			if (!b.$) {
				var bb = b.a;
				return b;
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}
	});
var $author$project$Main$safeConcat = function (l) {
	return A3($elm$core$List$foldr, $author$project$Main$safeAppend, $elm$core$Maybe$Nothing, l);
};
var $author$project$Main$viewRows = F3(
	function (b, c, l) {
		if (b) {
			if (!l.$) {
				var ll = l.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$List$sortBy,
						function ($) {
							return $.af;
						},
						A2(
							$elm$core$List$filter,
							function (a) {
								return _Utils_eq(a.a4, c);
							},
							ll)));
			} else {
				return $elm$core$Maybe$Nothing;
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Main$viewRowsAll = function (model) {
	var showPreferred = A3($author$project$Main$viewRows, model.aK, 0, model.h);
	var showOutside = A3($author$project$Main$viewRows, model.aJ, 2, model.h);
	var showNonPreferred = A3($author$project$Main$viewRows, model.aI, 1, model.h);
	return $author$project$Main$safeConcat(
		_List_fromArray(
			[showPreferred, showNonPreferred, showOutside]));
};
var $author$project$Main$renderResults = function (model) {
	var showRows = $author$project$Main$viewRowsAll(model);
	var filtShow = function () {
		if (!showRows.$) {
			var sr = showRows.a;
			return $elm$core$Maybe$Just(
				A2(
					$elm$core$List$map,
					function (a) {
						return a.aG;
					},
					sr));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A3(
						$author$project$Main$pdpSelectBox,
						model.aX,
						model.U,
						function (a) {
							return $author$project$Main$SelectPDP(a);
						})
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('offset-by-one column')
							]),
						_List_fromArray(
							[
								A4(
								$author$project$Main$checkbox,
								'Category A',
								model.aK,
								$author$project$Main$TogglePreferred,
								_List_fromArray(
									['two columns']))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('offset-by-one column')
							]),
						_List_fromArray(
							[
								A4(
								$author$project$Main$checkbox,
								'Category B',
								model.aI,
								$author$project$Main$ToggleNonPreferred,
								_List_fromArray(
									['two columns']))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('offset-by-one column')
							]),
						_List_fromArray(
							[
								A4(
								$author$project$Main$checkbox,
								'Category C',
								model.aJ,
								$author$project$Main$ToggleOutside,
								_List_fromArray(
									['two columns']))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('six columns'),
								$elm$html$Html$Attributes$class('offset-by-one column'),
								A2($elm$html$Html$Attributes$style, 'padding-top', '1.2em')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Main$ShowOutput),
										$elm$html$Html$Attributes$class('button-primary'),
										A2($elm$html$Html$Attributes$style, 'display', 'block')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Show Output')
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('four columns'),
								A2($elm$html$Html$Attributes$style, 'padding-top', '1.2em')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick(
										$author$project$Main$SelectAll(filtShow)),
										$elm$html$Html$Attributes$class('button'),
										A2($elm$html$Html$Attributes$style, 'width', '50%')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Select All')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Main$DeselectAll),
										$elm$html$Html$Attributes$class('button'),
										A2($elm$html$Html$Attributes$style, 'width', '50%')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Clear All')
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('ten columns'),
								$elm$html$Html$Attributes$class('offset-by-one column')
							]),
						_List_fromArray(
							[
								function () {
								if (!showRows.$) {
									var sr = showRows.a;
									return A3($billstclair$elm_sortable_table$Table$view, $author$project$Main$config, model.aa, sr);
								} else {
									return A3($billstclair$elm_sortable_table$Table$view, $author$project$Main$config, model.aa, _List_Nil);
								}
							}()
							]))
					]))
			]));
};
var $author$project$Main$submitFirst = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('row')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('six columns'),
							$elm$html$Html$Attributes$class('offset-by-three columns'),
							A2($elm$html$Html$Attributes$style, 'padding', '50px 0'),
							A2($elm$html$Html$Attributes$style, 'text-align', 'center')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Please Submit Data First')
						]))
				]))
		]));
var $author$project$Main$variousViews = function (model) {
	var _v0 = model.l;
	switch (_v0.$) {
		case 0:
			var fail = _v0.a;
			switch (fail) {
				case 0:
					return A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('I could not load ZIPs for some reason. '),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(model.am)
									]))
							]));
				case 1:
					return A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('I could not load PDP for some reason. '),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Main$RequestPDP),
										A2($elm$html$Html$Attributes$style, 'display', 'block')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Resubmit PDP')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(model.am)
									]))
							]));
				default:
					return A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('I could not load Plan(s) for some reason. '),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Main$SubmitForm),
										A2($elm$html$Html$Attributes$style, 'display', 'block')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Resubmit Plan Form')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(model.am)
									]))
							]));
			}
		case 2:
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A3($author$project$Main$renderForm, model, $author$project$Main$SubmitForm, 'Submit')
					]));
		case 1:
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A3($author$project$Main$renderForm, model, $author$project$Main$SubmitForm, 'Submit')
					]));
		case 3:
			return model.aC ? A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Main$renderResults(model)
					])) : A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[$author$project$Main$submitFirst]));
		default:
			return model.aC ? A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Main$renderOutput(model)
					])) : A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[$author$project$Main$submitFirst]));
	}
};
var $author$project$Main$view = function (model) {
	return {
		dI: _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('container')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('row')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$a,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$href('/')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$img,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$src('images/logo.png'),
												A2($elm$html$Html$Attributes$style, 'max-width', '400px'),
												A2($elm$html$Html$Attributes$style, 'height', 'auto'),
												A2($elm$html$Html$Attributes$style, 'margin', 'auto'),
												A2($elm$html$Html$Attributes$style, 'display', 'block')
											]),
										_List_Nil)
									]))
							])),
						$author$project$Main$navBar(model),
						$author$project$Main$variousViews(model)
					]))
			]),
		fa: 'Medicare School Quote'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{ef: $author$project$Main$init, es: $author$project$Main$UrlChanged, et: $author$project$Main$LinkClicked, eV: $author$project$Main$subscriptions, fg: $author$project$Main$update, fh: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));
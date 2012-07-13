/*******************************************************************************
 *
 * rawe-prim.js: Implementation of primitive operators
 * as part of rawe - ReActive Web Framework
 *
 * Copyright 2011, Roman Smrž <roman.smrz@seznam.cz>
 *
 *
 * In this file all the primitives used to created behaviour functions and are
 * assigned to the rawe.prim namespace.
 *
 * Parameters are passed either during initialization to the initialization
 * function itself (in cases of functions like compose or product), or during
 * evaluation to the assigned compute function. In the latter case, they are
 * passed in a single formal parameter arranged into tuples: first parameter
 * is param.get()[0], second param.get()[1].get()[0] and so on.
 *
 */


var prim = rawe.prim;


/******************************************************************************/
/*	Basic primitives
/******************************************************************************/


/******************************************************************************/
// 	Category


/* Simple identity function */

prim.id = function() {
	this.compute = function(x) { return x; }
}

/* Composition of functions */

prim.compose = function() {
	var args = arguments;
	this.compute = function(x) {
		for (var i = 0; i < args.length; i++)
			x = args[i].get().compute(x);
		return x;
 	};
};
 



/******************************************************************************/
// 	PreCartesian


/* Cartesian product - parameter is passed to two functions and a tuple is
 * formed from their results */

prim.product = function(f, g) {
	this.compute = function(x) { return new rawe.Thunk(function() {
		return [f.get().compute(x), g.get().compute(x)];
	}); };
}

/* Getting first and second component of a tuple */

prim.fst = function() {
	this.compute_t = function(x, t) { return x.get()[0].get(t); };
}

prim.snd = function() {
	this.compute_t = function(x, t) { return x.get()[1].get(t); };
}



/******************************************************************************/
// 	Cartesian Closed


/* Application - we get a pair of function and parameter and return the result */

prim.apply = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var func = params.get()[0].get();
		var value = params.get()[1];
		return func(value).get(this);
	}); };
}

/* Currying and uncurrying primitives */

prim.curry = function(f) {
	this.compute = function(x) { return new rawe.Thunk(function() {
		return function(y) { return f.get().compute(rawe.cthunk([x,y])); }
	}); };
}

prim.uncurry = function(f) {
	this.compute = function(xy) { return new rawe.Thunk(function() {
		return f.get().compute(xy.get()[0]).get()(xy.get()[1]).get(this);
	}); };
}


/******************************************************************************/
// 	Other functions

/* Constant behaviour function */

prim.cb = function(value) {
	this.compute = function() { return value; };
};

prim.cbf = function(f) {
	this.compute = function() {
		var bhv = this;

		// We need to pass all the parameters as behaviours, while we
		// get them as ordinary values.
		var app = function(cf) { return rawe.cthunk(function(x) {
			var bx = new rawe.BhvFun();
			rawe.prim.cb.call(bx, x);
			var y = cf(rawe.cthunk(bx)).get();

			// still taking another argument
			if (typeof y == 'function')
				return app(y);

			// final result
			return y.compute();
		}); };

		return app(f.get());
	};
};

/* Joining two levels of behaviours */

prim.bjoin = function(out) {
	this.compute = function(x) {
		var inner = out.get().compute().get();
		var res = inner.compute(x);
		res.add_depend(out.get());
		return res;
	}
}

/* Wrapping and unwrapping with the Bhv constructor */

prim.bhv_wrap = function() {
	var bhv = this;
	this.compute = function(x) { return new rawe.Thunk(function() {
		var res = new rawe.BhvFun();
		prim.cb.call(res, x);
		return res;
	}); };
}

prim.bhv_unwrap = function() {
	var bhv = this;

	this.compute = function(x) { return new rawe.Thunk(function() {
		this.add_depend(x.get());
		return x.get().compute().get(this);
	}); };
}

/* Fixpoint operator – create a thunk and pass it into i given function and let
 * the lazy evaluation work */

prim.fix = function() {
	this.compute = function(f) {
		var thunk;
		thunk = new rawe.Thunk(function() {
			return f.get()(thunk).get(thunk);
		});
		return thunk;
	}
}



/******************************************************************************/
/*	HTML functions
/******************************************************************************/


/******************************************************************************/
//	Manipulation with HTML

/* Adding attribute (from the second parameter) to the HTML snippet (in the
 * first parameter) */

prim.add_attr = function() {
	this.compute_ = function(params, thunk) {
		var name, value;
		var attr = params[1].get(thunk);
		if (attr.AttrVal) {
			name = attr.AttrVal[0];
			value = attr.AttrVal[1];
		}
		if (attr.AttrBool) {
			name = attr.AttrBool[0];
			value = true;
		}
		return params[0].get(thunk).attr(name, value);
	}
}

/* Append two HTML snippets */

prim.append_html = function() {
	this.compute_ = function(params, t) {
		return params[0].get(t).add(params[1].get(t));
	};
}

/* Until works similarly to '\x -> maybe x id', but keeps the inner value of
 * the x :: (Bhv HtmlM a) parameter even when the second one becomes Just */

prim.html_until = function(def, mb) {
	this.compute = function() { return new rawe.Thunk(function() {
		var x = mb.get().compute();
		var y = def.get().compute();

		if (typeof(x.get(this).Just) != 'undefined') {
			var xj = x.get().Just.get(this);

			// we need to copy the property representing the inner
			// value:
			xj.prop('rawe_html_inner', y.get().prop('rawe_html_inner'));
			return xj;
		}
		// Nothing -> just return the default
		return y.get(this);
	}); };

	// This is needed for mutually dependent values to work
	this.html_inner = function() {
		return def.get().html_inner();
	}
}

/* Gets the inner value of HtmlM a - just reads the relevant property using
 * html_inner() method */

prim.bhv_to_html_inner = function(out) {
	this.compute = function(x) { return new rawe.Thunk(function() {
		this.add_depend(out.get());
		var inner = out.get().html_inner().get();
		return inner.compute(x).get(this);
	}); };
}


/******************************************************************************/
//	Generating elements


/* General template for generating primitives */

prim.gen = function() {
	this.compute = function() {
		return this.value;
	};

	this.change = function(value) {
		value.add_depend(this);
		this.value = value;
		rawe.current_time++;
		this.invalidate();
	};

	this.gen.prop('rawe_bhv_gen', this);
}

/* Textfild - monitor changes and keyups */

prim.gen_input_text = function() {
	prim.gen.call(this);

	var elem = this.gen;
	var b = this;

	this.value = rawe.cthunk(elem.val());
	this.value.add_depend(this);

	elem.change(function(e) {
		b.change(rawe.cthunk(elem.val()));
	});
	elem.keyup(function(e) {
		b.change(rawe.cthunk(elem.val()));
	});

	elem.removeAttr('bhv-gen');
}

/* Buttons generate events when clicked */

prim.gen_input_button = function() {
	prim.gen.call(this);

	var b = this;
	var elem = this.gen;

	b.value = rawe.cthunk({ NotYet: [] });
	b.value.add_depend(this);
	elem.click(function(e) {
		e.preventDefault();
		b.change(rawe.cthunk({ OnTime: [rawe.cthunk(++rawe.current_time), rawe.cthunk([])] }));
	});
}

prim.gen_input_submit = function() {
	prim.gen.call(this);

	var b = this;
	var elem = this.gen;

	b.value = rawe.cthunk({ NotYet: [] });
	b.value.add_depend(this);
	elem.click(function(e) {
		b.change(rawe.cthunk({ OnTime: [rawe.cthunk(++rawe.current_time), rawe.cthunk(elem.val())] }));
	});
}

prim.gen_ae = prim.gen_input_button;

/* Form generates an event containing all the information from it when sent */

prim.gen_form = function() {
	prim.gen.call(this);
	var b = this;
	var elem = this.gen;

	b.value = rawe.cthunk({ NotYet: null });
	b.value.add_depend(this);

	elem.submit(function(e) {
		e.preventDefault();
		var result = {};
		elem.find('input, select, textarea').each(function() {
			result[$(this).attr('name')] = rawe.cthunk( $(this).val() );
		});
		b.change(rawe.cthunk({ OnTime: [rawe.cthunk(++rawe.current_time), rawe.cthunk(result)] }));
	});
}


/******************************************************************************/
//	Communication with server

/* Request a value form the server using GET method */

prim.sget = function(name) {
	name = name.get();

	var b = this;
	this.compute_t = function(_, thunk) {
		thunk.add_depend(this);

		for (i in prim.sget.values) {
			if (prim.sget.values[i][0] == name) {
				return { Just: rawe.cthunk( prim.sget.values[i][1] ) };
			}
		}

		$.get(document.location.href, { q: name }, function(json) {
			prim.sget.values.push([name, jQuery.parseJSON(json)]);
			rawe.current_time++;
			b.invalidate();
		});

		return { Nothing: null };
	};
}

// already received values are registered here
prim.sget.values = [];


/* On an event, send data to the server using POST method and process the
 * answer when it is received */

prim.post = function(name, signal) {
	var result = rawe.cthunk( { NotYet: null } );
	result.add_depend(this);

	var last_result = -1;
	var bhv = this;

	var parent_invalidate = this.invalidate;
	this.invalidate = function() {
		this.clear_depend();
		var x = signal.get().compute().get(this);

		// Only react if same event happened and it is a later time than
		// we already dealt with

		if (typeof x.OnTime == 'undefined' || x.OnTime[0].get() <= this.last_change)
			return;

		var query_time = x.OnTime[0].get(this);

		var obj = x.OnTime[1].get();
		var post = {};
		for (i in obj) post[i] = obj[i].get();

		$.post(document.location.href+'?q='+name.get(), post, function(json) {
			// if we already got an answer from a later request, we
			// just ignore this one
			if (last_result > query_time)
				return;

			var y = $.parseJSON(json);
			result = rawe.cthunk( { Just: rawe.cthunk(y) } );
			result.add_depend(bhv);
			rawe.current_time++;

			parent_invalidate.call(bhv);
			this.valid = true;
		});
	};

	/* To force initialization */
	this.init = this.invalidate;

	this.compute = function() { return result; };
}


/******************************************************************************/
//	Converting to HTML

prim.to_html_int = function() {
	this.compute_t = function(x,t) {
		return $('<span>'+x.get(t)+'</span>');
	};
}

prim.to_html_jsstring = function() {
	this.compute_t = function(x,t) {
		return $('<span>'+x.get(t)
				.replace(/&/g,"&amp;")
				.replace(/</g,"&lt;")
				.replace(/>/g,"&gt;")
				+'</span>');
	};
}


/******************************************************************************/
/*	JavaScript operators
/******************************************************************************/


/******************************************************************************/
//	Comparing

prim.cmp_eq = function() {
	this.compute_ = function(params, t) {
		return params[0].get(t) == params[1].get(t);
	};
}

prim.cmp_ne = function() {
	this.compute_ = function(params, t) {
		return params[0].get(t) != params[1].get(t);
	};
}

prim.cmp_lt = function() {
	this.compute_ = function(params, t) {
		return params[0].get(t) < params[1].get(t);
	};
}

prim.cmp_le = function() {
	this.compute_ = function(params, t) {
		return params[0].get(t) <= params[1].get(t);
	};
}

prim.cmp_gt = function() {
	this.compute_ = function(params, t) {
		return params[0].get(t) > params[1].get(t);
	};
}

prim.cmp_ge = function() {
	this.compute_ = function(params, t) {
		return params[0].get(t) >= params[1].get(t);
	};
}


/******************************************************************************/
//	Arithmetic

prim.arit_plus = function() {
	this.compute_ = function(x,t) { return x[0].get(t) + x[1].get(t); };
}

prim.arit_minus = function() {
	this.compute_ = function(x,t) { return x[0].get(t) - x[1].get(t); };
}

prim.arit_times = function() {
	this.compute_ = function(x,t) { return x[0].get(t) * x[1].get(t); };
}

prim.arit_div = function() {
	this.compute_ = function(x,t) { return x[0].get(t) / x[1].get(t); };
}

prim.arit_idiv = function() {
	this.compute_ = function(x,t) { return Math.floor(x[0].get(t) / x[1].get(t)); };
}

prim.arit_neg = function() {
	this.compute_t = function(x,t) { return -x.get(t); };
}

prim.arit_abs = function() {
	this.compute_ = function(x,t) { return Math.abs(x.get(t)); };
}


/******************************************************************************/
//	Other

prim.js_typeof = function() {
	this.compute_t = function(x,t) { return typeof x.get(t); }
}


prim.debug = function() {
	this.compute_ = function(x) {
		alert(x[0].get() + ': ' + x[1].get());
		return x[1].get();
	};
}

prim.error = function() {
	this.compute_ = function(msg) { alert(msg); };
}



/******************************************************************************/
/*	Constructors and destructors and related functions
/******************************************************************************/

/******************************************************************************/
//	JavaScript strings and objects

/* We need to work with ADT representation of linked lists were, hence the
 * traversing in the while loops */

prim.to_js_string = function() {
	this.compute = function(cur) { return new rawe.Thunk(function() {
		var result = '';
		while (typeof cur.get(this).cons != 'undefined') {
			result += cur.get().cons[0].get(this);
			cur = cur.get().cons[1];
		}
		return result;
	}); };
}

prim.from_js_string = function() {
	var bhv = this;
	this.compute = function(strt) { return new rawe.Thunk(function() {
		str = strt.get(this);
		var end = { nil: [] };
		var result = end;

		for (i in str) {
			newend = { nil: [] };
			delete end.nil;
			end.cons = [ rawe.cthunk(str[i]), rawe.cthunk(newend) ];
			end.cons[0].merge_depend(strt);
			end.cons[1].merge_depend(strt);
			end = newend;
		}
		return result;
	}); };
}

prim.to_js_object = function() {
	this.compute = function(cur) { return new rawe.Thunk(function() {
		var result = {};
		while (typeof cur.get(this).cons != 'undefined') {
			result[cur.get().cons[0].get()[0].get(this)] =
				cur.get().cons[0].get()[1];
			cur = cur.get().cons[1];
		}
		return result;
	}); };
}

prim.from_js_object = function() {
	this.compute = function(objt) { return new rawe.Thunk(function() {
		obj = objt.get(this);
		var end = { nil: [] };
		var result = end;

		for (i in obj) {
			newend = { nil: [] };
			delete end.nil;
			end.cons = [
				rawe.cthunk([rawe.cthunk(i).merge_depend(objt), obj[i]]),
				rawe.cthunk(newend).merge_depend(objt)
			];
			end = newend;
		}
		return result;
	}); };
}

prim.js_object_fmap = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var f = params.get()[0].get();
		var obj = params.get()[1].get(this);

		var result = {};
		for (i in obj) result[i] = f(obj[i]);
		return result;
	}); };
}


/******************************************************************************/
//	Bool

prim.btrue = function() {
	this.compute = function() { return rawe.cthunk(true); };
}

prim.bfalse = function() {
	this.compute = function() { return rawe.cthunk(false); };
}

prim.bool = function() {
	this.compute_t = function(params, thunk) {
		var t = params.get()[0];
		var f = params.get()[1].get()[0];
		var c = params.get()[1].get()[1];
		if (c.get(thunk)) return t.get(thunk);
		return f.get(thunk);
	};
}

/******************************************************************************/
//	Ordering

prim.lt = function() {
	this.compute = function() { return rawe.cthunk(-1); };
}

prim.eq = function() {
	this.compute = function() { return rawe.cthunk(0); };
}

prim.gt = function() {
	this.compute = function() { return rawe.cthunk(1); };
}

prim.ordering = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var value = params.get()[1].get()[1].get()[1].get(this);
		switch (value) {
			case -1: return params.get()[0].get(this);
			case  0: return params.get()[1].get()[0].get(this);
			case  1: return params.get()[1].get()[1].get()[0].get(this);
		}
	}); };
}


/******************************************************************************/
//	List

prim.nil = function() {
	this.compute = function() { return rawe.cthunk( { 'nil': [] }); };
}

prim.cons = function() {
	this.compute = function(x) { return rawe.cthunk( { 'cons': x.get() }); };
}

prim.list = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var b1 = params.get()[0];
		var b2 = params.get()[1].get()[0];
		var x = params.get()[1].get()[1].get(this);

		if (typeof x.nil != 'undefined')
			var res = b1.get(this);
		if (typeof x.cons != 'undefined')
			var res = b2.get()(x.cons[0]).get()(x.cons[1]).get(this);
		return res;
	}); };
}


/******************************************************************************/
//	Maybe

prim.nothing = function() {
	this.compute = function() { return rawe.cthunk({ Nothing: null }); };
}

prim.just = function() {
	this.compute = function(x) { return rawe.cthunk( { Just: x } ); };
}

prim.maybe = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var def = params.get()[0];
		var func = params.get()[1].get()[0];
		var mb = params.get()[1].get()[1].get(this);

		if (typeof mb.Just == 'undefined')
			return def.get(this)
		else return func.get()(mb.Just).get(this);
	}); };
}


/******************************************************************************/
//	Either

prim.left = function() {
	this.compute = function(x) { return rawe.cthunk({ Left: x }); };
};

prim.right = function() {
	this.compute = function(x) { return rawe.cthunk({ Right: x }); };
};

prim.either = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var value = params.get()[1].get()[1].get(this);
		var res;
		for (constr in value) {
			switch (constr) {
				case 'Left': res = params.get()[0]; break;
				case 'Right': res = params.get()[1].get()[0]; break;
			}

			if (res) {
				res = res.get()(value[constr]);
				return res.get(this);
			}
		}
	}); };
};


/******************************************************************************/
//	Timed

prim.not_yet = function() {
	this.compute = function() { return rawe.cthunk({ NotYet: [] }); };
}

prim.on_time = function() {
	this.compute = function(x) { return new rawe.Thunk(function() {
		return { OnTime: x.get() }
	}); };
}

prim.timed = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var value = params.get()[1].get()[1].get(this);
		var res;
		for (constr in value) {
			switch (constr) {
				case 'NotYet': res = params.get()[0]; break;
				case 'OnTime': res = params.get()[1].get()[0]; break;
			}

			if (res) {
				for (i in value[constr])
					res = res.get()(value[constr][i]);
				return res.get(this);
			}
		}
	}); };
}

prim.timed_fold = function(step, def, ev) {
	var value = def.get().compute();
	value.add_depend(this);

	this.compute = function() { return value; };

	var parent_invalidate = this.invalidate;
	this.invalidate = function() {
		this.clear_depend();
		var timed = ev.get().compute().get(this);

		if (timed.OnTime)
			value = step.get().compute().get()(timed.OnTime[0]).get()(value).get()(timed.OnTime[1]);

		value = rawe.cthunk(value.get());
		value.add_depend(this);

		parent_invalidate.call(this);
		this.valid = true;
	};
	this.init = this.invalidate;
}


/******************************************************************************/
//	Result

prim.result_error = function() {
	this.compute = function(x) { return rawe.cthunk({ Error: [x] }); };
}

prim.result_ok = function() {
	this.compute = function(x) { return rawe.cthunk({ Ok: [x] }); };
}

prim.result = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var value = params.get()[1].get()[1].get(this);
		for (i in value) {
			var func;
			switch (i) {
				case 'Ok': func = params.get()[0]; break;
				case 'Error': func = params.get()[1].get()[0]; break;
			}
			return func.get()(value[i][0]).get(this);
		}
	}); };
}

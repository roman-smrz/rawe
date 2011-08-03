/*******************************************************************************
 *
 * reactive-prim.js: Implementation of primitive operators
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

prim.compose = function(f, g) {
	this.add_depend(f.get());
	this.add_depend(g.get());

	this.compute = function(x) {
		var y = f.get().compute(x);
		return g.get().compute(y);
	};
}



/******************************************************************************/
// 	PreCartesian


/* Cartesian product - parameter is passed to two functions and a tuple is
 * formed from their results */

prim.product = function(f, g) {
	this.add_depend(f.get());
	this.add_depend(g.get());
	this.compute = function(x) { return new rawe.Thunk(function() {
		return [f.get().compute(x), g.get().compute(x)];
        }); };
}

/* Gettings first and second component of a tuple */

prim.fst = function() {
	this.compute_unbox = function(x) { return x.get()[0]; }
}

prim.snd = function() {
	this.compute_unbox = function(x) { return x.get()[1]; }
}



/******************************************************************************/
// 	Cartesian Closed


/* Application - we get a pair of function and parameter and return the result */

prim.apply = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var func = params.get()[0].get();
		var value = params.get()[1];
		var res = func(value);
		return res.get();
	}); };
}

/* Currying and uncurrying primitives */

prim.curry = function(f) {
	this.add_depend(f.get());
	this.compute = function(x) { return new rawe.Thunk(function() {
		return function(y) { return f.get().compute(rawe.cthunk([x,y])); }
	}); };
}

prim.uncurry = function(f) {
	this.add_depend(f.get());
	this.compute = function(xy) { return new rawe.Thunk(function() {
		return f.get().compute(xy.get()[0]).get()(xy.get()[1]).get();
	}); };
}


/******************************************************************************/
// 	Other functions

/* Constant behaviour function */

prim.const = function(value) {
	this.compute = function() {
		if (value.get().constructor.name == 'BhvFun')
			this.add_depend(value.get());
		return value;
	};
}

/* Joining two levels of behaviours */

prim.bjoin = function(out) {
	this.compute = function(x) {
		return out.get().compute().get().compute(x);
	}
}

/* Wrapping and unwrapping with the Bhv constructor */

prim.bhv_wrap = function() {
	var bhv = this;
	this.compute = function(x) { return new rawe.Thunk(function() {
		var res = new rawe.BhvFun();
		prim.const.call(res, x);
		res.add_depend(bhv);
		return res;
	}); };
}

prim.bhv_unwrap = function() {
	this.compute = function(x) { return new rawe.Thunk(function() {
		return x.get().compute().get();
	}); };
}

/* Fixpoint operator – create a thunk and pass it into i given function and let
 * the lazy evaluation work */

prim.fix = function() {
	this.compute = function(f) {
                var thunk;
                thunk = new rawe.Thunk(function() {
			return f.get()(thunk).get();
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
        this.compute_ = function(params) {
                var name, value;
                var attr = params[1].get();
                if (attr.AttrVal) {
                        name = attr.AttrVal[0];
                        value = attr.AttrVal[1];
                }
                if (attr.AttrBool) {
                        name = attr.AttrBool[0];
                        value = true;
                }
                return params[0].get().attr(name, value);
        }
}

/* Append to HTML snippets */

prim.append_html = function() {
	this.compute_ = function(params) {
		return params[0].get().add(params[1].get());
	};
}

/* Until works similarily to '\x -> maybe x id', but keeps the inner value of
 * the x :: (Bhv HtmlM a) parameter even when the second one becomes Just */

prim.html_until = function(def, mb) {
	this.add_depend(def.get());
	this.add_depend(mb.get());

	this.compute = function() { return new rawe.Thunk(function() {
		var x = mb.get().compute().get();
		var y = def.get().compute().get();

		if (typeof(x.Just) != 'undefined') {
			xj = x.Just.get();

			// we need to copy the property representing the inner
			// value:
			xj.prop('rawe_html_inner', y.prop('rawe_html_inner'));
			return xj;
		}
		// Nothing -> just return the default
		return y;
	}); };

	// This is needed for mutually dependent values to work
	this.html_inner = function() {
		return def.get().html_inner();
	}
}

/* Gets the inner value of HtmlM a - just reads the relevant property using
 * html_inner() method */

prim.bhv_to_html_inner = function(out) {
	var b = this;
	this.add_depend(out.get());

	this.compute = function(x) { return new rawe.Thunk(function() {
		var inner = out.get().html_inner().get();
		// TODO: clear old dependency
		b.add_depend(inner);
		return inner.compute(x).get();
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
	elem.click(function(e) {
		b.change(rawe.cthunk({ OnTime: [rawe.cthunk(++rawe.current_time), rawe.cthunk([])] }));
	});
}

prim.gen_input_submit = function() {
	prim.gen.call(this);

	var b = this;
	var elem = this.gen;

	b.value = rawe.cthunk({ NotYet: [] });
	elem.click(function(e) {
		b.change(rawe.cthunk({ OnTime: [rawe.cthunk(++rawe.current_time), rawe.cthunk(elem.val())] }));
	});
}

/* Form generates an event containing all the information from it when sent */

prim.gen_form = function() {
	prim.gen.call(this);
	var b = this;
	var elem = this.gen;

	if (typeof b.value == 'undefined')
		b.value = rawe.cthunk({ NotYet: null });

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
        this.compute_t = function() {
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
	var last_result = -1;
	var b = this;

	this.add_depend(signal.get());

	this.invalidate = function() {
		var x = signal.get().compute().get();

		// Only react if same event happend and it is a later time than
		// we already dealt with

		if (typeof x.OnTime == 'undefined' || x.OnTime[0].get() <= this.last_change)
			return;

		this.last_change = x.OnTime[0].get();

		var obj = x.OnTime[1].get();
		var post = {};
		for (i in obj) post[i] = obj[i].get();

		$.post(document.location.href+'?q='+name.get(), post, function(json) {
			// if we already got an answer from a later request, we
			// just ignore this one
			if (last_result > x.OnTime[0].get())
				return;

			var y = $.parseJSON(json);
			result = rawe.cthunk( { Just: rawe.cthunk(y) } );
			rawe.current_time++;
			for (i in b.rdepend)
				b.rdepend[i].invalidate();
		});
	};

	/* To force initialization */
	this.init = this.invalidate;

	this.compute = function() { return result; };
}


/******************************************************************************/
//	Converting to HTML

prim.to_html_int = function() {
        this.compute_ = function(x) {
                return $('<span>'+x+'</span>');
        };
}

prim.to_html_jsstring = function() {
	this.compute_ = function(x) {
		// TODO: escape the string
		return $('<span>'+x+'</span>');
	};
}


/******************************************************************************/
/*	JavaScript operators
/******************************************************************************/


/******************************************************************************/
//	Comparing

prim.cmp_eq = function() {
        this.compute_ = function(params) {
                return params[0].get() == params[1].get();
        };
}

prim.cmp_ne = function() {
        this.compute_ = function(params) {
                return params[0].get() != params[1].get();
        };
}

prim.cmp_lt = function() {
        this.compute_ = function(params) {
                return params[0].get() < params[1].get();
        };
}

prim.cmp_le = function() {
        this.compute_ = function(params) {
                return params[0].get() <= params[1].get();
        };
}

prim.cmp_gt = function() {
        this.compute_ = function(params) {
                return params[0].get() > params[1].get();
        };
}

prim.cmp_ge = function() {
        this.compute_ = function(params) {
                return params[0].get() >= params[1].get();
        };
}


/******************************************************************************/
//	Arithmetic

prim.arit_plus = function() {
        this.compute_ = function(x) { return x[0].get() + x[1].get(); };
}

prim.arit_minus = function() {
        this.compute_ = function(x) { return x[0].get() - x[1].get(); };
}

prim.arit_times = function() {
        this.compute_ = function(x) { return x[0].get() * x[1].get(); };
}

prim.arit_div = function() {
        this.compute_ = function(x) { return x[0].get() / x[1].get(); };
}

prim.arit_idiv = function() {
        this.compute_ = function(x) { return Math.floor(x[0].get() / x[1].get()); };
}

prim.arit_neg = function() {
        this.compute_ = function(x) { return -x; };
}

prim.arit_abs = function() {
        this.compute_ = function(x) { return Math.abs(x); };
}


/******************************************************************************/
//	Other

prim.typeof = function() {
	this.compute_ = function(x) { return typeof x; }
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

/* We need to work with ADT representation of linked listes were, hence the
 * traversing in the while loops */

prim.to_js_string = function() {
	this.compute = function(cur) { return new rawe.Thunk(function() {
		var result = '';
		while (typeof cur.get().cons != 'undefined') {
			result += cur.get().cons[0].get();
			cur = cur.get().cons[1];
		}
		return result;
	}); };
}

prim.from_js_string = function() {
	this.compute = function(str) { return new rawe.Thunk(function() {
		str = str.get();
		var end = { nil: [] };
		var result = end;

		for (i in str) {
			newend = { nil: [] };
			delete end.nil;
			end.cons = [ rawe.cthunk(str[i]), rawe.cthunk(newend) ];
			end = newend;
		}
		return result;
	}); };
}

prim.to_js_object = function() {
	this.compute = function(cur) { return new rawe.Thunk(function() {
		var result = {};
		while (typeof cur.get().cons != 'undefined') {
			result[cur.get().cons[0].get()[0].get()] =
				cur.get().cons[0].get()[1];
			cur = cur.get().cons[1];
		}
		return result;
	}); };
}

prim.from_js_object = function() {
	this.compute = function(obj) { return new rawe.Thunk(function() {
		obj = obj.get();
		var end = { nil: [] };
		var result = end;

		for (i in obj) {
			newend = { nil: [] };
			delete end.nil;
			end.cons = [ rawe.cthunk([rawe.cthunk(i), obj[i]]), rawe.cthunk(newend) ];
			end = newend;
		}
		return result;
	}); };
}

prim.js_object_fmap = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var f = params.get()[0].get();
		var obj = params.get()[1].get();

		var result = {};
		for (i in obj) result[i] = f(obj[i]);
		return result;
	}); };
}


/******************************************************************************/
//	Bool

prim.true = function() {
        this.compute = function() { return rawe.cthunk(true); };
}

prim.false = function() {
        this.compute = function() { return rawe.cthunk(false); };
}

prim.bool = function() {
	this.compute_unbox = function(params) {
                var t = params.get()[0];
                var f = params.get()[1].get()[0];
                var c = params.get()[1].get()[1];
		if (c.get()) return t;
		return f;
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
		var value = params.get()[1].get()[1].get()[1].get();
		switch (value) {
			case -1: return params.get()[0].get();
			case  0: return params.get()[1].get()[0].get();
			case  1: return params.get()[1].get()[1].get()[0].get();
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
		var x = params.get()[1].get()[1].get();

		if (typeof x.nil != 'undefined')
			return b1.get();
		if (typeof x.cons != 'undefined')
			return b2.get()(x.cons[0]).get()(x.cons[1]).get();
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
                var mb = params.get()[1].get()[1];

                if (typeof mb.get().Just == 'undefined')
			return def.get();
		return func.get()(mb.get().Just).get();
	}); };
}


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
                var not_yet = params.get()[0];
                var on_time = params.get()[1].get()[0];
                var value = params.get()[1].get()[1];

		if (typeof value.get().OnTime == 'undefined')
			return not_yet.get();

		var ot = value.get().OnTime;
		return on_time.get()(ot[0]).get()(ot[1]).get();
	}); };
}

prim.timed_fold = function(step, def, ev) {
	var value = def.get().compute();
	var b = this;
	var last_recomp = 0;

	step = step.get();
	ev = ev.get();
	this.add_depend(ev);

	this.compute = function() { return new rawe.Thunk(function() {
		if (!b.valid) {
			var timed = ev.compute().get();

			if (timed.OnTime && timed.OnTime[0].get() > last_recomp) {
				value = step.compute().get()(timed.OnTime[0]).get()(value).get()(timed.OnTime[1]);
				last_recomp = timed.OnTime[0].get();
			}

			b.valid = true;
		}
		return value.get();
	}); };
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
		var value = params.get()[1].get()[1].get();
		for (i in value) {
			var func;
			switch (i) {
				case 'Ok': func = params.get()[0]; break;
				case 'Error': func = params.get()[1].get()[0]; break;
			}
			return func.get()(value[i][0]).get();
		}
	}); };
}

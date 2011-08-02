
function r_prim_id() {
	this.compute = function(x) { return x; }
}

function r_prim_apply() {
	this.compute = function(params) { return new Thunk(function() {
		var func = params.get()[0].get();
		var value = params.get()[1];
		var res = func(value);
		return res.get();
	}); };
}

function r_prim_curry(f) {
	this.add_depend(f.get());
	this.compute = function(x) { return new Thunk(function() {
		return function(y) { return f.get().compute(cthunk([x,y])); }
	}); };
}

function r_prim_uncurry(f) {
	this.add_depend(f.get());
	this.compute = function(xy) { return new Thunk(function() {
		return f.get().compute(xy.get()[0]).get()(xy.get()[1]).get();
	}); };
}

function r_prim_bhv_wrap() {
	var bhv = this;
	this.compute = function(x) { return new Thunk(function() {
		var res = new BhvFun();
		r_prim_const.call(res, x);
		res.add_depend(bhv);
		return res;
	}); };
}

function r_prim_bhv_unwrap() {
	this.compute = function(x) { return new Thunk(function() {
		return x.get().compute(cthunk({})).get();
	}); };
}

function r_prim_add_attr() {
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

function r_prim_bhv_to_html_inner(out) {
	var b = this;
	this.add_depend(out.get());

	this.compute = function(x) { return new Thunk(function() {
		var inner = out.get().html_inner().get();
		// TODO: clear old dependency
		b.add_depend(inner);
		return inner.compute(x).get();
	}); };
}

function r_prim_bjoin(out) {
        this.compute = function(x, env) {
                return out.get().compute(cthunk([]), env).get().compute(x, env);
        }
}

// BehaviourFun a b -> BehaviourFun b c -> BehaviourFun a c
function r_prim_compose(f, g) {
	this.add_depend(f.get());
	this.add_depend(g.get());

	this.compute = function(x, env) {
		var y = f.get().compute(x, env);
		return g.get().compute(y, env);
	};
}

// a -> BehaviourFun b a
function r_prim_const(value) {
        this.compute = function(x, env) {
		if (value.get().constructor.name == 'BhvFun')
			this.add_depend(value.get());

                return new Thunk(function() {
                        var vg = value.get();
			if (vg.constructor.name == 'BhvFun') {
				bhv = new BhvFun();
                                bhv.compute = function(x) {
                                        return vg.compute(x, env);
                                };
                                return bhv;
                        }
                        return vg;
                });
        };
}

function r_prim_debug() {
        this.compute_ = function(x) {
                alert(x[0].get() + ': ' + x[1].get());
                return x[1].get();
        };
}

function r_prim_fix() {
        this.compute = function(x, env) {
                var thunk;
                thunk = new Thunk(function() {
			return x.get()(thunk).get();
                });
                return thunk;
        }
}

function r_prim_eq() {
        this.compute_ = function(params) {
                return params[0].get() == params[1].get();
        };
}

function r_prim_eq_string() {
        this.compute_ = function(params) {
                return params[0].get() == params[1].get();
        };
}

function r_prim_error() {
        this.compute_ = function(msg) { alert(msg); };
}

function r_prim_gen() {
        this.compute = function() {
                return this.value;
        };

        this.change = function(value) {
		this.value = value;
                r_current_time++;
                this.invalidate();
        };

	this.gen.prop('rawe_bhv_gen', this);
}

function r_prim_gen_input_text() {
	r_prim_gen.call(this);

	var elem = this.gen;
	var b = this;

	this.value = cthunk(elem.val());

	elem.change(function(e) {
		b.change(cthunk(elem.val()));
	});
	elem.keyup(function(e) {
		b.change(cthunk(elem.val()));
	});

	elem.removeAttr('bhv-gen');
}

function r_prim_gen_input_button() {
	r_prim_gen.call(this);

	var b = this;
	var elem = this.gen;

	b.value = cthunk({ NotYet: [] });
	elem.click(function(e) {
		b.change(cthunk({ OnTime: [cthunk(++r_current_time), cthunk([])] }));
	});
}

function r_prim_gen_input_submit() {
	r_prim_gen.call(this);

	var b = this;
	var elem = this.gen;

	b.value = cthunk({ NotYet: [] });
	elem.click(function(e) {
		b.change(cthunk({ OnTime: [cthunk(++r_current_time), cthunk(elem.val())] }));
	});
}

function r_prim_gen_form() {
	r_prim_gen.call(this);
	var b = this;
	var elem = this.gen;

	if (typeof b.value == 'undefined')
		b.value = cthunk({ NotYet: null });

	elem.submit(function(e) {
		e.preventDefault();
		var result = {};
		elem.find('input, select, textarea').each(function() {
			result[$(this).attr('name')] = cthunk( $(this).val() );
		});
		b.change(cthunk({ OnTime: [cthunk(++r_current_time), cthunk(result)] }));
	});
}



function r_prim_hask_to_bhv(inner, func) {
	this.add_depend(func.get());
        this.compute = function(x, env) {
                var h2b = env.h2b || {};
                var nh2b = {};
                for (i in h2b) nh2b[i] = h2b[i];
                nh2b[inner.get().id] = x;

                var nenv = {};
                for (i in env) nenv[i] = env[i];
                nenv.h2b = nh2b;

                return func.get().compute(cthunk([]), nenv);
        };
}

function r_prim_hask_to_bhv_inner() {
        this.compute = function(x, env) {
                return env.h2b[this.id];
        };
}

function r_prim_lt_int() {
        this.compute_ = function(params) {
                return params[0].get() < params[1].get();
        }
}

function r_prim_plus() {
        this.compute_ = function(x) { return x[0].get()+x[1].get(); }
}

function r_prim_plus_mb() {
        this.compute_ = function(x) {
                if (typeof x[0].get().Just != 'undefined' && typeof x[1].get().Just != 'undefined')
                        return { Just: cthunk (x[0].get().Just.get() + x[1].get().Just.get()) };
                return { Nothing: null };
        }
}

function r_prim_fst() {
	this.compute_unbox = function(x) { return x.get()[0]; }
}

function r_prim_snd() {
	this.compute_unbox = function(x) { return x.get()[1]; }
}

function r_prim_product(f, g) {
	this.add_depend(f.get());
	this.add_depend(g.get());
        this.compute = function(x, env) { return new Thunk(function() {
                return [f.get().compute(x, env), g.get().compute(x, env)];
        }); };
}

function r_prim_sget(name) {
        this.values = [];
        var b = this;

        this.compute_ = function(x) {
                for (i in this.values) {
                        if (this.values[i][0] == x) {
				return { Just: cthunk( this.values[i][1] ) };
                        }
                }

		$.get(document.location.href, { q: name.get() }, function(json) {
                        b.values.push([x, jQuery.parseJSON(json)]);
                        r_current_time++;
                        b.invalidate();
                });

                return { Nothing: null };
        };
}

function r_prim_post(name, signal) {
	var result = cthunk( { NotYet: null } );
	var last_result = -1;
	var b = this;

	this.add_depend(signal.get());

	this.invalidate = function() {
		var x = signal.get().compute(cthunk(null)).get();

		if (typeof x.OnTime == 'undefined' || x.OnTime[0].get() <= this.last_change)
			return;

		this.last_change = x.OnTime[0].get();

		var obj = x.OnTime[1].get();
		var post = {};
		for (i in obj) post[i] = obj[i].get();

		$.post(document.location.href+'?q='+name.get(), post, function(json) {
			if (last_result > x.OnTime[0].get())
				return;

			var y = $.parseJSON(json);
			result = cthunk( { Just: cthunk(y) } );
			r_current_time++;
			for (i in b.rdepend)
				b.rdepend[i].invalidate();
		});
	};

	this.init = this.invalidate;

	this.compute = function() { return result; };
}

function r_prim_to_html_int() {
        this.compute_ = function(x) {
                return $('<span>'+x+'</span>');
        };
}

function r_prim_to_html_jsstring() {
	this.compute_ = function(x) {
		// TODO: escape the string
		return $('<span>'+x+'</span>');
	};
}

function r_prim_append_html() {
	this.compute_ = function(params) {
		return params[0].get().add(params[1].get());
	};
}

function r_prim_html_until(def, mb) {
	this.add_depend(def.get());
	this.add_depend(mb.get());

	this.compute = function() { return new Thunk(function() {
		var x = mb.get().compute().get();
		var y = def.get().compute().get();

		if (typeof(x.Just) != 'undefined') {
			xj = x.Just.get();
			xj.prop('rawe_html_inner', y.prop('rawe_html_inner'));
			return xj;
			return x.Just.get().prop('rawe_html_inner', y.prop('rawe_html_inner'));
		}
		return y;
	}); };

	this.html_inner = function() {
		return def.get().html_inner();
	}
}

function r_prim_typeof() {
	this.compute_ = function(x) { return typeof x; }
}


/* Unsafe functions */

function r_prim_box() {
	this.compute = function(x) { return cthunk(x); }
}

function r_prim_unbox() {
	this.compute = function(x) { return x.get(); }
}


/* JSON interface */

function r_prim_to_js_string() {
	this.compute = function(cur) { return new Thunk(function() {
		var result = '';
		while (typeof cur.get().cons != 'undefined') {
			result += cur.get().cons[0].get();
			cur = cur.get().cons[1];
		}
		return result;
	}); };
}

function r_prim_from_js_string() {
	this.compute = function(str) { return new Thunk(function() {
		str = str.get();
		var end = { nil: [] };
		var result = end;

		for (i in str) {
			newend = { nil: [] };
			delete end.nil;
			end.cons = [ cthunk(str[i]), cthunk(newend) ];
			end = newend;
		}
		return result;
	}); };
}

function r_prim_to_js_object() {
	this.compute = function(cur) { return new Thunk(function() {
		var result = {};
		while (typeof cur.get().cons != 'undefined') {
			result[cur.get().cons[0].get()[0].get()] =
				cur.get().cons[0].get()[1];
			cur = cur.get().cons[1];
		}
		return result;
	}); };
}

function r_prim_from_js_object() {
	this.compute = function(obj) { return new Thunk(function() {
		obj = obj.get();
		var end = { nil: [] };
		var result = end;

		for (i in obj) {
			newend = { nil: [] };
			delete end.nil;
			end.cons = [ cthunk([cthunk(i), obj[i]]), cthunk(newend) ];
			end = newend;
		}
		return result;
	}); };
}

function r_prim_js_object_fmap() {
	this.compute = function(params, env) { return new Thunk(function() {
		var f = params.get()[0].get();
		var obj = params.get()[1].get();

		var result = {};
		for (i in obj) result[i] = f(obj[i]);
		return result;
	}); };
}



/* Bool constructors and destructor */

function r_prim_true() {
        this.compute = function() { return cthunk(true); };
}

function r_prim_false() {
        this.compute = function() { return cthunk(false); };
}

function r_prim_bool() {
	this.compute_unbox = function(params) {
                var t = params.get()[0];
                var f = params.get()[1].get()[0];
                var c = params.get()[1].get()[1];
		if (c.get()) return t;
		return f;
	};
}


/* List constructors and destructor */

function r_prim_nil() {
	this.compute = function() { return cthunk( { 'nil': [] }); };
}

function r_prim_cons() {
	this.compute = function(x) { return cthunk( { 'cons': x.get() }); };
}

function r_prim_list() {
	this.compute = function(params) { return new Thunk(function() {
		var b1 = params.get()[0];
		var b2 = params.get()[1].get()[0];
		var x = params.get()[1].get()[1].get();

		if (typeof x.nil != 'undefined')
			return b1.get();
		if (typeof x.cons != 'undefined')
			return b2.get()(x.cons[0]).get()(x.cons[1]).get();
	}); };
}


/* Maybe constructors and destructor */

function r_prim_nothing() {
        this.compute = function() { return cthunk({ Nothing: null }); };
}

function r_prim_just() {
        this.compute = function(x) { return cthunk( { Just: x } ); };
}

function r_prim_maybe() {
	this.compute = function(params) { return new Thunk(function() {
                var def = params.get()[0];
                var func = params.get()[1].get()[0];
                var mb = params.get()[1].get()[1];

                if (typeof mb.get().Just == 'undefined')
			return def.get();
		return func.get()(mb.get().Just).get();
	}); };
}


/* Timed constructors and destructor */

function r_prim_not_yet() {
	this.compute = function() { return cthunk({ NotYet: [] }); };
}

function r_prim_on_time() {
	this.compute = function(x) { return new Thunk(function() {
		return { OnTime: x.get() }
	}); };
}

function r_prim_timed() {
	this.compute = function(params) { return new Thunk(function() {
                var not_yet = params.get()[0];
                var on_time = params.get()[1].get()[0];
                var value = params.get()[1].get()[1];

		if (typeof value.get().OnTime == 'undefined')
			return not_yet.get();

		var ot = value.get().OnTime;
		return on_time.get()(ot[0]).get()(ot[1]).get();
	}); };
}

function r_prim_timed_map() {
        this.compute = function(params, env) { return new Thunk(function() {
                var f = params.get()[0];
                var x = params.get()[1];

		if (typeof x.get().OnTime != 'undefined')
			return { OnTime: [ x.get().OnTime[0], f.get().compute(x.get().OnTime[1], env) ] };
                return x.get();
        }); };
}

function r_prim_timed_fold(step, def, ev) {
	var value = def.get().compute(cthunk([]), {});
	var b = this;
	var last_recomp = 0;

	step = step.get();
	ev = ev.get();
	this.add_depend(ev);

	this.compute = function() { return new Thunk(function() {
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


/* Result constructors and destructor */

function r_prim_result_error() {
	this.compute = function(x) { return cthunk({ Error: [x] }); };
}

function r_prim_result_ok() {
	this.compute = function(x) { return cthunk({ Ok: [x] }); };
}

function r_prim_result() {
	this.compute = function(params) { return new Thunk(function() {
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

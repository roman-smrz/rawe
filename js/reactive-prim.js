
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
                return params[0].get().clone().attr(name, value);
        }
}

function r_prim_bhv_to_html_inner(out) {
        var b = this;
        this.depend[out.get().id] = true;

        this.compute = function(x, env) { return new Thunk(function() {
                $('*[bhv-id='+out.get().id+']').each(function() {
			var gen = $(this).attr('bhv-inner');
                        // TODO: some error message
                        if (!gen) return;

                        b.gen = gen;
                        b.depend[gen] = true;
                        r_behaviours[gen].rdepend[b.id] = true;
                        // TODO: clear old dependency
                });
                if (!b.gen) return cthunk( { Nothing: null, NotYet: null } );
                return r_behaviours[b.gen].compute(x, env).get();
        }); }
}

function r_prim_bjoin(out) {
        this.compute = function(x, env) {
                return out.get().compute(cthunk([]), env).get().compute(x, env);
        }
}

// BehaviourFun a b -> BehaviourFun b c -> BehaviourFun a c
function r_prim_compose(f, g) {
        this.depend[f.get().id] = true;
        this.depend[g.get().id] = true;

	this.compute = function(x, env, unboxed) {
		var y = f.get().compute(x, env, g.get().unboxed_param);
		return g.get().compute(y, env, unboxed);
	};
}

// a -> BehaviourFun b a
function r_prim_const(value) {
        this.compute = function(x, env) {
                return new Thunk(function() {
                        var vg = value.get();
                        if (vg.constructor.name == 'Behaviour') {
                                bhv = new Behaviour();
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
                        return x.get().compute(thunk, env).get();
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
                this.value = cthunk(value);
                r_current_time++;
                this.invalidate();
                r_update_html();
        };

}

function r_prim_hask_to_bhv(inner, func) {
        this.depend[func.get().id] = true;
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

function r_prim_length() {
        this.compute_ = function(list) {
                return list.length;
        }
}

function r_prim_lookup() {
        this.compute_ = function(param) {
                var key = param[0].get();
                var list = param[1].get();
                for (i in list) {
                        if (list[i].get()[0].get() == key)
                                return { Just: list[i].get()[1] };
                }
                return { Nothing: null };
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
        this.depend[f.get().id] = true;
        this.depend[g.get().id] = true;
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
                        r_update_html();
                });

                return { Nothing: null };
        };
}

function r_prim_spost(name) {
        var results = {};
        var b = this;

        this.compute = function(x) { return new Thunk(function() {
                if (typeof x.get().Timed == 'undefined')
                        return { Nothing: null };

                var time = x.get().Timed[0];
                var pairs = x.get().Timed[1].get();

                if (typeof results[time] != 'undefined')
                        return results[time]
                results[time] = { Nothing: null };

                var params = {};
                for (i in pairs)
                        params[pairs[i].get()[0].get()] = pairs[i].get()[1].get();

                $.post(document.location.href+'?q='+name.get(), params, function(json) {
                        results[time] = { Just: cthunk( jQuery.parseJSON(json) ) };
                        r_current_time++;
                        b.invalidate();
                        r_update_html();
                });

                return { Nothing: null };
        }); };
}

function r_prim_to_html_int() {
        this.compute_ = function(x) {
                return $('<span>'+x+'</span>');
        };
}

function r_prim_to_html_string() {
        this.compute_ = function(x) {
                // TODO: escape the string
                return $('<span>'+x+'</span>');
        };
}

function r_prim_to_html_jsstring() {
	this.compute = function(x) {
		// TODO: escape the string
		return cthunk($('<span>'+x+'</span>'));
	};
}

function r_prim_until() {
        this.compute = function(params) { return new Thunk(function() {
                var def = params.get()[0];
                var unt = params.get()[1];

                if (typeof unt.get().Just != 'undefined')
                        return unt.get().Just.get();
                return def.get();
        }); };
}


/* JSON interface */

function r_prim_to_js_string() {
	this.compute = function(cur) {
		var result = '';
		while (typeof cur.get().cons != 'undefined') {
			result += cur.get().cons[0].get();
			cur = cur.get().cons[1];
		}
		return result;
	};
}

function r_prim_from_js_string() {
	this.unboxed_param = true;
	this.compute = function(str) { return new Thunk(function() {
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
	this.compute = function(cur) {
		var result = {};
		while (typeof cur.get().cons != 'undefined') {
			result[cur.get().cons[0].get()[0]] =
				cur.get().cons[1].get()[1];
			cur = cur.get().cons[1];
		}
		return result;
	};
}

function r_prim_from_js_object() {
	this.unboxed_param = true;
	this.compute = function(obj) { return new Thunk(function() {
		var end = { nil: [] };
		var result = end;

		for (i in obj) {
			newend = { nil: [] };
			delete end.nil;
			end.cons = [ cthunk([i, obj[i]]), cthunk(newend) ];
			end = newend;
		}
		return result;
	}); };
}

function r_prim_js_object_fmap() {
	this.compute = function(params, env) {
		var f = params.get()[0].get();
		var obj = params.get()[1];

		var result = {};
		for (i in obj) result[i] = f.compute(obj[i], env);
		return result;
	};
}



/* Bool constructors and destructor */

function r_prim_true() {
        this.compute_ = function() { return true; };
}

function r_prim_false() {
        this.compute_ = function() { return false; };
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


/* Maybe constructors and destructor */

function r_prim_nothing() {
        this.compute = function() { return cthunk({ Nothing: null }); };
}

function r_prim_just() {
        this.compute = function(x) { return cthunk( { Just: x } ); };
}

function r_prim_maybe() {
	this.compute_unbox = function(params, env) {
                var def = params.get()[0];
                var func = params.get()[1].get()[0];
                var mb = params.get()[1].get()[1];

                if (typeof mb.get().Just == 'undefined')
			return def;
		return func.get().compute(mb.get().Just, env);
	};
}


/* Timed constructors and destructor */

function r_prim_not_yet() {
        this.compute = function() { return cthunk({ NotYet: null }); };
}

function r_prim_on_time() {
        this.compute = function(x) { return cthunk( { Timed: [ ++r_current_time, x ] } ); };
}

function r_prim_timed() {
	this.compute_unbox = function(params, env) {
                var not_yet = params.get()[0];
                var on_time = params.get()[1].get()[0];
                var value = params.get()[1].get()[1];

                if (typeof value.get().Timed == 'undefined')
			return not_yet;
		return on_time.get().compute(value.get().Timed[1], env);
	};
}

function r_prim_timed_fmap() {
        this.compute = function(params, env) { return new Thunk(function() {
                var f = params.get()[0];
                var x = params.get()[1];

                if (typeof x.get().Timed != 'undefined')
                        return { Timed: [ x.get().Timed[0], f.get().compute(x.get().Timed[1], env) ] };
                return x.get();
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
	this.compute_unbox = function(params, env) {
		var value = params.get()[1].get()[1].get();
		for (i in value) {
			var func;
			switch (i) {
				case 'Error': func = params.get()[0];
				case 'Ok': func = params.get()[1].get()[0];
			}
			return func.get().compute(value[i][0], env);
		}
	};
}

var prim = rawe.prim;

prim.id = function() {
	this.compute = function(x) { return x; }
}

prim.apply = function() {
	this.compute = function(params) { return new rawe.Thunk(function() {
		var func = params.get()[0].get();
		var value = params.get()[1];
		var res = func(value);
		return res.get();
	}); };
}

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
		return x.get().compute(rawe.cthunk({})).get();
	}); };
}

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

prim.bjoin = function(out) {
	this.compute = function(x) {
		return out.get().compute().get().compute(x);
	}
}

// BehaviourFun a b -> BehaviourFun b c -> BehaviourFun a c
prim.compose = function(f, g) {
	this.add_depend(f.get());
	this.add_depend(g.get());

	this.compute = function(x) {
		var y = f.get().compute(x);
		return g.get().compute(y);
	};
}

// a -> BehaviourFun b a
prim.const = function(value) {
	this.compute = function(x) {
		if (value.get().constructor.name == 'BhvFun')
			this.add_depend(value.get());

                return new rawe.Thunk(function() {
                        var vg = value.get();
			if (vg.constructor.name == 'BhvFun') {
				bhv = new rawe.BhvFun();
                                bhv.compute = function(x) {
					return vg.compute(x);
                                };
                                return bhv;
                        }
                        return vg;
                });
        };
}

prim.debug = function() {
        this.compute_ = function(x) {
                alert(x[0].get() + ': ' + x[1].get());
                return x[1].get();
        };
}

prim.fix = function() {
	this.compute = function(x) {
                var thunk;
                thunk = new rawe.Thunk(function() {
			return x.get()(thunk).get();
                });
                return thunk;
        }
}

prim.eq = function() {
        this.compute_ = function(params) {
                return params[0].get() == params[1].get();
        };
}

prim.eq_string = function() {
        this.compute_ = function(params) {
                return params[0].get() == params[1].get();
        };
}

prim.error = function() {
        this.compute_ = function(msg) { alert(msg); };
}

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



prim.lt_int = function() {
        this.compute_ = function(params) {
                return params[0].get() < params[1].get();
        }
}

prim.plus = function() {
        this.compute_ = function(x) { return x[0].get()+x[1].get(); }
}

prim.fst = function() {
	this.compute_unbox = function(x) { return x.get()[0]; }
}

prim.snd = function() {
	this.compute_unbox = function(x) { return x.get()[1]; }
}

prim.product = function(f, g) {
	this.add_depend(f.get());
	this.add_depend(g.get());
	this.compute = function(x) { return new rawe.Thunk(function() {
		return [f.get().compute(x), g.get().compute(x)];
        }); };
}

prim.sget = function(name) {
        this.values = [];
        var b = this;

        this.compute_ = function(x) {
                for (i in this.values) {
                        if (this.values[i][0] == x) {
				return { Just: rawe.cthunk( this.values[i][1] ) };
                        }
                }

		$.get(document.location.href, { q: name.get() }, function(json) {
                        b.values.push([x, jQuery.parseJSON(json)]);
                        rawe.current_time++;
                        b.invalidate();
                });

                return { Nothing: null };
        };
}

prim.post = function(name, signal) {
	var result = rawe.cthunk( { NotYet: null } );
	var last_result = -1;
	var b = this;

	this.add_depend(signal.get());

	this.invalidate = function() {
		var x = signal.get().compute().get();

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
			result = rawe.cthunk( { Just: rawe.cthunk(y) } );
			rawe.current_time++;
			for (i in b.rdepend)
				b.rdepend[i].invalidate();
		});
	};

	this.init = this.invalidate;

	this.compute = function() { return result; };
}

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

prim.append_html = function() {
	this.compute_ = function(params) {
		return params[0].get().add(params[1].get());
	};
}

prim.html_until = function(def, mb) {
	this.add_depend(def.get());
	this.add_depend(mb.get());

	this.compute = function() { return new rawe.Thunk(function() {
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

prim.typeof = function() {
	this.compute_ = function(x) { return typeof x; }
}



/* JSON interface */

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



/* Bool constructors and destructor */

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


/* List constructors and destructor */

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


/* Maybe constructors and destructor */

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


/* Timed constructors and destructor */

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


/* Result constructors and destructor */

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

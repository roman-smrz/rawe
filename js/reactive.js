var r_bhv_func = {};
var r_srv_deps = {};
var r_srv_val = {};
var r_srv_expr = {};
var r_event_deps = {};
var r_cur_bhv = null;


var r_bhv_fun_0 = {};
var r_current_time = 0;



function Thunk(thunk) {
        this.computed = false;
        this.value = null;
        this.thunk = thunk;

        this.get = function() {
                if (!this.computed) {
                        this.value = this.thunk();
                        this.computed = true;
                }
                return this.value;
        }
}

function cthunk(x) {
        thunk = new Thunk();
        thunk.value = x;
        thunk.computed = true;
        return thunk;
}


function BhvFun(id) {
        this.id = id;
	this.depend = [];
	this.rdepend = [];
        this.valid = false;
        this.last_change = 0;

        /*
        this.change = function(value) {
                if (this.value == value)
                        return;

                this.invalidate();
                this.value = value;
                this.valid = true;

                for (i in r_invalid)
			r_bhv_fun[r_invalid[i]].recompute();
                r_invalid = [];
        }
        */

	this.compute = function(x, env) {
                var b = this;
		if (b.compute_unbox) {
			return new Thunk(function() { return b.compute_unbox(x, env).get(); });
		}

                return new Thunk(function() { return b.compute_(x.get()) });
        }

        this.invalidate = function() {
                if (this.last_change == r_current_time)
                        return;

                this.valid = false;
                this.last_change = r_current_time;

                if (this.html) {
			var n = this.compute(cthunk(null)).get();
			if (this.html != n) {
				this.html.replaceWith(n);
				this.html = n;
			}
                }

                for (i in this.rdepend)
			this.rdepend[i].invalidate();
        }

	this.init = function() {
                if (this.valid) return;
		this.valid = true;

		for (i in this.depend)
			this.depend[i].init();

                if (this.html) {
			var n = this.compute(cthunk(null)).get();
			this.html.replaceWith(n);
			this.html = n;
                }
	}


	this.add_depend = function(bhv) {
		for (var i in this.depend)
			if (this.depend[i] == bhv)
				return;
		this.depend.push(bhv);
		bhv.rdepend.push(this);
	}

        /*
        this.recompute = function() {
                if (this.valid) return;

                if (i in this.depend)
			r_bhv_fun[i].recompute();
                this.value = this.compute();
                this.valid = true;
        }
        */
}


function r_init() {
	for (i in r_bhv_fun_0)
		r_bhv_fun_0[i].init();
}



/*

function r_compose(f, g) {
        var result = function(param) {
                if (typeof param == 'undefined') return undefined;
                param = g(param);
                if (typeof param == 'undefined') return undefined;
                return f(param);
        }
        return result;
}

function r_product(f, g) {
        return (function(param) {
                if (typeof param == 'undefined')
                        return undefined;
                x = f(param); y = g(param);
                if (typeof x == 'undefined' || typeof y == 'undefined')
                        return undefined;
                return [x, y];
        });
}

function r_bhv_update(bhv) {
        r_cur_bhv = bhv;
        var value = r_bhv_func[bhv](null);
        if (typeof value == 'undefined')
                return;

        var first = true
        $('*[bhv-id="'+bhv+'"]').each(function() {
                if (first) {
                        var newNode = value.clone();
                        newNode.each(function() { $(this).attr('bhv-id', ''+bhv); });
                        $(this).replaceWith(newNode);
                        first = false;
                } else {
                        $(this).remove();
                }
        });
}

function r_srv_call(name) {
        if (r_srv_deps[name] == undefined)
                r_srv_deps[name] = {};
        r_srv_deps[name][r_cur_bhv] = true;
        r_srv_expr[name] = name;

        return r_srv_val[name];
}


function r_bhv_init(id, rdep) {
        if (r_bhv_rdeps[id] == undefined)
                r_bhv_rdeps[id] = new Array();
        r_bhv_rdeps[id].push(rdep);

        if (r_bhv_time[id] != undefined)
                return;
        r_bhv_time[id] = 0;
        
        for (i in r_bhv_deps[id]) {
                dep = r_bhv_deps[id][i];
                r_bhv_init(dep, id);
        }
}


function call_event(id, value) {

}

function r_prim_toHtmlHtmlList(param) {
        var result = $('<div></div>');
        for (i in param) result.append(param[i].clone());
        return result.children();
}

function r_prim_enumFromTo(param) {
        var result = new Array();
        for (var i = param[0]; i <= param[1]; i++)
                result.push(i);
        return result;
}
*/


$.fn.find2 = function(selector) {
    return this.filter(selector).add(this.find(selector));
};

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

	this.compute = function(x) {
                var b = this;
		if (b.compute_unbox) {
			return new Thunk(function() { return b.compute_unbox(x).get(); });
		}

                return new Thunk(function() { return b.compute_(x.get()) });
        }

	this.html_inner = function(x) {
		return this.compute(x).get().prop('rawe_html_inner');
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

	this.init_dep = function() {
                if (this.valid) return;
		this.valid = true;

		for (i in this.depend)
			this.depend[i].init_dep();

                if (this.html) {
			var n = this.compute(cthunk(null)).get();
			this.html.replaceWith(n);
			this.html = n;
                }

		if (this.init) this.init();
	}


	this.add_depend = function(bhv) {
		for (var i in this.depend)
			if (this.depend[i] == bhv)
				return;
		this.depend.push(bhv);
		bhv.rdepend.push(this);
	}
}


function r_init() {
	for (i in r_bhv_fun_0)
		r_bhv_fun_0[i].init_dep();
}




$.fn.find2 = function(selector) {
    return this.filter(selector).add(this.find(selector));
};

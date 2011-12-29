var rawe = {
	// run-time representation of time, incremented whenever somthing changes
	current_time: 0,

	/* objects of this type represent thunks - possibly unevaluated expressions */
	Thunk: function (thunk) {
		this.computed = false;
		this.value = null;
		this.thunk = thunk;
		this.depend = [];

		this.get = function(transfer_depend) {
			if (!this.computed) {
				this.value = this.thunk.call(this);
				this.computed = true;
			}
			if (transfer_depend)
				transfer_depend.merge_depend(this);
			return this.value;
		}

		this.add_depend = function(dep) {
			for (i in this.depend)
				if (this.depend[i] == dep)
					return;

			this.depend.push(dep);
			return this;
		};

		this.merge_depend = function(other) {
			for (i in other.depend)
				this.add_depend(other.depend[i]);
			return this;
		};

		this.get_depend = function() {
			this.get();
			return this.depend;
		};
	},

	/* creates thunk with given (evaluated) value */
	cthunk: function(x) {
		thunk = new rawe.Thunk();
		thunk.value = x;
		thunk.computed = true;
		return thunk;
	},


	/* the run-time representation of behaviour functions: */

	BhvFun: function(id) {
		// ID (unnecessary, used for debugging)
		this.id = id;

		// list of behaviour functions, on which we depend
		this.depend = [];

		// list of behaviour functions, which depend on us
		this.rdepend = [];

		// information for invalidating (prevents multiple invalidatins
		// and resulting infinite loops)
		this.valid = false;
		this.last_change = 0;


		/* The compute function is usually provided by individual
		 * primitives; it gets a thunk with a parameter and returns a
		 * thunk with a result. It is also possible to implement other
		 * variants instead:
		 *
		 * - compute_t gets thunk, but returns just a value, it is
		 * 	wrapped in a thunk automatically
		 *
		 * - compute_ gets a parameter as a value and returns also value.
		 */
		this.compute = function(x) {
			var b = this;

			if (b.compute_t) {
				var thunk = new rawe.Thunk(function() {
					return b.compute_t(x, thunk);
				});
				return thunk;
			}

			var thunk = new rawe.Thunk(function() {
				return b.compute_(x.get(), thunk)
			});
			return thunk;
		}

		/* Returns the inner object of HTML-valued behaviour. This
		 * needs to be reimplemented in 'until' so we do not get
		 * infinite loops in the case of mutual dependencies
		 */
		this.html_inner = function(x) {
			return this.compute(x).get().prop('rawe_html_inner');
		}

		/* Invalidates the behaviour (if it wasn't already), recomputes
		 * the HTML, if we have any assigned, and send the signal
		 * further.
		 */
		this.invalidate = function() {
			if (this.last_change == rawe.current_time)
				return;

			this.valid = false;
			this.last_change = rawe.current_time;

			if (this.html) {
				this.clear_depend();
				var n = this.compute().get(this);
				if (this.html != n) {
					this.html.replaceWith(n);
					this.html = n;
				}
			}

			// We need to clone the reverse-dependency list,
			// because it may change while handling invalidations.
			var rdeps = this.rdepend.slice(0);
			for (i in rdeps) rdeps[i].invalidate();
		}

		/* Initialization with respect to dependencies */
		this.init_dep = function() {
			if (this.valid) return;
			this.valid = true;

			for (i in this.depend)
				this.depend[i].init_dep();

			if (this.html) {
				var n = this.compute().get(this);
				this.html.replaceWith(n);
				this.html = n;
			}

			if (this.init) this.init();
		}


		/* Adds a dependency */
		this.add_depend = function(bhv) {
			for (var i in this.depend)
				if (this.depend[i] == bhv)
					return;
			this.depend.push(bhv);
			bhv.rdepend.push(this);
		}

		this.merge_depend = function(other) {
			for (i in other.depend)
				this.add_depend(other.depend[i]);
			return this;
		};

		/* Removes a dependency */
		this.del_depend = function(bhv) {
			for (var i in this.depend) {
				if (this.depend[i] == bhv) {
					this.depend.splice(i, 1);
					break;
				}
			}

			for (var i in bhv.rdepend) {
				if (bhv.rdepend[i] == this) {
					bhv.rdepend.splice(i, 1);
					break;
				}
			}
		}

		this.clear_depend = function(bhv) {
			for (var i in this.depend) {
				for (var j in this.depend[i].rdepend) {
					if (this.depend[i].rdepend[j] == this)
						this.depend[i].rdepend.splice(j, 1);
				}
			}

			this.depend = [];
		};
	},


	/* Inicialization function */
	init: function(funs) {
		for (i in funs)
			funs[i].init_dep();
	},

	/* Here will come all the primitives */
	prim: {}
}



/* We need a find that also takes into account the top-level elements */
$.fn.find2 = function(selector) {
	return this.filter(selector).add(this.find(selector));
};

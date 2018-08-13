#
#	Rfunctions.R
#Tue 14 Aug 2007 01:39:42 PM CEST 

#
#	<§> abstract data functions
#

inverse = function(f, interval = c(-Inf, Inf)) {
	Vectorize(
	function(y, ...) {
		optimize(function(x, ...){ (y - f(x, ...))^2 }, interval = interval, ...)$minimum
	}
	)
}

#
#	<p> meta functions
#

callWithArgs = function(fctName, args) {
	#arguments = paste(sapply(names(args), function(n)sprintf("%s = %s", n, args[[n]])), collapse = ", ");
	fhead = sprintf("%s(%s)", fctName, paste(names(args), collapse = ", "));
	eval(parse(text = fhead))
}

.do.call = function(f, args, restrictArgs = T, usePositional = T, restrictPositional = F) {
	# <p> function arguments
	fargs = names(as.list(args(f)));
	# remove spurious arguments
	fargs = fargs[fargs != ''];

	if (restrictArgs && all(fargs != '...')) {
		idcs = which.indeces(fargs, names(args));
		if (usePositional) {
			positional = which(names(args) == '');
			Npositional = (length(fargs) - length(idcs));
			if (!restrictPositional && length(positional) > Npositional)
				stop(".do.call: unmachted positional arguments");
			idcs = c(idcs, positional[1:Npositional]);
		}
		args = args[sort(idcs)];
	}
	do.call(f, args)
}

callDelegate = function(functionBase, delegation, args, restrictArgs = T) {
	f = get(Sprintf('%{functionBase}s%{delegation}u'));
	.do.call(f, args, restrictArgs = restrictArgs)
}

CallDelegate = function(functionBase, delegation, ..., restrictArgs = T) {
	callDelegate(functionBase, delegation, args = list(...), restrictArgs = T)
}

#
#	<p> generic functions
#

Identity = function(...)list(...)

#
#	<p> benchmarking
#

benchmark.timed = function(.f, ..., N__ = 1e1) {
	t0 = Sys.time();
	for (i in 1:N__) {
		r = .f(...);
	}
	t1 = Sys.time();
	r = list(time = (t1 - t0)/N__, lastResult = r, t0 = t0, t1 = t1);
	print(r$time);
	print(r$t0);
	print(r$t1);
	r
}

Benchmark = function(expr, N__ = 1, verbose = T, returnTiming = F, Nabbr = 20, logLevel = 2,
	gcFirst = FALSE, timeStat = sum, envir = parent.frame()) {
	s = Deparse(substitute(expr));

	# <p> timing
	t0 = Sys.time();
	# <i> for-loop required due to side-effects
	r0 = NULL;
	timesCal = NULL;
	timesSys = NULL;
	for (i in 1:N__) {
		t0i = Sys.time();
		t = system.time(r0 <- eval(expr, envir = envir), gcFirst = gcFirst);
		t1i = Sys.time();
		timesCal[i] = t1i - t0i;
		timesSys[i] = timeStat(t);
	}
	t1 = Sys.time();

	# <p> stats
	#timeTotal = t1 - t0;
	#timeIteration = timeTotal/N__;
	timing = list(
		timeCal = sum(timesCal), timeCalIter = mean(timesCal), timeCalSd = sd(timesCal),
		timeSys = sum(timesSys), timeSysIter = mean(timesSys), timeSysSd = sd(timesSys),
		lastResult = r0, t0 = t0, t1 = t1
	);

	if (verbose) with(timing, {
		exprStr = strAbbr(s, Nabbr);
		l = logLevel;
		Logs('Timing of %{exprStr}s', logLevel = l);
		Logs('\tCal: %{timeCal}.2f Iter: %{timeCalIter}.2f (%{timeCalSd}.1f)', logLevel = l);
		Logs("\tSys: %{timeSys}.2f Iter: %{timeSysIter}.2f (%{timeSysSd}.1f)", logLevel = l);
	})
	r = if (returnTiming) timing else r0;
	r
}

#
#	<p> optimization
#

# Ngrid should be uneven
# not used at the moment
gridFactor = function(Ngrid = 5, Step = .5, Factor = 1.5) {
	genF = function(center, min, max) {
		gridMarginal = lapply(center, function(p) {
			gridRaw = (Step * Factor ^ (0: (Ngrid - 1)));
			c(p - gridRaw, p, p + gridRaw)
		});
		grid = as.matrix(Rbind(merge.multi.list(gridMarginal)));
		return(grid);
	};
	genF
}

gridBounding = function(Ngrid = 5) {
	genF = function(center, min, max) {
		gridMarginal = lapply(seq_along(min), function(i) {
			seq(min[i], max[i], length.out = Ngrid)
		});
		grid = as.matrix(Rbind(merge.multi.list(gridMarginal)));
		return(grid);
	};
	genF
}

searchContourGridRaw = function(f, grid, v, ..., contour = 0.05, gridGen, eps = 1e-3, lower = T) {
	# assume regular grid
	Ndim = ncol(grid);
	pts = apply(grid, 2, function(v)sort(unique(v)));
	Ns = apply(pts, 2, function(pts)pop(seq_along(pts)));
	# list of canonical points of sub-hypercubes
	cubes = as.matrix(Rbind(merge.multi.list(Df_(Ns))));
	# calculate offsets to get all vertices of hypercube, coords per column
	hyper = t2r(sapply(1:(2^Ndim) - 1, ord2bin, digits = Ndim));
	# iterate hypercubes to decide value, nd defines canonical vertex of hypercube
	sel = apply(cubes, 1, function(nd) {
		NdsCube = t(nd + hyper);
		#print(NdsCube - (nd + t_(hyper)));
		# search funcion values on vertices of hypercube
		#coords = cbind(pts[NdsCube[, 1], 1], pts[NdsCube[, 2], 2]);
		coords = sapply(1:ncol(NdsCube), function(i)pts[NdsCube[, i], i]);
		idcs = DfSearch(Df_(coords), Df_(grid));
		vs = v[idcs];
		if (any(vs <= contour) & any(vs >= contour)) { # tb persued
			mn = apply(coords, 2, min);
			mx = apply(coords, 2, max);
			center = (mn + mx) / 2;
			searchContourGrid(f, gridGen(center, mn, mx), ...,
				contour = contour, gridGen = gridGen, eps = eps, lower = lower);
		} else list()
	});
	return(unlist.n(sel, 1));
}

searchContourGrid = function(f, grid, ..., contour = 0.05, gridGen, eps = 1e-3, lower = T) {
	# compute values of function on grid vertices
	v = apply(grid, 1, f, ...);
	# determine recursion end
	mn = apply(grid, 2, min);
	mx = apply(grid, 2, max);
	# found contour elevation to desired accuracy
	#print(max(mx - mn));
	if (max(mx - mn) < eps) {
		i = if (lower) which.min(v) else which.max(v);
		return(list(grid[i, ]));
	}
	# continue searching
	r = searchContourGridRaw(f, grid, v, ..., contour = contour, gridGen = gridGen, eps = eps, lower = lower);
	return(r);
}

searchContourGridList = function(f, gridList, ..., contour = 0.05, gridGen, eps = 1e-2, lower = T) {
	gL = lapply(gridList, searchContourGrid, ..., f = f, contour = contour, gridGen = gridGen);
	return(unlist.n(gL, 1));
}


searchContour = function(f, start, ..., contour = 0.05, delta = 3,
	gridGen = gridBounding(Ngrid = 3), eps = 1e-2, lower = T) {
	grid = gridGen(start, start - delta, start + delta);
	r = searchContourGrid(f, grid, ..., contour = contour, gridGen = gridGen, eps = eps);
	return(do.call(rbind, r));
}

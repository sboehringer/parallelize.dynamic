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

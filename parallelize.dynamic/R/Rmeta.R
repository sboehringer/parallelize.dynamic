#
#	Rmeta.R
#Wed Jun  3 15:11:27 CEST 2015


#
#	<p> Meta-functions
#

#
#		Environments
#

# copy functions code adapted from restorepoint R package
object.copy = function(obj) {
	# Dealing with missing values
	if (is.name(obj)) return(obj);
	obj_class = class(obj);

	copy =
		if ('environment' %in% obj_class) environment.copy(obj) else
		if (all('list' == class(obj))) list.copy(obj) else
		#if (is.list(obj) && !(is.data.frame(obj))) list.copy(obj) else
		obj;
	return(copy)
}
list.copy = function(l)lapply(l, object.copy);
environment.restrict = function(envir__, restrict__= NULL) {
	if (!is.null(restrict__)) {
		envir__ = as.environment(List_(as.list(envir__), min_ = restrict__));
	}
	envir__
}
environment.copy = function(envir__, restrict__= NULL) {
	as.environment(eapply(environment.restrict(envir__, restrict__), object.copy));
}

bound_vars = function(f, functions = F) {
	fms = formals(f);
	# variables bound in default arguments
	vars_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e)))));
	# variables used in the body
	vars_body = setdiff(all.vars(body(f)), names(fms));
	vars = setdiff(unique(c(vars_defaults, vars_body)), c('...', '', '.GlobalEnv'));
	if (functions) {
		vars = vars[!sapply(vars, function(v)is.function(rget(v, envir = environment(f))))];
	}
	vars
}
bound_fcts_std_exceptions = c('Lapply', 'Sapply', 'Apply');
bound_fcts = function(f, functions = F, exceptions = bound_fcts_std_exceptions) {
	fms = formals(f);
	# functions bound in default arguments
	fcts_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e), functions = T))));
	# functions bound in body
	fcts = union(fcts_defaults, all.vars(body(f), functions = T));
	# remove variables
	#fcts = setdiff(fcts, c(bound_vars(f, functions), names(fms), '.GlobalEnv', '...'));
	fcts = setdiff(fcts, c(bound_vars(f, functions = functions), names(fms), '.GlobalEnv', '...'));
	# remove functions from packages
	fcts = fcts[
		sapply(fcts, function(e) {
			f_e = rget(e, envir = environment(f));
			!is.null(f_e) && environmentName(environment(f_e)) %in% c('R_GlobalEnv', '') && !is.primitive(f_e)
	})];
	fcts = setdiff(fcts, exceptions);
	fcts
}


environment_evaled = function(f, functions = FALSE, recursive = FALSE) {
	vars = bound_vars(f, functions);
	e = nlapply(vars, function(v) rget(v, envir = environment(f)));
	#Log(sprintf('environment_evaled: vars: %s', join(vars, ', ')), 7);
	#Log(sprintf('environment_evaled: functions: %s', functions), 7);
	if (functions) {
		fcts = bound_fcts(f, functions = T);
		fcts_e = nlapply(fcts, function(v){
			#Log(sprintf('environment_evaled: fct: %s', v), 7);
			v = rget(v, envir = environment(f));
			#if (!(environmentName(environment(v)) %in% c('R_GlobalEnv')))
			v = environment_eval(v, functions = T);
		});
		#Log(sprintf('fcts: %s', join(names(fcts_e))));
		e = c(e, fcts_e);
	}
	#Log(sprintf('evaled: %s', join(names(e))));
	r = new.env();
	lapply(names(e), function(n)assign(n, e[[n]], envir = r));
	#r = if (!length(e)) new.env() else as.environment(e);
	parent.env(r) = .GlobalEnv;
	#Log(sprintf('evaled: %s', join(names(as.list(r)))));
	r
}
environment_eval = function(f, functions = FALSE, recursive = FALSE) {
	environment(f) = environment_evaled(f, functions = functions, recursive = recursive);
	f
}

#
#		Freeze/thaw
#

delayed_objects_env = new.env();
delayed_objects_attach = function() {
	attach(delayed_objects_env);
}
delayed_objects_detach = function() {
	detach(delayed_objects_env);
}

thaw_list = function(l)lapply(l, thaw_object, recursive = T);
thaw_environment = function(e) {
	p = parent.env(e);
	r = as.environment(thaw_list(as.list(e)));
	parent.env(r) = p;
	r
}

# <i> sapply
thaw_object_internal = function(o, recursive = T, envir = parent.frame()) {
	r = 		 if (class(o) == 'ParallelizeDelayedLoad') thaw(o) else
	#if (recursive && class(o) == 'environment') thaw_environment(o) else
	if (recursive && class(o) == 'list') thaw_list(o) else o;
	r
}

thaw_object = function(o, recursive = T, envir = parent.frame()) {
	if (all(search() != 'delayed_objects_env')) delayed_objects_attach();
	thaw_object_internal(o, recursive = recursive, envir = envir);
}

#
#	<p> backend classes
#

setGeneric('thaw', function(self, which = NA) standardGeneric('thaw'));

setClass('ParallelizeDelayedLoad',
	representation = list(
		path = 'character'
	),
	prototype = list(path = NULL)
);
setMethod('initialize', 'ParallelizeDelayedLoad', function(.Object, path) {
	.Object@path = path;
	.Object
});

setMethod('thaw', 'ParallelizeDelayedLoad', function(self, which = NA) {
	if (0) {
	key = sprintf('%s%s', self@path, ifelse(is.na(which), '', which));
	if (!exists(key, envir = delayed_objects_env)) {
		Log(sprintf('Loading: %s; key: %s', self@path, key), 4);
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		assign(key, object, envir = delayed_objects_env);
		gc();
	} else {
		#Log(sprintf('Returning existing object: %s', key), 4);
	}
	#return(get(key, envir = delayed_objects_env));
	# assume delayed_objects_env to be attached
	return(as.symbol(key));
	}

	delayedAssign('r', {
		gc();
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		object
	});
	return(r);
});

RNGuniqueSeed = function(tag) {
	if (exists('.Random.seed')) tag = c(.Random.seed, tag);
	md5 = md5sumString(join(tag, ''));
	r = list(
		kind = RNGkind(),
		seed = hex2int(substr(md5, 1, 8))
	);
	r
}

RNGuniqueSeedSet = function(seed) {
	RNGkind(seed$kind[1], seed$kind[2]);
	#.Random.seed = freeze_control$rng$seed;
	set.seed(seed$seed);
}

FreezeThawControlDefaults = list(
	dir = '.', sourceFiles = c(), libraries = c(), objects = c(), saveResult = T,
	freeze_relative = F, freeze_ssh = T, logLevel = Log.level()
);

thawCall = function(
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag)) {

	load(freeze_file, envir = .GlobalEnv);
	r = with(callSpecification, {
		for (library in freeze_control$libraries) {
			eval(parse(text = sprintf('library(%s)', library)));
		}
		for (s in freeze_control$sourceFiles) source(s, chdir = T);
		Log.setLevel(freeze_control$logLevel);
		if (!is.null(freeze_control$rng)) RNGuniqueSeed(freeze_control$rng);

		if (is.null(callSpecification$freeze_envir)) freeze_envir = .GlobalEnv;
		# <!> freeze_transformation must be defined by the previous source/library calls
		transformation = eval(parse(text = freeze_control$thaw_transformation));
		r = do.call(eval(parse(text = f)), transformation(args), envir = freeze_envir);
		#r = do.call(f, args);
		if (!is.null(freeze_control$output)) save(r, file = freeze_control$output);
		r
	});
	r
}

frozenCallWrap = function(freeze_file, freeze_control = FreezeThawControlDefaults,
	logLevel = Log.level(), remoteLogLevel = logLevel)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {
	sp = splitPath(freeze_file, ssh = freeze_ssh);
	file = if (freeze_relative) sp$file else sp$path;
	#wrapperPath = sprintf("%s-wrapper.RData", splitPath(file)$fullbase);
	r = sprintf("R.pl --template raw --no-quiet --loglevel %d --code 'eval(get(load(\"%s\")[[1]]))' --",
		logLevel, file);
	r
})

frozenCallResults = function(file) {
	callSpecification = NULL;	# define callSpecification
	load(file);
	get(load(callSpecification$freeze_control$output)[[1]]);
}

freezeCallEncapsulated = function(call_,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_objects = NULL, thaw_transformation = identity)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {

	sp = splitPath(freeze_file, ssh = freeze_ssh);
	outputFile = if (freeze_save_output)
		sprintf("%s_result.RData", if (freeze_relative) sp$base else sp$fullbase) else
		NULL;

	callSpecification = list(
		f = deparse(call_$fct),
		#f = freeze_f,
		args = call_$args,
		freeze_envir = if (is.null(call_$envir)) new.env() else call_$envir,
		freeze_control = list(
			sourceFiles = sourceFiles,
			libraries = libraries,
			output = outputFile,
			rng = freeze_control$rng,
			logLevel = freeze_control$logLevel,
			thaw_transformation = deparse(thaw_transformation)
		)
	);
	thawFile = if (freeze_relative) sp$file else sp$path;
	callWrapper = call('thawCall', freeze_file = thawFile);
	#Save(callWrapper, callSpecification, thawCall, file = file);
	#Save(c('callWrapper', 'callSpecification', 'thawCall', objects),
	#	file = freeze_file, symbolsAsVectors = T);
	#Save(c(c('callWrapper', 'callSpecification', 'thawCall'), objects),
	Save(c('callWrapper', 'callSpecification', 'thawCall', freeze_objects),
		file = freeze_file, symbolsAsVectors = T);
	freeze_file
})

# <!> assume matched call
# <A> we only evaluate named args
callEvalArgs = function(call_, env_eval = FALSE) {
	#if (is.null(call_$envir__) || is.null(names(call_$args))) return(call_);
	#if (is.null(call_$envir) || !length(call_$args)) return(call_);

	# <p> evaluate args
	if (length(call_$args)) {
		args = call_$args;
		callArgs = lapply(1:length(args), function(i)eval(args[[i]], envir = call_$envir));
		# <i> use match.call instead
		names(callArgs) = setdiff(names(call_$args), '...');
		call_$args = callArgs;
	}

	if (env_eval) {
		call_$fct = environment_eval(call_$fct, functions = T);
	}
	# <p> construct return value
	#callArgs = lapply(call_$args, function(e){eval(as.expression(e), call_$envir)});
	call_
}

#callWithFunctionArgs = function(f, args, envir__ = parent.frame(), name = NULL) {
callWithFunctionArgs = function(f, args, envir__ = environment(f), name = NULL, env_eval = FALSE) {
	if (env_eval) f = environment_eval(f, functions = T);
	call_ = list(
		fct = f,
		envir = environment(f),
		args = args,
		name = name
	);
	call_
}

freezeCall = function(freeze_f, ...,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_envir = parent.frame(), freeze_objects = NULL, freeze_env_eval = F,
	thaw_transformation = identity) {

	# args = eval(list(...), envir = freeze_envir)
	call_ = callWithFunctionArgs(f = freeze_f, args = list(...),
		envir__ = freeze_envir, name = as.character(sys.call()[[2]]), env_eval = freeze_env_eval);

	freezeCallEncapsulated(call_,
		freeze_control = freeze_control, freeze_tag = freeze_tag,
		freeze_file = freeze_file, freeze_save_output = freeze_save_output, freeze_objects = freeze_objects,
		thaw_transformation = thaw_transformation
	);
}


encapsulateCall = function(.call, ..., envir__ = environment(.call), do_evaluate_args__ = FALSE,
	unbound_functions = F) {
	# function body of call
	name = as.character(.call[[1]]);
	fct = get(name);
	callm = if (!is.primitive(fct)) {
		callm = match.call(definition = fct, call = .call);
		as.list(callm)[-1]
	} else as.list(.call)[-1];
	args = if (do_evaluate_args__) {
		nlapply(callm, function(e)eval(callm[[e]], envir = envir__))
	} else nlapply(callm, function(e)callm[[e]])
	# unbound variables in body fct
	#unbound_vars = 

	call_ = list(
		fct = fct,
		envir = envir__,

		#args = as.list(sys.call()[[2]])[-1],
		args = args,

		name = name
	);
	call_
}


#
#	</p> freeze/thaw functions
#

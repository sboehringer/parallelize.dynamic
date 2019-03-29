#
#	Rmeta.R
#Wed Jun  3 15:11:27 CEST 2015
#Mon 27 Jun 2005 10:49:06 AM CEST
#system("~/src/Rprivate/exportR.sh");
#system("~/src/Rprivate/exportR.sh"); source("RgenericAllRaw.R"); source("Rgenetics.R"); loadLibraries();
#system('. ~/src/Rprivate/exportR.sh ; cp ~/src/Rprivate/RgenericAllRaw.R .');

#
#	<p> Meta-helpers
#

#
#	level dependend logging
# moved from Rsystem.R to break dependency cylce (22.3.2017)
#Global..Log..Level = 4;
#Default..Log..Level = 4;
#assign(Default..Log..Level, 4, envir = .GlobalEnv);
Log_env__ <- new.env();
assign('DefaultLogLevel', 4, envir = Log_env__);

#' Log a message to stderr.
#' 
#' Log a message to stderr. Indicate a logging level to control verbosity.
#' 
#' This function prints a message to stderr if the condition is met that a
#' global log-level is set to greater or equal the value indicated by
#' \code{level}. \code{Log.level} returns the current logging level.
#' 
#' @aliases Log Log.setLevel Log.level
#' @param o Message to be printed.
#' @param level If \code{Log.setLevel} was called with this value, subsequent
#' calls to \code{Log} with values of \code{level} smaller or equal to this
#' value will be printed.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{Log.setLevel}}, ~~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 	Log.setLevel(4);
#' 	Log('hello world', 4);
#' 	Log.setLevel(3);
#' 	Log('hello world', 4);
#' 
Log = function(o, level = get('DefaultLogLevel', envir = Log_env__), doPrint = NULL) {
	if (level <= get('GlobalLogLevel', envir = Log_env__)) {
		cat(sprintf("R %s: %s\n", date(), as.character(o)));
		if (!is.null(doPrint)) print(doPrint);
	}
}
Logs = function(o, level = get('DefaultLogLevel', envir = Log_env__), ..., envir = parent.frame()) {
	Log(Sprintf(o, ..., envir = envir), level = level);
}
LogS = function(level, s, ..., envir = parent.frame()) {
	Log(Sprintf(s, ..., envir = envir), level = level);
}

Log.level = function()get('GlobalLogLevel', envir = Log_env__);
Log.setLevel = function(level = get('GlobalLogLevel', envir = Log_env__)) {
	assign("GlobalLogLevel", level, envir = Log_env__);
}
Log.expr = function(level, expr, envir = parent.frame()) {
	oldLevel = Log.level();
	on.exit(Log.setLevel(oldLevel));
	Log.setLevel(level);
	return(eval(expr, envir = envir));
}

Log.setLevel(4);	# default


Stop = function(..., call. = TRUE, domain = NULL, envir = parent.frame()) {
	stop(Sprintf(list(...)[[1]], envir = envir), call., domain)
}

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
		fcts = bound_fcts(f, functions = TRUE);
		fcts_e = nlapply(fcts, function(v){
			#Log(sprintf('environment_evaled: fct: %s', v), 7);
			v = rget(v, envir = environment(f));
			#if (!(environmentName(environment(v)) %in% c('R_GlobalEnv')))
			v = environment_eval(v, functions = TRUE);
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
#	Parsing, evaluation
#

Parse = function(text, ...) {
	parse(text = text, ...)
}
Eval = function(e, ..., envir = parent.frame(), autoParse = T) {
	if (autoParse && is.character(e)) e = Parse(e, ...);
	eval(e, envir = envir)
	
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

freezeObjectsCooked = function(objects, envir = parent.frame(), defaultEnv = 0) {
	if (is.null(objects)) return(NULL);
	ns = names(objects);
	o = lapply(seq_along(objects), function(i) {
		if (ns[i] != '') {
			lapply(objects[[i]], function(n)setNames(list(get(n, envir = get(ns[i], envir))), n));
		} else setNames(list(get(objects[[i]], envir = envir)), objects[[i]])
	});
	return(setNames(o, ns));
}
thawObjectsCooked = function(objects, envir = parent.frame(), assignPos = 1) {
	if (is.null(objects)) return(NULL);
	ns = names(objects);
	freeze = lapply(seq_along(objects), function(i) {
		if (ns[i] != '') {
			os = sapply(objects[[i]], function(o) {
				this = mget(ns[i], envir, ifnotfound = NA)[[1]];
				if (!is.environment(this)) {
					this = new.env();
					assign(ns[i], this, pos = assignPos);
				}
				assign(names(o[1]), o[[1]], envir = this);
				return(names(o[1]));
			});
			return(os);
		} else {
			assign(names(objects[[i]]), objects[[i]][[1]], pos = assignPos);
			return(names(objects[[i]]));
		}
	});
	return(setNames(freeze, ns));
}

thawCall = function(
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	envir = .GlobalEnv) {

	load(freeze_file, envir = envir);
	r = with(callSpecification, {
		for (library in freeze_control$libraries) {
			eval(parse(text = sprintf('library(%s)', library)));
		}
		for (s in freeze_control$sourceFiles) source(s, chdir = T);
		Log.setLevel(freeze_control$logLevel);
		if (!is.null(freeze_control$rng)) RNGuniqueSeed(freeze_control$rng);
		thawObjectsCooked(freeze_objects, envir);

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
	freeze_save_output = F, freeze_objects = NULL, freeze_objects_envir = parent.frame(),
	thaw_transformation = identity)
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
	freeze_objects = freezeObjectsCooked(freeze_objects, freeze_objects_envir);
	Save(c('callWrapper', 'callSpecification', 'thawCall', 'freeze_objects'),
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
		call_$fct = environment_eval(call_$fct, functions = FALSE, recursive = FALSE);
	}
	# <p> construct return value
	#callArgs = lapply(call_$args, function(e){eval(as.expression(e), call_$envir)});
	call_
}

#callWithFunctionArgs = function(f, args, envir__ = parent.frame(), name = NULL) {
callWithFunctionArgs = function(f__, args__, envir__ = environment(f__), name = NULL, env_eval = FALSE) {
	if (env_eval) f = environment_eval(f__, functions = FALSE, recursive = FALSE);
	call_ = list(
		fct = f__,
		envir = environment(f__),
		args = args__,
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
		freeze_file = freeze_file, freeze_save_output = freeze_save_output,
		freeze_objects = freeze_objects, freeze_objects_envir = freeze_envir,
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
# compact saving for delayed loading
#
# <i> make 
# object: list with single element
freezeObject = function(object, env) {
	dir = attr(env, 'path');
	name = names(object);
	file = Sprintf('%{dir}s/%{name}s.Rdata');
print(file);
	save(list = name, envir = as.environment(object), file = file);
	eval(substitute(delayedAssign(OBJECT, get(load(file = FILE)[1])),
		list(OBJECT = name, FILE = file)), envir = env);
}
freezeObjectsList = function(objects, pos = 2, parent = parent.frame(), freezeObjectDir = NULL) {
	td = firstDef(freezeObjectDir, tempdir());
	env = new.env(parent = parent);
	attr(env, "path") = td;

	if (any(names(objects) == '')) stop('Only named objects can be frozen. Check for naming conflicts.');
	#n = names(objects) == '';
	#if (sum(n) > 0) names(objects)[n] = paste('ARG_ANON__', 1:sum(n), sep = '');
	nlapply(objects, function(n)freezeObject(objects[n], env = env));
	env
}
freezeObjects = function(..., pos = 2, parent = parent.frame(), freezeObjectDir = NULL) {
	freezeObjectsList(list(...), pos = pos, parent, freezeObjectDir)
}

#
#	</p> freeze/thaw functions
#

Deparse = function(o)join(deparse(o));

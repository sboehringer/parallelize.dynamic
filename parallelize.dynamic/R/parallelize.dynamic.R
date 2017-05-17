#
#	parrallelize.dynamic.R
#

.onLoad = function(libname, pkgname) {
	Log.setLevel(2);
	perlPath = system.file('Perl', package = pkgname);
	Sys.setenv(PERL5LIB = sprintf('%s:%s', perlPath, Sys.getenv('PERL5LIB')));
	Sys.setenv(PATH = sprintf('%s:%s', perlPath, Sys.getenv('PATH')));
	# might exist from reconstructions on remote machines
	if (!exists('parallelize_env')) parallelize_env <<- new.env();
	parallelize_setEnable(F);
}

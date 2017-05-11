#
#	update.R
#Wed Jan 15 13:56:41 2014

# run this with Rscript update.R
require('roxygen2');
require('devtools');

if (0) {
	system('./pullin.sh');
}

if (1) {
	#roxygenize('../parallelize.dynamic', roclets = 'namespace');
	#update_collate('../parallelize.dynamic');
	#system('git commit -a -m "documentation update" ; git push');
	document('../parallelize.dynamic');

	# <p> currently broken (22.6.2015)
	#install_local('../parallelize.dynamic');
	# <p> current workaround for broken install_local (22.6.2015)
	source('../parallelize.dynamic/R/Rdata.R');
	source('../parallelize.dynamic/R/Rsystem.R');
	Install_local('../parallelize.dynamic', dependencies = FALSE);
}

if (0) {
	install_github('sboehringer/parallelize.dynamic', subdir = 'parallelize.dynamic');
}

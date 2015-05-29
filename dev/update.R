#
#	update.R
#Wed Jan 15 13:56:41 2014

# run this with Rscript update.R

if (1) {
	system('./pullin.sh');
}

if (1) {
	require('roxygen2');
	require('devtools');
	roxygenize('.', roclets = 'namespace');
	update_collate('.');
	#system('git commit -a -m "documentation update" ; git push');
	document('.');
	install_local('.');
}

if (0) {

}

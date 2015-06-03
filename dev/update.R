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
	roxygenize('../parallelize.dynamic', roclets = 'namespace');
	update_collate('../parallelize.dynamic');
	#system('git commit -a -m "documentation update" ; git push');
	document('../parallelize.dynamic');
	#install_local('../parallelize.dynamic');
}

if (0) {

}

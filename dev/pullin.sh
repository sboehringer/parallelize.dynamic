#
#	pullin.sh
#Tue Jan 14 17:29:47 CET 2014

# script to update external dependencies that cannot be handled through version control

#
#	<p> perl dependencies
#

PERLLIB=~/src/privatePerl
cp $PERLLIB/TempFileNames.pm $PERLLIB/Set.pm ~/bin/qsub.pl ../parallelize.dynamic/inst/Perl

#
#	<p> R
#
cp $RPRIVATE/Rdata.R $RPRIVATE/Rsystem.R $RPRIVATE/Rfunctions.R ../parallelize.dynamic/R

#
#	<p> R (just once)
#
#cp $RPRIVATE/Rparallel.R $RPRIVATE/Rparallel.back.R $RPRIVATE/Rparallel_setEnable_pkg.R R
#cp $RPRIVATE/Rparallel_functions_parallel.R $RPRIVATE/Rparallel_functions_std.R inst/R

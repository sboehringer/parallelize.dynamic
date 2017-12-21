#
#	pullin.sh
#Tue Jan 14 17:29:47 CET 2014

# script to update external dependencies that cannot be handled through version control

#
#	<p> perl dependencies
#

PERLLIB=~/src/privatePerl

cp $PERLLIB/TempFileNames.pm $PERLLIB/Set.pm $PERLLIB/PropertyList.pm ~/bin/qsub.pl ~/bin/R.pl ../parallelize.dynamic/inst/Perl
cp $PERLLIB/Statistics/R.pm ../parallelize.dynamic/inst/Perl/Statistics


#
#	<p> R
#
export RPRIVATE=~/src/Rprivate
cp $RPRIVATE/Rdata.R $RPRIVATE/Rsystem.R $RPRIVATE/Rfunctions.R $RPRIVATE/Rmeta.R ../parallelize.dynamic/R

#
#	<p> R (just once)
#
#cp $RPRIVATE/Rparallel.R $RPRIVATE/Rparallel.back.R $RPRIVATE/Rparallel_setEnable_pkg.R R
#cp $RPRIVATE/Rparallel_functions_parallel.R $RPRIVATE/Rparallel_functions_std.R inst/R

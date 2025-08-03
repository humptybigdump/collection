#! /bin/sh

echo Number of parameters: $#
echo List of parameters: "$@"

LOGFILE=$1

# Quoting of variable to prevent wrong behaviour
# if $1 is not set.
# (Better) alternative, check if at least one
# positional parameter is given.
if [ ! -e "$LOGFILE" ]; then
	echo Error: $LOGFILE doesn\'t exist
	echo exiting...
	exit 1
fi

echo "Writing: $(date): $(du -sm ~)" 
echo "$(date): $(du -sm ~)" >>$LOGFILE

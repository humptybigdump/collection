#! /bin/sh


# If this script assumes that the EXERCISE_DIR is a subdirectory:

# We check if the directory exists and  change to to EXERCISE_DIR

EXERCISE_DIR=Shell-Uebung

if [ ! -d $EXERCISE_DIR ]; then
	echo "ERROR: $EXERCISE_DIR doesn't exist in current working directory"
	exit 1
fi

cd Shell-Uebung || exit

# Easier:
# If the script is supposed to be called in the EXERCISE_DIR
# the following is all that is needed:

for word in Integral Ableitung Energieerhaltung; do
	echo $word:
	mkdir -p $word
	files=$(grep -rl $word .)
	for file in $files; do
		cp "$file" $word
	done
done

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define CANARY 0x654d614c

void function(char *str) {
	int32_t auth = 0;
	int32_t canary = CANARY;
	char buf[8];
	int32_t i = 0;

	for (i = 0; i < strlen(str); i++)
		buf[i] = str[i];
		if (canary != CANARY)
			exit(1);
		if (auth != 0)
			printf("Superuser!\n");
	}

int main(int argc, char **argv) {
	if (argc > 1)
	function(argv[1]);
	return 0;
}

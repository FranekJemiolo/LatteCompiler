#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void printInt(int x) {
	printf("%d\n", x);
}

void printString(char* string) {
	printf("%s\n", string);
}

void error() {
	printf("runtime error\n");
	exit(1);
	return;
}

int readInt() {
	int x;
	scanf("%d\n", &x);
	return x;
}

char* readString() {
	char* line = NULL;
	size_t len = 0;
	size_t read;
	if ((read = getline(&line, &len, stdin)) != -1) {
	  char* cleanedLine = malloc(sizeof(char)*(strlen(line)));
	  for (int i = 0; i < strlen(line); i++) {
	    cleanedLine[i] = line[i];
	  }
	  cleanedLine[strlen(line) - 1] = '\0';
		return cleanedLine;
	} else {
		error();
		return NULL;
	}
}

char* _appendString(char* str1, char* str2) {
  char* appended = malloc(strlen(str1) + strlen(str2) + 1);
  strcpy(appended, str1);
  strcat(appended, str2);
  return appended;
}

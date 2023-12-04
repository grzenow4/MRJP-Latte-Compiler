#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef const char* string;
typedef long long int64;

void printInt(int64 n) {
    printf("%lld\n", n);
}

void printString(string s) {
    printf("%s\n", s);
}

void error() {
    fprintf(stderr, "Runtime error\n");
    exit(1);
}

int64 readInt() {
    int64 n;
    scanf("%lld ", &n);
    return n;
}

string readString() {
    char *str = NULL;
    size_t size = 0;

    ssize_t read = getline(&str, &size, stdin);

    if (read == -1) {
        fprintf(stderr, "Error reading input\n");
        exit(1);
    }

    if (read > 0 && str[read - 1] == '\n') {
        str[read - 1] = '\0';
    }

    return str;
}

string __concatString(string s1, string s2) {
    char *res = malloc(strlen(s1) + strlen(s2) + 1);
    if (res == NULL) {
        fprintf(stderr, "Error in malloc\n");
        exit(1);
    }
    strcpy(res, s1);
    strcat(res, s2);
    return res;
}

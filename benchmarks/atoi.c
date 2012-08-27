#include <stdlib.h>
#include <string.h>

/* We have to wrap atoi because it expects a null-terminated string */
int hs_atoi(char *str, int len) {
    char *str0 = malloc(len + 1);
    int x;
    strncpy(str0, str, len);
    str0[len] = '\0';
    x = atoi(str0);
    free(str0);
    return x;
}

// C-ohjelmana
#include <stdio.h>

void lisaa(int h, int m, int lisa_min) {
    int yht_min = h*60 + m + lisa_min;
    h = yht_min / 60;
    m = yht_min % 60;
}

void tulosta(int h, int m) {
    printf("%02d:%02d\n",h,m);
}

int main(void) {
    int h=12,m=15;
    tulosta(h,m);
    lisaa(h,m,55);
    tulosta(h,m);
    return 0;
}


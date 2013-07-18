#include <stdio.h>


int table[201][8];
int coins[] = {1,2,5,10,20,50,100,200};

int main(char *argc, char **argv){


    int i = 0;
    int j = 0;

    for(i = 0; i < 201; i++){
        for(j = 0; j < 8; j++){
            table[i][j] = -1;
        }

    }

    int asdf = 1+getChange(200,7);
    printf("%d\n",asdf);


    return 0;
}


int getChange(int n, int m){

    if (n == 0) return 1;
    else if (n < 0) return 0;
    else if (m <= 0 && n >=1) return 0;
    else if ( table[n][m-1] != -1 ) return table[n][m-1];

    else{
        table[n][m-1] = getChange(n,m-1) + getChange( n - coins[m-1] , m );
        return table[n][m-1];
    }

}



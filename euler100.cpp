#include <iostream>
using namespace std;


unsigned long long int oblue 	= 500000000001;
unsigned long long int ored 		= 500000000000;

unsigned long long int blue 		= 500000000001;
unsigned long long int red 		= 500000000000;

unsigned long long int total = 0;

double p;

int main (int argc, char const *argv[])
{
	
	while(true){
		while(true){
			total = blue+red;
			p = ((double)blue)/((double)total);
			p = p * (((double)(blue-1)) / ((double)(total-1)));
			//p = ((double)blue/(double)total) * (double)(blue-1)/(double)(total-1);
			cout << "Blue " << blue << " | Red " << red << " Total " << total << " p = " << p << endl;
			if(p == 0.5){
				cout << blue;
				return 0;
			}
			if(p > 0.5){
				break;
			}
			blue++;
		}
		blue = oblue++;
		red = ored++;
	}
	
	
	return 0;
}

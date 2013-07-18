#include <stdio.h>
#include <stdlib.h>

char bouncy(int num, char *foo);
char inc(char *foo);


char bouncy(int num, char *foo){
	
	char *ptr = foo;
	int count = 0;
	
	while(num > 0){
		int f = (num % 10);
		*ptr = (f + '0');
		ptr++;
		num /= 10;
		count++;
	}
	
	count--;
	ptr = foo;
	
	char decreasing = inc(foo);
	
	while(count >= 1){
		*ptr ^= *(ptr+count);
		*(ptr+count) ^= *(ptr);
		*ptr ^= *(ptr+count);
		ptr++;
		count -= 2;
	}
	
	char increasing = inc(foo);
	
	return ((!decreasing) && (!increasing));
}

char inc(char *foo){
	char *ptr = foo;
	
	while(*(ptr+1) != 0){
		char a = (*ptr) - '0';
		char b = (*(ptr+1)) -'0';
		
		if( a > b ) {
			return 0; //false
		} 
		else{
			ptr++;
		}
	}
	return 1; //true
}


int main(int argc, char **argv){
	
	char str[10];		//buffer to store the value;
	
	int i = 1;
	
	int count = 0;
	char a;
	char *ptr = str;
	
	
	//euler 112 here
	while(1){
		
		
		//clean up the buffer
		while(*(ptr) != 0){
			*ptr = 0;
			ptr++;
		}
		
		a = bouncy(i,str);
		
		if(a){
			//printf("%s is a bouncy number\n",str);
			count++;
		}
		
		double k = ((double)count / (double)i);
		printf("i = %d, count = %d, prop = %.10f\n",i,count,k);
		
		if( k == 0.99){
			break;
		}
		
		i++;
	}
	
	
	
	//euler113 here
	
	while(0){
		
		
		//clean up the buffer
		while(*(ptr) != 0){
			*ptr = 0;
			ptr++;
		}
		
		a = bouncy(i,str);
		
		if(a){
			//printf("%s is a bouncy number\n",str);
			count++;
		}
		
		double k = ((double)count / (double)i);
		printf("i = %d, count = %d, prop = %.10f\n",i,count,k);
		
		if( k == 0.99){
			break;
		}
		
		i++;
	}
	
	
	
	printf("Total number of bouncy numbers is %d\n",count);
	
	printf("Size %d", sizeof(double));

}







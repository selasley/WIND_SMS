#include <stdio.h>
int main(int argc, char *argv[])
{
char dst[5]="1234";
char src[5]="ABCD";
int i;

for (i=0;i<4;i++) dst[i]=src[i];
return 1;
}

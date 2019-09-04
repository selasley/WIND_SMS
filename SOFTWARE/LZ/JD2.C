#include <stdio.h>


/******************************************************************************
* JulianDay.
*    The year, month, and day are assumed to have already been validated.  This
* is the day since 0 AD (julian day may not be the proper term).
******************************************************************************/

long JulianDay (y,m,d)
long y, m, d;
{
  return (367*y-7*(y+(m+9)/12)/4-3*((y+(m-9)/7)/100+1)/4+275*m/9+d+1721029);
}

int main(int argc, char *argv[])
{
  long scclock1,tjd,jdn,julianday,year,month,day,jd2;

  while (1) {
  printf("Enter year, month, day): ");
  scanf("%d,%d,%d", &year, &month, &day);

  jd2 = JulianDay(year,month,day);

  printf("JD (CDF) = %d\n",jd2);
  }
}

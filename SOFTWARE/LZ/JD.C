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
  long scclock1,tjd,jdn,julianday,year,month,doy,jd2;

  printf("Enter SC clock(1) as 8-digit hex number: ");
  scanf("%x", &scclock1);
  printf("Enter ATC value (year, doy): ");
  scanf("%d,%d", &year, &doy);

  tjd = (scclock1 >> 17 ) & 0x3FFF;
  jdn = tjd + 2440000;
  jd2 = JulianDay(year,1L,1L);
  jd2 = jd2 + (doy-1);

  printf("JD (PB5) = %d\n",jdn);
  printf("JD (CDF) = %d\n",jd2);
}

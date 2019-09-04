      subroutine setoctbox(n,vx,vy,vs)
      integer*2 n
      real*4 vx(60,0:8),vy(60,0:8),vs(60,0:3)
      integer*2 j,k,m
      INTEGER*2 SPCSW
      BYTE SPCNM(80)

 8888 continue					! OPEN NEW FILE FIRST TIME
      type *,' enter OCTBOX filename.DAT->  fname(.dat)',
     +         'or "@" to for manual entry'
      accept 1001,SPCNM
 1001 format(80A1)
    1   continue
        if(SPCNM(1).EQ.'@') then
          do n=1,1
            do j = 0,7				! get vertices
              type*,' enter vertices of corner',j,' of box', n
              accept*, vx(n,j),vy(n,j)
            enddo
            call setslope(n,vx,vy,vs)		! set slopes in setslope
          enddo
          n = 1
        else					!c  call readoctbox(n,vx,vy)
          SPCSW=0                               ! OR ON REQUEST
          DO WHILE (.NOT.SPCSW)
 1000     continue			!  IF(IFIRST.EQ.1) THEN
c          TYPE *,' ENTER OCTBOX filename.DAT->  fname(.dat)'
c          ACCEPT 1001,SPCNM
          SPCNM(80)=0
          OPEN(UNIT=70,NAME=SPCNM,STATUS='OLD',FORM='FORMATTED',
     1           RECL=300, READONLY,ERR=99)
          SPCSW=1
99        IF(.NOT.SPCSW) THEN
            TYPE *,' ERROR OPENING DISKFILE... REENTER'
            GOTO 8888
          END IF
          END DO

          do m = 1,60
            read(70,*,err=9999,end=9999)(vx(m,j),vy(m,j),j=0,7)
 7777       format(16f)
          enddo  
 9999     n = 60
          if(m.lt.60) n = m - 1
          type*,n,' boxes read!'
          close(70)
          do j = 1,n
            call setslope(j,vx,vy,vs)
          enddo
        endif
       return
       end

       subroutine setslope(n,vx,vy,vs)
       integer*2 n
       real*4 vx(60,0:8),vy(60,0:8),vs(60,0:3)
       integer*2 j,k,mm
c       common vectors /vx,vy,vs/

c********   set slopes if != 0 or 1...i.e., j/2-1
c********       do mm = 1,n
         mm = n
c         type*,' box ',mm
         vx(mm,8)=vx(mm,0)
         vy(mm,8)=vy(mm,0)
         do k=2,8,2
           vs(mm,k/2-1) = 0.
           if(vx(mm,k).ne.vx(mm,k-1)) then
             vs(mm,k/2-1) = (vy(mm,k)-vy(mm,k-1))
     1                     /(vx(mm,k)-vx(mm,k-1))
           endif
c          type*,' k:',vx(mm,k),vy(mm,k),' k-1',vx(mm,k-1),vy(mm,k-1)
c          type*,' slope ',(k/2-1),' is ',vs(mm,k/2-1)
          enddo
c        enddo  
      return
        end

       logical function octbox(n,vx,vy,vs,x,y)
       integer*2 n
       real*4 x,y
       real*4 vx(60,0:8),vy(60,0:8),vs(60,0:3)
       integer*2 j,k

c   bottom and top 
       if(y.lt.vy(n,7) .or. y.ge.vy(n,2)) goto 9998		!break

c   left and right sides
       if(x.lt.vx(n,0) .or. x.ge.vx(n,4)) goto 9998		!break

c   top left corner
       if(y.ge.vy(n,1).and.x.lt.vx(n,2)		!<- 'y.gt....'
     1    .and.vs(n,0).ne.0.) then
         if(y.ge.(vy(n,1)+vs(n,0)*(x-vx(n,1)))) goto 9998	!break
       endif

c   top right corner
       if(y.gt.vy(n,4).and.x.gt.vx(n,3)
     1    .and.vs(n,1).ne.0.) then
         if(y.ge.(vy(n,3)+vs(n,1)*(x-vx(n,3)))) goto 9998	!break
       endif

c   bottom left corner
       if(y.lt.vy(n,0).and.x.lt.vx(n,7)
     1    .and.vs(n,3).ne.0.) then
         if(y.lt.(vy(n,7)+vs(n,3)*(x-vx(n,7)))) goto 9998	!break
       endif

c   bottom right corner
       if(y.lt.vy(n,5).and.x.gt.vx(n,6)
     1    .and.vs(n,2).ne.0.) then
         if(y.lt.(vy(n,5)+vs(n,2)*(x-vx(n,5)))) goto 9998	!break
       endif
       octbox = 1			! particle's in the box
       return
 9998  octbox = 0			! particle's not in the box
       return
       end

       subroutine octrange(n,vx,vy,xmn,xmx,ymn,ymx)
       integer*2 n
       real*4 vx(60,0:8),vy(60,0:8),xmn,xmx,ymn,ymx
       integer*2 j,k,mm

        xmn = 3.33e33
        ymn = 3.33e33
        xmx = -3.33e32
        ymx = -3.33e32
        do mm = 1,n
         do j=0,7,1
           if(vx(mm,j).lt.xmn) xmn = vx(mm,j)
           if(vx(mm,j).gt.xmx) xmx = vx(mm,j)
         enddo
         do j=0,7,1
           if(vy(mm,j).lt.ymn) ymn = vy(mm,j)
           if(vy(mm,j).gt.ymx) ymx = vy(mm,j)
         enddo
       enddo
      return
        end


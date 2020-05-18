      module hycom_read_latlon
      implicit none

      contains

      subroutine get_coord(lat_p,lon_p,area_p, &
                           lat_q,lon_q,area_q, &
                           itdm,jtdm)
      implicit none

      real, dimension(itdm,jtdm), intent(inout) :: lat_p, lon_p
      real, dimension(itdm,jtdm), intent(inout) :: lat_q, lon_q
      real, dimension(itdm,jtdm), intent(inout) :: area_p, area_q
      real*4, dimension(itdm,jtdm) :: tmp1, tmp2
      real*4, allocatable :: pad(:)

      integer :: npad
      integer :: ios,nrecl
      integer :: i,j
      character(len=240) :: cfilea
      integer :: itdm,jtdm

      cfilea = 'regional.grid.a'

      npad = 4096 - MOD(itdm*jtdm,4096)
      if(npad.eq.4096) npad=0

      allocate(pad(npad))

      INQUIRE( IOLENGTH=nrecl) tmp1,pad

#if defined(ENDIAN_IO)
      open(unit=11,file=cfilea, form='unformatted', status='old', &
               access='direct', recl=nrecl, convert="BIG_ENDIAN", &
               iostat=ios)
#else
      open(unit=11,file=cfilea, form='unformatted', status='old', &
               access='direct', recl=nrecl, &
               iostat=ios)
#endif

      IF (ios.ne.0) THEN
        print *,"error in reading regional.grid.a"
        call exit(1)
      endif

      read(11,rec=1,iostat=ios) tmp1
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, plon"
        call exit(2)
      endif
      lon_p = tmp1

      read(11,rec=2,iostat=ios) tmp1
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, plat"
        call exit(3)
      endif
      lat_p = tmp1

      read(11,rec=3,iostat=ios) tmp1
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, qlon"
        call exit(2)
      endif
      lon_q = tmp1

      read(11,rec=4,iostat=ios) tmp1
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, qlat"
        call exit(3)
      endif
      lat_q = tmp1

      do j=1, jtdm
        do i=1, itdm
          if (lon_p(i,j).ge.360.) lon_p(i,j) = lon_p(i,j)-360.
          if (lon_q(i,j).ge.360.) lon_q(i,j) = lon_q(i,j)-360.
        enddo
      enddo

      print *,'readHycomLatLon,plat, min,max=', &
          minval(lat_p),maxval(lat_p)
      print *,'readHycomLatLon,plon, min,max=', &
          minval(lon_p),maxval(lon_p)
      print *,'readHycomLatLon,qlat, min,max=', &
          minval(lat_q),maxval(lat_q)
      print *,'readHycomLatLon,qlon, min,max=', &
          minval(lon_q),maxval(lon_q)

      read(11,rec=10,iostat=ios) tmp1
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, pscx"
        call exit(3)
      endif

      read(11,rec=11,iostat=ios) tmp2
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, pscy"
        call exit(3)
      endif

      print *,'readHycomLatLon,pscx, min,max=', &
          minval(tmp1),maxval(tmp1)
      print *,'readHycomLatLon,pscy, min,max=', &
          minval(tmp2),maxval(tmp2)

      ! calculate area on p
      area_p = tmp1*tmp2

      read(11,rec=12,iostat=ios) tmp1
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, pscx"
        call exit(3)
      endif

      read(11,rec=13,iostat=ios) tmp2
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, qscy"
        call exit(3)
      endif

      print *,'readHycomLatLon,qscx, min,max=', &
          minval(tmp1),maxval(tmp1)
      print *,'readHycomLatLon,qscy, min,max=', &
          minval(tmp2),maxval(tmp2)

      ! calculate area on q
      area_q = tmp1*tmp2

      ! deallocate temporary arrays
      if (allocated(pad)) deallocate(pad)

      return
      end subroutine get_coord

      end module hycom_read_latlon

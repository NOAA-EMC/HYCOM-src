#if defined (USE_NUOPC_CESMBETA) || (ESPC_COUPLE)
#define USE_NUOPC_GENERIC 1
#endif
      module mod_import

      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays

      implicit none

      public hycom_imp_reset
      public hycom_imp_mrg

      contains

! --- reset imp_merge flag
      subroutine hycom_imp_reset(reset)
      logical :: reset
      character(*),parameter :: rname = 'hycom_imp_reset'

#if defined (USE_NUOPC_GENERIC)
        imp_merge = reset
        cpl_merge = reset
#endif
      end subroutine hycom_imp_reset

! --- merge import data where imp_merge is true
      subroutine hycom_imp_mrg()
      character(*),parameter :: rname = 'hycom_imp_mrg'
      integer                :: i,j,jja

#if defined (USE_NUOPC_GENERIC)
#if defined(ARCTIC)
!   arctic (tripole) domain, top row is replicated (ignore it)
        jja=min(jj,(jtdm-1-j0))
#else
        jja=jj
#endif

        if(natm.eq.2) then
          if(cpl_taux) then
            if (mnproc.eq.1) print *, rname//" merge imp_taux"
            taux(:,:,l0) = &
              merge(imp_taux(:,:,l0),taux(:,:,l0),imp_merge(:,:,l0))
            taux(:,:,l1) = &
              merge(imp_taux(:,:,l0),taux(:,:,l1),imp_merge(:,:,l0))
          endif
          if (cpl_tauy) then
            if (mnproc.eq.1) print *, rname//" merge imp_tauy"
            tauy(:,:,l0) = &
              merge(imp_tauy(:,:,l0),tauy(:,:,l0),imp_merge(:,:,l0))
            tauy(:,:,l1) = &
              merge(imp_tauy(:,:,l0),tauy(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_wndspd.or.calc_wndspd) then
            if (mnproc.eq.1) print *, rname//" merge imp_wndspd"
            wndspd(:,:,l0) = &
              merge(imp_wndspd(:,:,l0),wndspd(:,:,l0),imp_merge(:,:,l0))
            wndspd(:,:,l1) = &
              merge(imp_wndspd(:,:,l0),wndspd(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_ustara) then
            if (mnproc.eq.1) print *, rname//" merge imp_ustara"
            ustara(:,:,l0) = &
              merge(imp_ustara(:,:,l0),ustara(:,:,l0),imp_merge(:,:,l0))
            ustara(:,:,l1) = &
              merge(imp_ustara(:,:,l0),ustara(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_airtmp) then
            if (mnproc.eq.1) print *, rname//" merge imp_airtmp"
            airtmp(:,:,l0) = &
              merge(imp_airtmp(:,:,l0),airtmp(:,:,l0),imp_merge(:,:,l0))
            airtmp(:,:,l1) = &
              merge(imp_airtmp(:,:,l0),airtmp(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_vapmix) then
            if (mnproc.eq.1) print *, rname//" merge imp_vapmix"
            vapmix(:,:,l0) = &
              merge(imp_vapmix(:,:,l0),vapmix(:,:,l0),imp_merge(:,:,l0))
            vapmix(:,:,l1) = &
              merge(imp_vapmix(:,:,l0),vapmix(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_precip) then
            if (mnproc.eq.1) print *, rname//" merge imp_precip"
            precip(:,:,l0) = &
              merge(imp_precip(:,:,l0),precip(:,:,l0),imp_merge(:,:,l0))
            precip(:,:,l1) = &
              merge(imp_precip(:,:,l0),precip(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_surtmp) then
            if (mnproc.eq.1) print *, rname//" merge imp_surtmp"
            surtmp(:,:,l0) = &
              merge(imp_surtmp(:,:,l0),surtmp(:,:,l0),imp_merge(:,:,l0))
            surtmp(:,:,l1) = &
              merge(imp_surtmp(:,:,l0),surtmp(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_seatmp) then
            if (mnproc.eq.1) print *, rname//" merge imp_seatmp"
            seatmp(:,:,l0) = &
              merge(imp_seatmp(:,:,l0),seatmp(:,:,l0),imp_merge(:,:,l0))
            seatmp(:,:,l1) = &
              merge(imp_seatmp(:,:,l0),seatmp(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_sic) then
            write(lp,'(/ a,a /)') 'error - ', &
              rname//' cpl_sic merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sitx) then
            write(lp,'(/ a,a /)') 'error - ', &
              rname//' cpl_sitx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sity) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sity merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_siqs) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_siqs merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sifh) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sifh merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sifs) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sifs merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sifw) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sifw merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sit) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sit merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sih) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sih merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_siu) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_siu merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_siv) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_siv merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_swflx) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_swflx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_lwmdnflx) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_lwmdnflx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_lwmupflx) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_lwmupflx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_latflx) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_latflx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_sensflx) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sensflx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_orivers) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_orivers merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_irivers) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_irivers merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_implicit) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_implicit merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_swflx_net.or.cpl_swflx_net2down.or.cpl_swflxd) then
            if (mnproc.eq.1) print *, rname//" merge imp_swflx"
            swflx(:,:,l0) = &
              merge(imp_swflx(:,:,l0),swflx(:,:,l0),imp_merge(:,:,l0))
            swflx(:,:,l1) = &
              merge(imp_swflx(:,:,l0),swflx(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_lwflx_net.or.cpl_lwflx_net2down.or.cpl_lwflxd) then
            if (mnproc.eq.1) print *, rname//" merge imp_lwdflx"
            lwflx(:,:,l0) = &
              merge(imp_lwdflx(:,:,l0),lwflx(:,:,l0),imp_merge(:,:,l0))
            lwflx(:,:,l1) = &
              merge(imp_lwdflx(:,:,l0),lwflx(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_sbhflx) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_sbhflx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_lthflx) then
            write(lp,'(/ a,a /)') 'error - ', &
               rname//' cpl_lthflx merge is not implemented'
            call flush(lp)
            call xcstop(rname)
                   stop rname
          endif
          if(cpl_u10) then
            if (mnproc.eq.1) print *, rname//" merge imp_wndspx"
            wndspx(:,:,l0) = &
              merge(imp_wndspx(:,:,l0),wndspx(:,:,l0),imp_merge(:,:,l0))
            wndspx(:,:,l1) = &
              merge(imp_wndspx(:,:,l0),wndspx(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_v10) then
            if (mnproc.eq.1) print *, rname//" merge imp_wndspy"
            wndspy(:,:,l0) = &
              merge(imp_wndspy(:,:,l0),wndspy(:,:,l0),imp_merge(:,:,l0))
            wndspy(:,:,l1) = &
              merge(imp_wndspy(:,:,l0),wndspy(:,:,l1),imp_merge(:,:,l0))
          endif
          if(cpl_mslprs) then
            if (mnproc.eq.1) print *, rname//" merge imp_mslprs"
            mslprs(:,:,l0) = &
              merge(imp_mslprs(:,:,l0),mslprs(:,:,l0),imp_merge(:,:,l0))
            mslprs(:,:,l1) = &
              merge(imp_mslprs(:,:,l0),mslprs(:,:,l1),imp_merge(:,:,l0))
          endif
          if(calc_radflx) then
            if (mnproc.eq.1) print *, rname//" merge imp_radflx"
            radflx(:,:,l0) = &
              merge(imp_radflx(:,:,l0),radflx(:,:,l0),imp_merge(:,:,l0))
            radflx(:,:,l1) = &
              merge(imp_radflx(:,:,l0),radflx(:,:,l1),imp_merge(:,:,l0))
          endif
        else
          write(lp,'(/ a,a /)') 'error - ', &
             rname//' merge natm.ne.2 is not implemented'
          call flush(lp)
          call xcstop(rname)
                 stop rname
        end if

!   check CICE feedback
      if ((.not.cpl_sic) .or.(.not.cpl_sitx).or.(.not.cpl_sity).or. &
        (.not.cpl_siqs).or.(.not.cpl_sifh).or.(.not.cpl_sifs).or. &
        (.not.cpl_sifw).or.(.not.cpl_sit) .or.(.not.cpl_sih) .or. &
        (.not.cpl_siu) .or.(.not.cpl_siv)) then
        if (mnproc.eq.1) print *, &
          'warning... no feedback from CICE to HYCOM('//rname//')'
        else
!       copy import data to sea ice
        do j=1, jja
        do i=1, ii
          if (imp_merge(i,j,l0)) then
            if (ishlf(i,j).eq.1) then !standard ocean point
              if ((iceflg.ge.2).and.(icmflg.ne.3)) then
                covice(i,j)=sic_import(i,j) !Sea Ice Concentration
                si_c(i,j)=sic_import(i,j) !Sea Ice Concentration
                if (covice(i,j).gt.0.0) then
                   si_tx(i,j) = -sitx_import(i,j) !Sea Ice X-Stress into ocean
                   si_ty(i,j) = -sity_import(i,j) !Sea Ice Y-Stress into ocean
                  fswice(i,j) =  siqs_import(i,j) !Solar Heat Flux thru Ice to Ocean
                  flxice(i,j) =  fswice(i,j) + &
                                 sifh_import(i,j) !Ice Freezing/Melting Heat Flux
                  sflice(i,j) =  sifs_import(i,j)*1.e3 !Ice Salt Flux
                  wflice(i,j) =  sifw_import(i,j) !Ice freshwater Flux
                  temice(i,j) =   sit_import(i,j) !Sea Ice Temperature
                    si_t(i,j) =   sit_import(i,j) !Sea Ice Temperature
                  thkice(i,j) =   sih_import(i,j) !Sea Ice Thickness
                    si_h(i,j) =   sih_import(i,j) !Sea Ice Thickness
                    si_u(i,j) =   siu_import(i,j) !Sea Ice X-Velocity
                    si_v(i,j) =   siv_import(i,j) !Sea Ice Y-Velocity
                else
                   si_tx(i,j) = 0.0
                   si_ty(i,j) = 0.0
                  fswice(i,j) = 0.0
                  flxice(i,j) = 0.0
                  sflice(i,j) = 0.0
                  wflice(i,j) = 0.0
                  temice(i,j) = 0.0
                    si_t(i,j) = 0.0
                  thkice(i,j) = 0.0
                    si_h(i,j) = 0.0
                    si_u(i,j) = 0.0
                    si_v(i,j) = 0.0
                endif !covice
              elseif ((iceflg.ge.2).and.(icmflg.eq.3)) then
                si_c(i,j)=sic_import(i,j) !Sea Ice Concentration
                if (si_c(i,j).gt.0.0) then
                  si_tx(i,j) = -sitx_import(i,j) !Sea Ice X-Stress into ocean
                  si_ty(i,j) = -sity_import(i,j) !Sea Ice Y-Stress into ocean
                   si_h(i,j) =   sih_import(i,j) !Sea Ice Thickness
                   si_t(i,j) =   sit_import(i,j) !Sea Ice Temperature
                   si_u(i,j) =   siu_import(i,j) !Sea Ice X-Velocity
                   si_v(i,j) =   siv_import(i,j) !Sea Ice Y-Velocity
                else
                  si_tx(i,j) = 0.0
                  si_ty(i,j) = 0.0
                   si_h(i,j) = 0.0
                   si_t(i,j) = 0.0
                   si_u(i,j) = 0.0
                   si_v(i,j) = 0.0
                endif !covice
              endif !iceflg>=2 (icmflg)
            endif !ishlf
          endif !imp_merge
        enddo
        enddo

#if defined(ARCTIC)
!       update last active row of array
!jcx    call xctila( sic_import,1,1,halo_ps) !Sea Ice Concentration
!jcx    call xctila(sitx_import,1,1,halo_pv) !Sea Ice X-Stress
!jcx    call xctila(sity_import,1,1,halo_pv) !Sea Ice Y-Stress
!jcx    call xctila(siqs_import,1,1,halo_ps) !Solar Heat Flux thru Ice to Ocean
!jcx    call xctila(sifh_import,1,1,halo_ps) !Ice Freezing/Melting Heat Flux
!jcx    call xctila(sifs_import,1,1,halo_ps) !Ice Freezing/Melting Salt Flux
!jcx    call xctila(sifw_import,1,1,halo_ps) !Ice Net Water Flux
!jcx    call xctila( sit_import,1,1,halo_ps) !Sea Ice Temperature
!jcx    call xctila( sih_import,1,1,halo_ps) !Sea Ice Thickness
!jcx    call xctila( siu_import,1,1,halo_pv) !Sea Ice X-Velocity
!jcx    call xctila( siv_import,1,1,halo_pv) !Sea Ice Y-Velocity
        if ((iceflg.ge.2).and.(icmflg.ne.3)) then
          call xctila(covice,1,1,halo_ps) !Sea Ice Concentration
          call xctila(  si_c,1,1,halo_ps) !Sea Ice Concentration
          call xctila( si_tx,1,1,halo_pv) !Sea Ice X-Stress into ocean
          call xctila( si_ty,1,1,halo_pv) !Sea Ice Y-Stress into ocean
          call xctila(fswice,1,1,halo_ps) !Solar Heat Flux thru Ice to Ocean
          call xctila(flxice,1,1,halo_ps) !Ice Freezing/Melting Heat Flux
          call xctila(sflice,1,1,halo_ps) !Ice Salt Flux
          call xctila(wflice,1,1,halo_ps) !Ice Freshwater Flux
          call xctila(temice,1,1,halo_ps) !Sea Ice Temperature
          call xctila(  si_t,1,1,halo_ps) !Sea Ice Temperature
          call xctila(thkice,1,1,halo_ps) !Sea Ice Thickness
          call xctila(  si_h,1,1,halo_ps) !Sea Ice Thickness
          call xctila(  si_u,1,1,halo_pv) !Sea Ice X-Velocity
          call xctila(  si_v,1,1,halo_pv) !Sea Ice Y-Velocity
        elseif ((iceflg.ge.2).and.(icmflg.eq.3)) then
          call xctila(  si_c,1,1,halo_ps) !Sea Ice Concentration
          call xctila( si_tx,1,1,halo_pv) !Sea Ice X-Stress into ocean
          call xctila( si_ty,1,1,halo_pv) !Sea Ice Y-Stress into ocean
          call xctila(  si_h,1,1,halo_ps) !Sea Ice Thickness
          call xctila(  si_t,1,1,halo_ps) !Sea Ice Temperature
          call xctila(  si_u,1,1,halo_pv) !Sea Ice X-Velocity
          call xctila(  si_v,1,1,halo_pv) !Sea Ice Y-Velocity
        endif
#endif

!       smooth sea ice velocity fields
        call psmooth(si_u,0,0,ishlf,util1)
        call psmooth(si_v,0,0,ishlf,util1)
#if defined(ARCTIC)
        call xctila(si_u,1,1,halo_pv)
        call xctila(si_v,1,1,halo_pv)
#endif
!       call xctilr(si_u,1,1, nbdy,nbdy, halo_pv)
!       call xctilr(si_v,1,1, nbdy,nbdy, halo_pv)

!       copy back from si_ to _import for archive_ice
        do j=1, jja
        do i=1, ii
          if (imp_merge(i,j,l0)) then
            if (si_c(i,j).gt.0.0) then
              siu_import(i,j)=si_u(i,j) !Sea Ice X-Velocity
              siv_import(i,j)=si_v(i,j) !Sea Ice Y-Velocity
            endif !si_c
          endif
        enddo !i
        enddo !j
      endif !feedback from CICE to HYCOM
#endif
      end subroutine hycom_imp_mrg

      end module mod_import

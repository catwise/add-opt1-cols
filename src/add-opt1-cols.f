c-----------------------------------------------------------------------
c
c  Add-Opt1-Cols  -  Add the mdex columns appendeded by mrgad in option
c                    1 to the option-0 mdex table from WPHotpmc; fill
c                    columns with "null"
c
c  version 1.0  B80614: initial version
c          1.1  B80616: added sanity check and override switch "-f"
c
c-----------------------------------------------------------------------
c
                     Integer*4  MaxFld
                     Parameter (MaxFld = 1000)
c
      Character*5000 Line
      Character*500  InFNam, OutFNam
      Character*25   Field(MaxFld)
      Character*11   vsn
      Character*8    CDate, CTime, flag
      integer*4      nSrc, nArgs, nHead, IFa(MaxFld), IFb(MaxFld), NF
      logical*4      SanityChk
c
      Data Vsn/'1.1  B80616'/, nSrc/0/, nHead/0/, SanityChk/.true./
c
      Common / VDT / CDate, CTime, Vsn
c
c-----------------------------------------------------------------------
c
      NArgs = IArgC()
      If ((NArgs .ne. 2) .and. (NArgs .ne. 3)) then
        print *,'Add-Opt1-Cols vsn ', Vsn
        print *
        print *,'Usage: add-opt1-cols inputfile outputfile <-f>'
        print *
        print *,'The option-1 mdex columns added by mrgad will be'
        print *,'appended to the data rows of inputfile with null'
        print *,'values to generate outputfile.'
        print *
        print *,'If the "-f" (force) option is specified, the files'
        print *,'will be processed even if the input does not pass'
        print *,'the sanity check.'
        call exit(32)
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
      call signon('add-opt1-cols')
c
      call GetArg(1,InFNam)
      if (Access(InFNam(1:lnblnk(InFNam)),' ') .ne. 0) then
        print *,'File not found: ',InFNam(1:lnblnk(InFNam))
        call exit(64)
      end if
c
      call GetArg(2,OutFNam)
c
      if (Nargs .eq. 3) then
        call GetArg(3,flag)
        if ((flag .eq. '-f') .or. (flag .eq. '-F')) then
          SanityChk = .false.
        else
          print *,'ERROR: illegal specification: '//flag
          call exit(64)
        end if
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Create output table header
      open (10, file = InFNam)
c
      if (SanityChk) then
2       read (10,'(a)', end = 3000) Line
        if (Line(1:1) .eq. '\') go to 2
c
        call GetFlds(Line,Field,IFa,IFb,NF)
        if (NF .ne. 185) then
          print *,'ERROR: input is not a two-band wphot mdex file'
          print *,'       no. of fields =', NF,'; should be 165'
          call exit(64)
        end if
c                                          ! verify some fields in mdex file
        call ChkFld(Field(3),'ra',3)
        call ChkFld(Field(4),'dec',4)
        call ChkFld(Field(5),'sigra',5)
        call ChkFld(Field(6),'sigdec',6)
        call ChkFld(Field(7),'sigradec',7)
        rewind(10)
      end if
c
      open (20, file = OutFNam)
10    read (10, '(a)', end=1000) Line
      if (Line(1:1) .eq. '\') then
        write(20,'(a)') Line(1:lnblnk(Line))
        go to 10
      end if
      if (Line(1:1) .eq. '|') then
        nHead = nHead + 1
        if (nHead .eq. 1) write(20,'(a)') Line(1:lnblnk(Line))
     + //'  dist  |dw1mag|rch2w1|dw2mag|rch2w2|   elon   |  elonSig |'
     + //'   elat   | elatSig |   Delon  | DelonSig |   Delat  |'
     + //' DelatSig| DelonSNR | DelatSNR | chi2pmra|chi2pmdec|ka|k1|'
     + //'k2|km|m|  par_pm  | par_pmSig| par_stat | par_sigma|'
        if (nHead .eq. 2) write(20,'(a)') Line(1:lnblnk(Line))
     + //'  real  |   r  |   r  |   r  |   r  |    r     |     r    |'
     + //'     r    |    r    |    r     |     r    |     r    |'
     + //'    r    |    r     |     r    |    r    |     r   | i| i|'
     + //' i| i|c|    r     |     r    |    r     |     r    |'
        if (nHead .eq. 3) write(20,'(a)') Line(1:lnblnk(Line))
     + //'  asec  |  mag |   -  |  mag |   -  |   deg    |   asec   |'
     + //'    deg   |   asec  |   asec   |   asec   |   asec   |'
     + //'   asec  |     -    |     -    |    -    |    -    | -| -|'
     + //' -| -|-|   asec   |   asec   |   asec   |   asec   |'
        if (nHead .eq. 4) write(20,'(a)') Line(1:lnblnk(Line))
     + //'  null  | null | null | null | null |   null   |   null   |'
     + //'   null   |   null  |   null   |   null   |   null   |'
     + //'   null  |   null   |   null   |   null  |   null  | n| n|'
     + //' n| n|x|   null   |   null   |   null   |   null   |'
        go to 10
      end if
c
      nSrc = nSrc + 1
      write(20,'(a)') Line(1:lnblnk(Line))
     + //'   null    null   null   null   null     null       null    '
     + //'   null       null      null       null       null    '
     + //'   null      null       null       null      null    n  n '
     + //' n  n x    null       null       null       null   '
        go to 10
c
1000  print *,' No. data rows converted:', nSrc
      call signoff('add-opt1-cols')
      stop
c
3000  print *,'ERROR: end-of-file encountered during sanity check'
      call exit(64)
      stop
c
      end
c      
c=======================================================================
c
      subroutine SignOn(pgmnam)
c
c *** signon- routine which provides sign-on and sign-off messages
c             (orig by John Fowler- mod by Howard McCallon-041214-SIRTF)
c
c     inputs:  pgmnam = program name                                 [call arg]
c
c     outputs: message to stdout
c
      character*(*) pgmnam
      character vsn*11,cdate*8,ctime*8,Fmt*11,FLen*4
      integer*4 onoff,jdate(3),jtime(3),lnblnk
      real*4    dummyt,second(2),etime
c
      common /vdt/ cdate,ctime,vsn
c##
      onoff = 1
c
c         i. obtain date
c
100   cdate = '00-00-00'
      call idate(jdate)    ! Linux call
c
      jdate(3) = mod(jdate(3), 100)
      write(cdate(1:2), '(i2)') jdate(2)
      write(cdate(4:5), '(i2)') jdate(1)
      write(cdate(7:8), '(i2)') jdate(3)
c
      if(cdate(4:4) .eq. ' ') cdate(4:4) = '0'
      if(cdate(7:7) .eq. ' ') cdate(7:7) = '0'
c
c         ii. obtain time
c
      ctime = '00:00:00'
      call itime(jtime)
      write(ctime(1:2), '(i2)') jtime(1)
      write(ctime(4:5), '(i2)') jtime(2)
      write(ctime(7:8), '(i2)') jtime(3)
c
      if(ctime(4:4) .eq. ' ') ctime(4:4) = '0'
      if(ctime(7:7) .eq. ' ') ctime(7:7) = '0'
c
c         iii. set up format for pgmnam
c
      write(Flen,'(I4)') lnblnk(pgmnam)
      Fmt = '(A'//Flen//'$)'
c
c         iv. write out results
c
      write(*,Fmt) pgmnam
      if(onoff .eq. 1) then                      ! sign on
        write(*,301) vsn,cdate,ctime
      else                                       ! sign off
        dummyt = etime(second)
        write(*,302) vsn,cdate,ctime,second
      endif
  301 format(' version: ',a11,' - execution begun on ',a8,' at ',a8)
  302 format(' version: ',a11,' - execution ended on ',a8,' at ',a8
     *    /1x,f9.2,' cpu seconds used;',f8.2,' system seconds used.')
c
      return
c
      entry SignOff(pgmnam)
      OnOff = 2
      go to 100
c
      end
c
c=======================================================================
c
      subroutine ChkFld(Fld1, Fld2, k)
c      
      character*(*) Fld1, Fld2
      integer*4     k, lnblnk      
c
      if (Fld1 .ne. Fld2) then
        print *,'ERROR: input field no.',k,' expected to be ',
     +           Fld2(1:lnblnk(Fld2)),'; got ',Fld1(1:lnblnk(Fld2))
        call exit(64)
      end if
c      
      return
c      
      end
c      
c=======================================================================
c
      subroutine GetFlds(ColNam,Field,IFa,IFb,NF)
c-----------------------------------------------------------------------
c
c  Get fields in a table-file header line
c
c-----------------------------------------------------------------------
                     Integer*4  MaxFld
                     Parameter (MaxFld = 1000)
c
      character*5000 ColNam
      Character*300  Line
      character*25   Field(MaxFld)
      integer*4      IFa(MaxFld), IFb(MaxFld), NF, N, M, L, K, LNBlnk,
     +               LastErr
c
c-----------------------------------------------------------------------
c
      N = 0
      K = 0
      LastErr = 0
      do 100 M = 1, LNBlnk(ColNam)
        if (ColNam(M:M) .eq. '|') then
          N = N + 1
          NF = N - 1
          if (N .gt. 1) IFb(N-1) = M-1
          if (N .gt. MaxFld) return
          IFa(N) = M
          do 10 L = 1, 25
            Field(N)(L:L) = ' '
10        continue
          K = 0
        else
          if (ColNam(M:M) .ne. ' ') then
            K = K + 1
            if (K .le. 25) then
              Field(N)(K:K) = ColNam(M:M)
            else
              if (LastErr .ne. N) then
                write(Line,*) N
                Line = 'GetFlds - Table column name no. '
     +               //Line(1:lnblnk(Line))//' longer than 25 '
     +               //'characters: '//Field(N)//'....; excess ignored'
                print *,Line(1:lnblnk(line))
                LastErr = N
              end if
            end if
          end if
        end if
100   continue
c
      return
      end

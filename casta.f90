! cast  - console application screen tools
! casta - console application screen tools a type
! 12.10.2025


module casta_scr

    implicit none
    integer, parameter, private :: f1_def=100, f2_def=35, f3_def=30
    integer, parameter, private :: f1_min=30,  f2_min=25, f3_min=16
    integer, parameter, private :: f1_max=165, f2_max=50, f3_max=40

    type casta_scr_topt
        integer :: field1=f1_def,field2=f2_def,field3=f3_def
    end type casta_scr_topt

    type(casta_scr_topt), protected :: casta_scr_opt

    public casta_scr_setfields,&
           casta_scr_linestart,casta_scr_linereturn,&
           casta_scr_textwof,casta_scr_textfar,casta_scr_textfal,&
           casta_scr_text,&
           casta_scr_textfar1,casta_scr_textfar2,casta_scr_textfar3,&
           casta_scr_textfal1,casta_scr_textfal2,casta_scr_textfal3,&
           casta_scr_linesymbolfields,casta_scr_textblancfields

    contains

    subroutine casta_scr_setfields(field1,field2,field3)
        integer, intent(in), optional :: field1,field2,field3
        logical :: f1,f2,f3
        f1=present(field1)
        f2=present(field2)
        f3=present(field3)
        if ((.not.f1).and.(.not.f2).and.(.not.f3)) then
            casta_scr_opt%field1=f1_def
            casta_scr_opt%field2=f2_def
            casta_scr_opt%field3=f3_def
        else
            if (f1) call setfield(field1,f1_min,f1_max,casta_scr_opt%field1)
            if (f2) call setfield(field2,f2_min,f2_max,casta_scr_opt%field2)
            if (f3) call setfield(field3,f3_min,f3_max,casta_scr_opt%field3)
        end if
        contains
        subroutine setfield(fv,fmin,fmax,f)
            integer, intent(in) :: fv,fmin,fmax
            integer, intent(out) :: f
            if (fv<fmin) then
                f=fmin
            elseif (fv>fmax) then
                f=fmax
            else
                f=fv
            endif
        end subroutine setfield
    end subroutine casta_scr_setfields

    subroutine casta_scr_linestart()
        write(*,'(1X)',advance='no')
    end subroutine casta_scr_linestart

    subroutine casta_scr_linereturn()
        write(*,*)
    end subroutine casta_scr_linereturn

    subroutine casta_scr_textwof(text,rep)
        character(len=*), intent(in) :: text
        integer,intent(in),optional :: rep
        integer:: i
        if (present(rep)) then
            do i=1,rep
                write(*,'(A)',advance='no') text
            end do
        else
            write(*,'(A)',advance='no') text(1:len_trim(text))
        end if
    end subroutine casta_scr_textwof

    subroutine casta_scr_textfar(text,field,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        integer,intent(in) :: field
        character(len=1) :: bl
        integer :: lentext
        if (present(blanc)) then
            bl=blanc
        else
            bl=' '
        end if
        lentext=len_trim(text)
        if (field <= lentext) then
            call casta_scr_textwof(text(1:field),rep=1)
        else
            call casta_scr_textwof(bl,rep=field-lentext)
            call casta_scr_textwof(text(1:lentext),rep=1)
        end if
    end subroutine casta_scr_textfar

    subroutine casta_scr_textfal(text,field,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        integer,intent(in) :: field
        character(len=1) :: bl
        integer :: lentext
        if (present(blanc)) then
            bl=blanc
        else
            bl=' '
        end if
        lentext=len_trim(text)
        if (field <= lentext) then
            call casta_scr_textwof(text(1:field),rep=1)
        else
            call casta_scr_textwof(text(1:lentext),rep=1)
            call casta_scr_textwof(bl,rep=field-lentext)
        end if
    end subroutine casta_scr_textfal

    subroutine casta_scr_text(text,rep,field,align,blanc,text1,align1,blanc1,text2,align2,blanc2,text3,align3,blanc3,newline)
        character(len=*),intent(in),optional :: text,text1,text2,text3
        integer,intent(in),optional :: rep,field
        character(len=*),intent(in),optional :: blanc,blanc1,blanc2,blanc3,align,align1,align2,align3,newline
        if (present(newline)) then
            if ((newline(1:1)=='y').or.(newline(1:1)=='Y')) call casta_scr_linestart()
        end if
        if (present(text)) then
            if (present(rep)) then
                call casta_scr_textwof(text,rep)
            elseif (present(field)) then
                if (present(align)) then
                    if ((align(1:1)=='r').or.(align(1:1)=='R')) then
                        if (present(blanc)) then
                            call casta_scr_textfar(text,field,blanc)
                        else
                            call casta_scr_textfar(text,field)
                        end if
                    else
                        if (present(blanc)) then
                            call casta_scr_textfal(text,field,blanc)
                        else
                            call casta_scr_textfal(text,field)
                        end if
                    end if
                else
                    if (present(blanc)) then
                        call casta_scr_textfal(text,field,blanc)
                    else
                        call casta_scr_textfal(text,field)
                    end if
                end if
            else
                call casta_scr_textwof(text)
            end if
        else
            if (present(text1)) then
                if (present(align1)) then
                    if ((align1(1:1)=='r').or.(align1(1:1)=='R')) then
                        if (present(blanc1)) then
                            call casta_scr_textfar(text1,casta_scr_opt%field1,blanc1)
                        else
                            call casta_scr_textfar(text1,casta_scr_opt%field1)
                        end if
                    else
                        if (present(blanc1)) then
                            call casta_scr_textfal(text1,casta_scr_opt%field1,blanc1)
                        else
                            call casta_scr_textfal(text1,casta_scr_opt%field1)
                        end if
                    end if
                else
                    if (present(blanc1)) then
                        call casta_scr_textfal(text1,casta_scr_opt%field1,blanc1)
                    else
                        call casta_scr_textfal(text1,casta_scr_opt%field1)
                    end if
                end if
                if (present(text2)) call casta_scr_textwof(text=' ',rep=1)
            end if
            if (present(text2)) then
                if (present(align2)) then
                    if ((align2(1:1)=='r').or.(align2(1:1)=='R')) then
                        if (present(blanc2)) then
                            call casta_scr_textfar(text2,casta_scr_opt%field2,blanc2)
                        else
                            call casta_scr_textfar(text2,casta_scr_opt%field2)
                        end if
                    else
                        if (present(blanc2)) then
                            call casta_scr_textfal(text2,casta_scr_opt%field2,blanc2)
                        else
                            call casta_scr_textfal(text2,casta_scr_opt%field2)
                        end if
                    end if
                else
                    if (present(blanc2)) then
                        call casta_scr_textfal(text2,casta_scr_opt%field2,blanc2)
                    else
                        call casta_scr_textfal(text2,casta_scr_opt%field2)
                    end if
                end if
                if (present(text3)) call casta_scr_textwof(text=' ',rep=1)
            end if
            if (present(text3)) then
                if (present(align3)) then
                    if ((align3(1:1)=='r').or.(align3(1:1)=='R')) then
                        if (present(blanc3)) then
                            call casta_scr_textfar(text3,casta_scr_opt%field3,blanc3)
                        else
                            call casta_scr_textfar(text3,casta_scr_opt%field3)
                        end if
                    else
                        if (present(blanc3)) then
                            call casta_scr_textfal(text3,casta_scr_opt%field3,blanc3)
                        else
                            call casta_scr_textfal(text3,casta_scr_opt%field3)
                        end if
                    end if
                else
                    if (present(blanc3)) then
                        call casta_scr_textfal(text3,casta_scr_opt%field3,blanc3)
                    else
                        call casta_scr_textfal(text3,casta_scr_opt%field3)
                    end if
                end if
            end if
        end if
        if (present(newline)) then
            if ((newline(1:1)=='y').or.(newline(1:1)=='Y')) call casta_scr_linereturn()
        end if
    end subroutine casta_scr_text





    subroutine casta_scr_textfar1(text,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        if (present(blanc)) then
            call casta_scr_textfar(text=text,field=casta_scr_opt%field1,blanc=blanc)
        else
            call casta_scr_textfar(text=text,field=casta_scr_opt%field1)
        end if
    end subroutine casta_scr_textfar1

    subroutine casta_scr_textfar2(text,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        if (present(blanc)) then
            call casta_scr_textfar(text=text,field=casta_scr_opt%field2,blanc=blanc)
        else
            call casta_scr_textfar(text=text,field=casta_scr_opt%field2)
        end if
    end subroutine casta_scr_textfar2

    subroutine casta_scr_textfar3(text,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        if (present(blanc)) then
            call casta_scr_textfar(text=text,field=casta_scr_opt%field3,blanc=blanc)
        else
            call casta_scr_textfar(text=text,field=casta_scr_opt%field3)
        end if
    end subroutine casta_scr_textfar3

    subroutine casta_scr_textfal1(text,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        if (present(blanc)) then
            call casta_scr_textfal(text=text,field=casta_scr_opt%field1,blanc=blanc)
        else
            call casta_scr_textfal(text=text,field=casta_scr_opt%field1)
        end if
    end subroutine casta_scr_textfal1

    subroutine casta_scr_textfal2(text,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        if (present(blanc)) then
            call casta_scr_textfal(text=text,field=casta_scr_opt%field2,blanc=blanc)
        else
            call casta_scr_textfal(text=text,field=casta_scr_opt%field2)
        end if
    end subroutine casta_scr_textfal2

    subroutine casta_scr_textfal3(text,blanc)
        character(len=*), intent(in) :: text
        character(len=1), intent(in),optional :: blanc
        if (present(blanc)) then
            call casta_scr_textfal(text=text,field=casta_scr_opt%field3,blanc=blanc)
        else
            call casta_scr_textfal(text=text,field=casta_scr_opt%field3)
        end if
    end subroutine casta_scr_textfal3

    subroutine casta_scr_linesymbolfields(symb1,symb2,symb3)
        character(len=1),intent(in),optional :: symb1,symb2,symb3
        call casta_scr_linestart()
        if (present(symb1)) then
            call casta_scr_textwof(text=symb1,rep=casta_scr_opt%field1)
        else
            call casta_scr_textwof(text=' ',rep=casta_scr_opt%field1)
        end if
        call casta_scr_textwof(text=' ',rep=1)
        if (present(symb2)) then
            call casta_scr_textwof(text=symb2,rep=casta_scr_opt%field2)
        else
            call casta_scr_textwof(text=' ',rep=casta_scr_opt%field2)
        end if
        call casta_scr_textwof(text=' ',rep=1)
        if (present(symb3)) then
            call casta_scr_textwof(text=symb3,rep=casta_scr_opt%field3)
        else
            call casta_scr_textwof(text=' ',rep=casta_scr_opt%field3)
        end if
        call casta_scr_linereturn()
    end subroutine casta_scr_linesymbolfields

    subroutine casta_scr_textblancfields(text1,text2,text3,&
                                         blanc1,blanc2,blanc3,&
                                         align1,align2,align3)
        character(len=1), intent(in),optional :: align1,align2,align3
        character(len=*),intent(in),optional :: text1,text2,text3
        character(len=1),intent(in),optional :: blanc1,blanc2,blanc3
        logical :: r1,r2,r3
        character(len=255) :: t
        character(len=1) :: b
        if (present(align1)) then
            r1=(align1=='r').or.(align1=='R')
        else
            r1=.false.
        end if
        if (present(align2)) then
            r2=(align2=='r').or.(align2=='R')
        else
            r2=.false.
        end if
        if (present(align3)) then
            r3=(align3=='r').or.(align3=='R')
        else
            r3=.false.
        end if
        call casta_scr_linestart()
        if (present(text1)) then
            t=text1
        else
            t=''
        end if
        if (present(blanc1)) then
            b=blanc1
        else
            b=' '
        end if
        if (r1) then
            call casta_scr_textfar1(text=t,blanc=b)
        else
            call casta_scr_textfal1(text=t,blanc=b)
        end if
        call casta_scr_textwof(text=' ',rep=1)
        if (present(text2)) then
            t=text2
        else
            t=''
        end if
        if (present(blanc2)) then
            b=blanc2
        else
            b=' '
        end if
        if (r2) then
            call casta_scr_textfar2(text=t,blanc=b)
        else
            call casta_scr_textfal2(text=t,blanc=b)
        end if
        call casta_scr_textwof(text=' ',rep=1)
        if (present(text3)) then
            t=text3
        else
            t=''
        end if
        if (present(blanc3)) then
            b=blanc3
        else
            b=' '
        end if
        if (r3) then
            call casta_scr_textfar3(text=t,blanc=b)
        else
            call casta_scr_textfal3(text=t,blanc=b)
        end if
        call casta_scr_linereturn()
    end subroutine casta_scr_textblancfields

end module casta_scr



module casta_appl

    contains

    subroutine casta_appl_entry(name,curdate,curtime,comline,curwdir,descr)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields
        character(len=*),intent(in),optional :: name,curdate,curtime,comline,curwdir,descr
        call insline(symb1=' ',symb2='>',symb3='>')
        call insline(symb1=' ',symb2='>',symb3='>')
        call instext(blanc1='>',&
                     text2='APPLICATION',text3='ENTRY POINT',align2='r',align3='r')
        call insline(symb1='>',symb2='>',symb3='>')
        if (present(name)) call instext(blanc1='>',&
                                        text2='NAME',text3=name,align2='r',align3='r')
        if (present(curdate)) call instext(blanc1='>',&
                                           text2='CURRENT DATE',text3=curdate,align2='r',align3='r')
        if (present(curtime)) call instext(blanc1='>',&
                                           text2='STARTING TIME',text3=curtime,align2='r',align3='r')
        if (present(comline)) call instext(blanc3='>',&
                                           text1=comline,text2='COMMAND LINE',align1='r',align2='r')
        if (present(curwdir)) call instext(blanc3='>',&
                                           text1=curwdir,text2='CURRENT WORKING DIRECTORY',&
                                           align1='r',align2='r')
        if (present(descr)) call instext(blanc3='>',&
                                         text1=descr,text2='DESCRITPION',align1='r',align2='r')
        call insline(symb1='>',symb2='>',symb3='>')
        call insline(symb1=' ',symb2='>',symb3='>')
        call insline(symb1=' ',symb2='>',symb3='>')
    end subroutine casta_appl_entry

    subroutine casta_appl_exit(res,curdate,curtime,exetime)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields
        character(len=*),intent(in),optional :: res,curdate,curtime,exetime
        call insline(symb1=' ',symb2='>',symb3='>')
        call insline(symb1=' ',symb2='>',symb3='>')
        call insline(symb1='>',symb2='>',symb3='>')
        if (present(res)) call instext(blanc3='>',&
                                       text1=res,text2='RESULT',align1='r',align2='r')
        if (present(curdate)) call instext(blanc1='>',&
                                           text2='CURRENT DATE',text3=curdate,align2='r',align3='r')
        if (present(curtime)) call instext(blanc1='>',&
                                           text2='FINAL TIME',text3=curtime,align2='r',align3='r')
        if (present(exetime)) call instext(blanc1='>',&
                                           text2='EXECUTING TIME',text3=exetime,align2='r',align3='r')
        call insline(symb1='>',symb2='>',symb3='>')
        call instext(blanc1='>',&
                     text2='APPLICATION',text3='EXIT POINT',align2='r',align3='r')
        call insline(symb1=' ',symb2='>',symb3='>')
        call insline(symb1=' ',symb2='>',symb3='>')
    end subroutine casta_appl_exit

    subroutine casta_appl_message(mes1,mes2,mes3)
        use casta_scr, only: casta_scr_textblancfields
        character(len=*),intent(in),optional :: mes1,mes2,mes3
        character(len=255) :: m1,m2,m3
        if (present(mes1)) then
            m1=mes1
        else
            m1=''
        end if
        if (present(mes2)) then
            m2=mes2
        else
            m2=''
        end if
        if (present(mes3)) then
            m3=mes3
        else
            m3=''
        end if
        call casta_scr_textblancfields(text1=m1,text2=m2,text3=m3,align1='r',align2='r',align3='r')
    end subroutine casta_appl_message

    subroutine casta_appl_finalpause()
        use casta_scr,only:casta_scr_opt,&
                           casta_scr_linestart,casta_scr_linereturn,&
                           casta_scr_textwof,casta_scr_textfar
        call casta_scr_linestart()
        call casta_scr_textwof(text=' ',rep=casta_scr_opt%field1)
        call casta_scr_textwof(text=' ',rep=1)
        call casta_scr_textwof(text='>',rep=casta_scr_opt%field2+casta_scr_opt%field3+1)
        call casta_scr_linereturn()
        call casta_scr_linestart()
        call casta_scr_textwof(text=' ',rep=casta_scr_opt%field1)
        call casta_scr_textwof(text=' ',rep=1)
        call casta_scr_textfar(text='PRESS ENTER TO EXIT APPLICATION FINALLY',&
                               field=casta_scr_opt%field2+casta_scr_opt%field3,&
                               blanc='>')
        read(*,*)
        call casta_scr_linereturn()
    end subroutine casta_appl_finalpause

end module casta_appl



module casta_job

    contains

    subroutine casta_job_entry(name,curdate,curtime,descr)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields
        character(len=*),intent(in),optional :: name,curdate,curtime,descr
        call insline(symb1=' ',symb2='>',symb3='>')
        call instext(blanc1='>',&
                     text2='JOB',text3='ENTRY POINT',align2='r',align3='r')
        if (present(name)) call instext(blanc1='=',&
                                        text2='NAME',text3=name,align2='r',align3='r')
        if (present(curdate)) call instext(blanc1='=',&
                                           text2='CURRENT DATE',text3=curdate,align2='r',align3='r')
        if (present(curtime)) call instext(blanc1='=',&
                                           text2='STARTING TIME',text3=curtime,align2='r',align3='r')
        if (present(descr)) call instext(blanc3='=',&
                                         text1=descr,text2='DESCRITPION',align1='r',align2='r')
        call insline(symb1='>',symb2='>',symb3='>')
        call insline(symb1=' ',symb2='>',symb3='>')
    end subroutine casta_job_entry

    subroutine casta_job_exit(res,curdate,curtime,exetime)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields
        character(len=*),intent(in),optional :: res,curdate,curtime,exetime
        call insline(symb1=' ',symb2='>',symb3='>')
        call insline(symb1='>',symb2='>',symb3='>')
        if (present(res)) call instext(blanc3='=',&
                                       text1=res,text2='RESULT',align1='r',align2='r')
        if (present(curdate)) call instext(blanc1='=',&
                                           text2='CURRENT DATE',text3=curdate,align2='r',align3='r')
        if (present(curtime)) call instext(blanc1='=',&
                                           text2='FINAL TIME',text3=curtime,align2='r',align3='r')
        if (present(exetime)) call instext(blanc1='=',&
                                           text2='EXECUTING TIME',text3=exetime,align2='r',align3='r')
        call instext(blanc1='>',&
                     text2='JOB',text3='EXIT POINT',align2='r',align3='r')
        call insline(symb1=' ',symb2='>',symb3='>')
    end subroutine casta_job_exit

    subroutine casta_job_message(mes1,mes2,mes3)
        use casta_scr, only: casta_scr_textblancfields
        character(len=*),intent(in),optional :: mes1,mes2,mes3
        character(len=255) :: m1,m2,m3
        if (present(mes1)) then
            m1=mes1
        else
            m1=''
        end if
        if (present(mes2)) then
            m2=mes2
        else
            m2=''
        end if
        if (present(mes3)) then
            m3=mes3
        else
            m3=''
        end if
        call casta_scr_textblancfields(text1=m1,text2=m2,text3=m3,align1='r',align2='l',align3='r')
    end subroutine casta_job_message

end module casta_job



module casta_task

    contains

    subroutine casta_task_entry(name,curdate,curtime,descr)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields
        character(len=*),intent(in),optional :: name,curdate,curtime,descr
        call insline(symb1=' ',symb2='=',symb3='=')
        call instext(blanc1='=',&
                     text2='TASK',text3='ENTRY POINT',align2='r',align3='r')
        if (present(name)) call instext(blanc1=':',&
                                        text2='NAME',text3=name,align2='r',align3='r')
        if (present(curdate)) call instext(blanc1=':',&
                                           text2='CURRENT DATE',text3=curdate,align2='r',align3='r')
        if (present(curtime)) call instext(blanc1=':',&
                                           text2='STARTING TIME',text3=curtime,align2='r',align3='r')
        if (present(descr)) call instext(blanc3=':',&
                                         text1=descr,text2='DESCRITPION',align1='r',align2='r')
        call insline(symb1='=',symb2='=',symb3='=')
    end subroutine casta_task_entry

    subroutine casta_task_exit(res,curdate,curtime,exetime)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields
        character(len=*),intent(in),optional :: res,curdate,curtime,exetime
        call insline(symb1='=',symb2='=',symb3='=')
        if (present(res)) call instext(blanc3=':',&
                                       text1=res,text2='RESULT',align1='r',align2='r')
        if (present(curdate)) call instext(blanc1=':',&
                                           text2='CURRENT DATE',text3=curdate,align2='r',align3='r')
        if (present(curtime)) call instext(blanc1=':',&
                                           text2='FINAL TIME',text3=curtime,align2='r',align3='r')
        if (present(exetime)) call instext(blanc1=':',&
                                           text2='EXECUTING TIME',text3=exetime,align2='r',align3='r')
        call instext(blanc1='=',&
                     text2='TASK',text3='EXIT POINT',align2='r',align3='r')
        call insline(symb1=' ',symb2='=',symb3='=')
    end subroutine casta_task_exit

    subroutine casta_task_message(mes1,mes2,mes3)
        use casta_scr, only: casta_scr_textblancfields
        character(len=*),intent(in),optional :: mes1,mes2,mes3
        character(len=255) :: m1,m2,m3
        if (present(mes1)) then
            m1=mes1
        else
            m1=''
        end if
        if (present(mes2)) then
            m2=mes2
        else
            m2=''
        end if
        if (present(mes3)) then
            m3=mes3
        else
            m3=''
        end if
        call casta_scr_textblancfields(text1=m1,text2=m2,text3=m3,align1='r',align2='l',align3='l')
    end subroutine casta_task_message

end module casta_task



module casta_action

    contains

    subroutine casta_action_entry(descr,curtime)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields,&
                            linestart => casta_scr_linestart,&
                            linereturn => casta_scr_linereturn,&
                            instextwof => casta_scr_textwof
        character(len=*),intent(in) :: descr
        character(len=*),intent(in),optional :: curtime
        character(len=255) :: text
        if (present(curtime)) then
            call insline(symb1=' ',symb2=':',symb3=' ')
            call instext(text1='ACTION ENTRY',text2='',text3='',align1='r',blanc2=':',blanc3=':')
            if (curtime.ne.'') call instext(text1='CURRENT TIME',text2=curtime,text3='',align1='r')
        end if
        call linestart()
        call instextwof(text=':',rep=19)
        call linereturn()
        write(unit=text,fmt=10) descr
        call linestart()
        call instextwof(text=text)
        call linereturn()
        10 format(':::::::: RUNNING:',1X,A)
    end subroutine casta_action_entry

    subroutine casta_action_execentry()
       use casta_scr,only:  linestart => casta_scr_linestart,&
                            instextwof => casta_scr_textwof
       call linestart()
       call instextwof(text=' ',rep=9)
       call instextwof(text=':',rep=8)
       call instextwof(text=' ',rep=1)
       call instextwof(text='EXECUTING')
    end subroutine casta_action_execentry

    subroutine casta_action_execute(percentage)
        real,intent(in) :: percentage
        write(unit=*,fmt=10,advance='no') char(13),percentage
        10 format(A,1X,F7.3,'%')
    end subroutine casta_action_execute

    subroutine casta_action_execexit()
        use casta_scr,only: linereturn => casta_scr_linereturn
        call linereturn()
    end subroutine casta_action_execexit

    subroutine casta_action_comment(text)
        use casta_scr,only: opt => casta_scr_opt,&
                            linestart => casta_scr_linestart,&
                            linereturn => casta_scr_linereturn,&
                            instextwof => casta_scr_textwof
        character(len=*),intent(in),optional :: text
        if (present(text)) then
            call linestart()
            call instextwof(text=' ',rep=18)
            call instextwof(text=text)
            call linereturn()
        else
            call linestart()
            call instextwof(text=':',rep=opt%field1+opt%field2+opt%field3+2)
            call linereturn()
        end if
    end subroutine casta_action_comment

    subroutine casta_action_exit(res,curtime,exectime)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields,&
                            linestart => casta_scr_linestart,&
                            linereturn => casta_scr_linereturn,&
                            instextwof => casta_scr_textwof
        character(len=*),intent(in) :: res
        character(len=*),intent(in),optional :: curtime,exectime
        character(len=255) :: text
        call linestart()
        call instextwof(text=' ',rep=18)
        call instextwof(text=':',rep=9)
        call linereturn()
        write(unit=text,fmt=10) res
        call linestart()
        call instextwof(text=text)
        call linereturn()
        call linestart()
        call instextwof(text=':',rep=25)
        call linereturn()
        if (present(curtime)) call instext(text1='CURRENT TIME',text2=curtime,text3='',align1='r')
        if (present(exectime)) call instext(text1='EXECUTION TIME',text2=exectime,text3='',align1='r')
        if (present(curtime).or.present(exectime)) then
            call instext(text1='ACTION EXIT',text2='',text3='',align1='r',blanc2=':',blanc3=':')
            call insline(symb1=' ',symb2=':',symb3=' ')
        end if
        10 format(17X,' RESULT:',1X,A)
    end subroutine casta_action_exit

end module casta_action



module casta_ioroutines
    contains

    subroutine casta_ioroutines_inputentry(res,inpname,inpfmt,posval,defval)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            linestart => casta_scr_linestart,&
                            linereturn => casta_scr_linereturn,&
                            instextwof => casta_scr_textwof,&
                            instextfal1 => casta_scr_textfal1,&
                            instextfal2 => casta_scr_textfal2,&
                            instextfal3 => casta_scr_textfal3,&
                            instextfar1 => casta_scr_textfar1,&
                            instextfar2 => casta_scr_textfar2
        character(len=*),intent(out) :: res
        character(len=*),intent(in),optional :: inpname,inpfmt,posval,defval
        call insline(symb1=' ',symb2='-',symb3='-')
        call linestart()
        call instextfal1(text='')
        call instextwof(text=' ',rep=1)
        call instextfar2(text='TYPE THE VALUE')
        call instextwof(text=' ',rep=1)
        call instextfal3(text='AND PRESS ENTER')
        call linereturn()
        if (present(inpfmt)) then
            call linestart()
            call instextfal1(text='')
            call instextwof(text=' ',rep=1)
            call instextfar2(text='FORMAT')
            call instextwof(text=' ',rep=1)
            call instextfal3(text=inpfmt)
            call linereturn()
        end if
        if (present(posval)) then
            call linestart()
            call instextfal1(text='')
            call instextwof(text=' ',rep=1)
            call instextfar2(text='POSSIBLE VALUE')
            call instextwof(text=' ',rep=1)
            call instextfal3(text=posval)
            call linereturn()
        end if
        if (present(defval)) then
            call linestart()
            call instextfal1(text='')
            call instextwof(text=' ',rep=1)
            call instextfar2(text='OR JUST PRESS')
            call instextwof(text=' ',rep=1)
            call instextfal3(text='ENTER TO ASSIGN')
            call linereturn()
            call linestart()
            call instextfar1(text='DEFAULT VALUE =')
            call instextwof(text=' ',rep=1)
            call instextfal2(text=defval)
            call linereturn()
        end if
        call insline(symb1=' ',symb2='.',symb3=' ')
        call linestart()
        if (present(inpname)) then
            call instextfar1(text=inpname//':',blanc='.')
        else
            call instextfar1(text='',blanc='.')
        end if
        call instextwof(text=' ',rep=1)
        read(unit=*,fmt='(A)') res
        if (present(defval).and.(res.eq.'')) res=defval
    end subroutine casta_ioroutines_inputentry

    subroutine casta_ioroutines_inputcomment(text)
        use casta_scr,only: opt => casta_scr_opt,&
                            linestart => casta_scr_linestart,&
                            linereturn => casta_scr_linereturn,&
                            instextwof => casta_scr_textwof
        character(len=*),intent(in) :: text
        call linestart()
        call instextwof(text=' ',rep=opt%field1)
        call instextwof(text=' ',rep=1)
        call instextwof(text=text)
        call linereturn()
    end subroutine casta_ioroutines_inputcomment

    subroutine casta_ioroutines_inputexit(res)
        use casta_scr,only: insline => casta_scr_linesymbolfields,&
                            linestart => casta_scr_linestart,&
                            linereturn => casta_scr_linereturn,&
                            instextwof => casta_scr_textwof,&
                            instextfal2 => casta_scr_textfal2,&
                            instextfar1 => casta_scr_textfar1
        character(len=*),intent(in),optional :: res
        call insline(symb1=' ',symb2='.',symb3=' ')
        if (present(res)) then
            call linestart()
            call instextfar1(text='RESULT =')
            call instextwof(text=' ',rep=1)
            call instextfal2(text=res)
            call linereturn()
        end if
        call insline(symb1='-',symb2='-',symb3=' ')
    end subroutine casta_ioroutines_inputexit

    function casta_ioroutines_menu(items,caption,defitemnum) result(res)
        use casta_scr,only: opt => casta_scr_opt,&
                            insline => casta_scr_linesymbolfields,&
                            instext => casta_scr_textblancfields,&
                            linestart => casta_scr_linestart,&
                            linereturn => casta_scr_linereturn,&
                            instextwof => casta_scr_textwof,&
                            instextfal3 => casta_scr_textfal3,&
                            instextfar1 => casta_scr_textfar1
        integer :: res
        character(len=*),dimension(:),intent(in) :: items
        character(len=*),intent(in),optional :: caption
        integer,optional :: defitemnum
        integer :: counti,defi,i,ios
        character(len=2) :: cres
        character(len=255) :: text
        call insline(symb1=':',symb2=':',symb3=':')
        if (present(caption)) then
            call instext(text1=caption,blanc1=':',&
                         text2=' MENU',align2='r',&
                         blanc3=':')
        else
            call instext(text2=' MENU',align2='r',blanc1=':',blanc3=':',blanc2=':')
        end if
        counti=size(items)
        if (present(defitemnum)) then
            defi=defitemnum
            if ((defi.lt.0).or.(defi.gt.counti)) defi=-1
        else
            defi=-1
        end if
        100 call instext(text1='ITEMS',align1='r',blanc1=':',text2='-> NUMBERS',blanc2=':',blanc3=':')
        call printitem('EXIT MENU',0)
        do i=1,size(items)
            call printitem(items(i),i)
        end do
        write(unit=text,fmt=10) 0,counti
        call casta_ioroutines_inputentry(res=cres,inpname='NUMBER OF THE CHOSEN ITEM',posval=text)
        if ((defi.ge.0).and.(cres.eq.'')) then
            res=defi
            text='SUCCESSFUL INPUTTING'
            call casta_ioroutines_inputcomment(text=text)
            write(unit=text,fmt=20) res
            ios=0
        else
            read(unit=cres,fmt='(I2)',iostat=ios) res
            if (ios.ne.0) then
                call casta_ioroutines_inputcomment(text='INPUTED NUMBER IS NOT INTEGER')
                text='NOT DEFINED, TRY INPUTTING AGAIN'
            else
                if ((res.lt.0).or.(res.gt.counti)) then
                    write(unit=text,fmt=30) res,0,counti
                    call casta_ioroutines_inputcomment(text=text)
                    text='NOT DEFINED, TRY INPUTTING AGAIN'
                    ios=1
                else
                    text='SUCCESSFUL INPUTTING'
                    call casta_ioroutines_inputcomment(text=text)
                    write(unit=text,fmt=20) res
                end if
            end if
        end if
        call casta_ioroutines_inputexit(res=text)
        if (ios.ne.0) goto 100
        call insline(symb1=':',symb2=':',symb3=':')
        10 format('INTEGER BETWEEN',1X,I2,1X,'AND',1X,I2)
        20 format(I2)
        30 format('INPUTED NUMBER',1X,I2,1X,'IS NOT BETWEEN',1X,I2,1X,'AND',1X,I2)
        contains
        subroutine printitem(captionitem,numberitem)
            character(len=*),intent(in) :: captionitem
            integer,intent(in) :: numberitem
            character(len=5) :: inum
            write(unit=inum,fmt=10) numberitem
            if (defi.eq.numberitem) then
                call linestart()
                call instextfar1(text=captionitem,blanc='.')
                call instextwof(text=' ',rep=1)
                call instextwof(text=inum)
                call instextwof(text=' ',rep=1)
                call instextwof(text='-',rep=opt%field2-7)
                call instextwof(text='>')
                call instextwof(text=' ',rep=1)
                call instextfal3(text='DEFAULT')
                call linereturn()
            else
                call instext(text1=captionitem,align1='r',blanc1='.',text2=inum,blanc3=':')
            end if
            10 format('->',1X,I2)
        end subroutine printitem
    end function casta_ioroutines_menu


end module casta_ioroutines

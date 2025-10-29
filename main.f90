
program hello
    use casta_scr
    use casta_appl
    use casta_job
    use casta_task
    use casta_action
    implicit none

    print *, "Hello World!"

    call casta_scr_setfields(field2=25,field1=50,field3=20)
    print*,casta_scr_opt
    call casta_scr_setfields()
    print*,casta_scr_opt
    call casta_scr_setfields(field1=50)
    print*,casta_scr_opt
    call casta_scr_setfields(field2=25)
    print*,casta_scr_opt
    call casta_scr_setfields(field3=20)
    print*,casta_scr_opt
    call casta_scr_setfields(field1=5000,field2=250,field3=200)
    print*,casta_scr_opt

    call casta_scr_setfields()
    call casta_scr_linestart()
    call casta_scr_textwof(text='>',rep=casta_scr_opt%field1)
    call casta_scr_textwof(text=' ',rep=1)
    call casta_scr_textwof(text='>',rep=casta_scr_opt%field2)
    call casta_scr_textwof(text=' ',rep=1)
    call casta_scr_textwof(text='>',rep=casta_scr_opt%field3)
    call casta_scr_linereturn()

    call casta_scr_linestart()
    call casta_scr_textwof(text='>',rep=casta_scr_opt%field1)
    call casta_scr_textwof(text=' ',rep=1)
    call casta_scr_textfar(text='text',field=casta_scr_opt%field2,blanc='>')
    call casta_scr_textwof(text=' ',rep=1)
    call casta_scr_textfal(text='text',field=casta_scr_opt%field3,blanc='>')
    call casta_scr_linereturn()

    call casta_scr_linesymbolfields(symb1='>',symb2='=',symb3='-')
    call casta_scr_linesymbolfields(symb1='>')
    call casta_scr_linesymbolfields(symb2='=')
    call casta_scr_linesymbolfields(symb3='-')
    call casta_scr_linesymbolfields(symb1='>',symb2='=')
    call casta_scr_linesymbolfields(symb1='>',symb3='-')
    call casta_scr_linesymbolfields(symb2='=',symb3='-')
    call casta_scr_textblancfields(align1='r',align3='r',&
    text1='text1',text2='text2',text3='text3',blanc1='>',blanc2='=',blanc3='-')
    call casta_scr_textblancfields(align1='r',text1='text1',blanc2='>',blanc3='>')
    call casta_scr_textblancfields(align2='r',text1='text1',blanc2='>',blanc3='>')
    call casta_scr_textblancfields(align3='r',text1='text1',blanc2='>',blanc3='>')
    call casta_scr_textblancfields(&
    text1='text1',text2='text2',text3='text3',blanc1='>',blanc2='=',blanc3='-')

    !call casta_scr_setfields(field1=0,field2=0,field3=0)
    call casta_appl_entry(name='project',curdate='12.10.2025',curtime='16:23:35.123',&
                          comline='d:/erte/esrgerreg/ergtretgert/project',&
                          curwdir='d:/erte/esrgerreg/ergtretgert',&
                          descr='it is testing for casta application')
    call casta_appl_message(mes1='mes1',mes2='mes2',mes3='mes3')
    call casta_appl_message(mes1='mes1')
    call casta_appl_message(mes2='mes2')
    call casta_appl_message(mes3='mes3')
    call casta_appl_message(mes1='mes1',mes2='mes2')
    call casta_appl_message(mes1='mes1',mes3='mes3')
    call casta_appl_message(mes2='mes2',mes3='mes3')
    call casta_job_entry(name='preprocessor',curdate='12.10.2025',curtime='16:23:35.123',&
                         descr='it is testing for casta job')
    call casta_job_message(mes1='mes1',mes2='mes2',mes3='mes3')
    call casta_task_entry(name='inputting',curdate='12.10.2025',curtime='16:23:35.123',&
                         descr='it is testing for casta job')
    call casta_task_message(mes1='mes1',mes2='mes2',mes3='mes3')
    call casta_action_entry(descr='it is testing for casta action')
    !call casta_action_exec(part=1.0)
    call casta_action_exit(res='successful')
    !call casta_action_entry(curtime='16:23:35.123',descr='it is testing for casta action')
    !call casta_action_exec(part=0.55)
    !call casta_action_terminate()
    !call casta_action_exit(res='failure',curtime='16:23:45.234')
    !call casta_action_entry(curtime='16:23:35.123',descr='it is testing for casta action')
    !call casta_action_exec()
    !call casta_action_exit(res='failure',curtime='16:23:45.234')

    !call casta_action_exit(curdate='12.10.2025',curtime='16:23:35.123',exetime='00:02:45.234',res='successful')
    !call casta_action_entry(descr='it is testing for casta action')
    !call casta_action_exit(res='successful')
    call casta_task_exit(res='successful executed',curdate='12.10.2025',curtime='12:32:45.234',exetime='00:02:45.234')
    call casta_job_exit(res='successful executed',curdate='12.10.2025',curtime='12:32:45.234',exetime='00:02:45.234')
    call casta_appl_exit(res='successful executed',curdate='12.10.2025',curtime='12:32:45.234',exetime='00:02:45.234')
    call casta_appl_finalpause()







end program


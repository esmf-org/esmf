#                      -----------------                          
#                      Check Environment
#                      -----------------                          

   ifneq ($(shell $(ESMABIN)/Assert), 0)
      $(error Please correct your build environment and try again)
   endif

// the interface subroutine names MUST be in lower case by ESMF convention

  void c_mapl_locstreamretrieveptr_(void **ptr, long *addr) {
    *addr = (long)(*ptr);
  }

  void c_mapl_locstreamrestoreptr_(void **ptr, long *addr) {
    *ptr = (void *)(*addr);
  }



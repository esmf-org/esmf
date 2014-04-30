import ESMF

# Start up ESMF, this call is only necessary to override the default parameters
# for logkind (ESMF.LogKind.NONE) and debug (False)
esmpy = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

print "Hello ESMPy World from PET (processor) {0}!".format(ESMF.local_pet())

import ESMF

# Start up ESMF.
esmp = ESMF.Manager(logkind=ESMF.LogKind.SINGLE, debug=True)
pet_count = ESMF.pet_count()

print "Hello ESMPy World!"

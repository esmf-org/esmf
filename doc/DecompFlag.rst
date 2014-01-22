DecompFlag
========

This flag indicates how DistGrid elements are decomposed over DEs.

Values are:

        DEFAULT = 0
            Use default decomposition behavior. Currently equal to 'BALANCED'.
        BALANCED = 1
            Decompose elements as balanced as possible across DEs. The maximum 
            difference in number of elements per DE is 1, with the extra elements on
            the lower DEs.
        RESTFIRST = 2
            Divide elements over DEs. Assign the rest of this division to the first DE.
        RESTLAST = 3
            Divide elements over DEs. Assign the rest of this division to the last DE.
        CYCLIC = 4
            Decompose elements cyclically across DEs.

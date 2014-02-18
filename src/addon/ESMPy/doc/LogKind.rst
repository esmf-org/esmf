LogKind
=======

This flag is used to specify how much logging should be done.

Values are:

    SINGLE = 1
        Use a single log file, combining messages from all of the PETs.
        Not supported on some platforms.
    MULTI = 2
        Use multiple log files -- one per PET.
    NONE = 3
        Do not issue messages to a log file.

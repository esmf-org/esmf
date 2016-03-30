class Method (object):
    def __init__(self, func):
        self._function = func

    def __get__(self, obj, type):
        return self.AbstractMethodHelper(self._function, type)

    class AbstractMethodHelper (object):
        def __init__(self, func, cls):
            self._function = func
            self._class = cls

        def __call__(self, *args, **kwargs):
            raise NotImplementedError('Abstract method `' + self._class.__name__ \
                            + '.' + self._function + '\' called')

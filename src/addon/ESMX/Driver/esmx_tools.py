from pathlib import Path
import os
import sys
import yaml

def esmx_read_config(file_path: str):
    # open yaml file and read it
    if not os.path.exists(file_path):
        sys.exit('File not found: {}'.format(file_path))
    with open(file_path) as file:
        data = yaml.safe_load(file)
        if data is not None:
            return dict({k.replace("-", "_"): v for k, v in data.items()})
        else:
            return {}

class ESMXOpt():
    option: str
    default: str = None
    ctype: type = str
    def __init__(self, option: str, default: str, ctype: type):
        self.option = option
        self.default = default
        self.ctype = ctype
    def upper(self):
        return self.option.upper()

class ESMXAppCfg(dict):
    def __init__(self, args):
        if type(args) is dict:
            if "application" in args:
                super().__init__(args["application"])
            else:
                super().__init__({})
        elif type(args) is str:
            self.__init__(esmx_read_config(args))
        else:
            super().__init__(args)
    def list(self):
        return list(self.keys())

class ESMXCmpCfg(dict):
    def __init__(self, args):
        if type(args) is dict:
            if "components" in args:
                super().__init__(args["components"])
            else:
                super().__init__({})
        elif type(args) is str:
            self.__init__(esmx_read_config(args))
        else:
            super().__init__(args)
    def list(self):
        return list(self.keys())
    def remove_ci(self, comp):
        for key in list(self):
            if key.lower() == comp.lower():
                del self[key]
    def get_config(self, comp, files = None):
        if comp not in self:
            return {}
        elif type(self[comp]) is str:
            file_path = os.path.abspath(self[comp])
            if files is not None:
                if file_path in files:
                    sys.exit('Component configuration loop detected: {}'
                             .format(comp))
                else:
                    files.append(file_path)
            else:
                files = [file_path]
            return ESMXCmpCfg(file_path).get_config(comp, files)
        else:
            return self[comp]

class ESMXTstCfg(dict):
    def __init__(self, args):
        if type(args) is dict:
            if "tests" in args:
                super().__init__(args["tests"])
            else:
                super().__init__({})
        elif type(args) is str:
            self.__init__(esmx_read_config(args))
        else:
            super().__init__(args)
    def list(self):
        return list(self.keys())
    def get_config(self, test, files = None):
        if test not in self:
            return {}
        elif type(self[test]) is str:
            file_path = os.path.abspath(self[test])
            if files is not None:
                if file_path in files:
                    sys.exit('Test configuration loop detected: {}'
                             .format(test))
                else:
                    files.append(file_path)
            else:
                files = [file_path]
            return ESMXTstCfg(file_path).get_config(test, files)
        else:
            return self[test]

def esmx_write_tstcfg(tst: str, cfg: ESMXTstCfg, opts: [ESMXOpt], file):
    tstcfg = cfg.get_config(tst)
    for opt in opts:
        val = tstcfg.get(opt.option, opt.default)
        if (val):
            if (opt.ctype == dir):
                val = val.strip()
                if not val.startswith('$'):
                    val = os.path.abspath(val)
            file.write('set({}-{} {})\n'.format(tst, opt.upper(), val))

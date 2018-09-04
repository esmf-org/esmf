#ifndef ESMCI_COMPONENTINFO_H
#define ESMCI_COMPONENTINFO_H

using std::string;
using std::vector;
using std::stringstream;

namespace ESMCI {

  class ESMFId {
  public:
  ESMFId(int vmid, int baseid): _vmid(vmid), _baseid(baseid) {}
    
    bool operator==(const ESMFId &other) const {
      return _vmid == other._vmid && _baseid == other._baseid;
    }

    bool operator!=(const ESMFId &other) const {
      return !(*this == other);
    }
    
    unsigned long hashcode() const {
      return 5381 * (31*(_vmid+1)) * (13*(_baseid+1));
    }
    
  private:
    int _vmid;
    int _baseid;
    
  };
  
  class ESMFPhaseId {
  public:
  ESMFPhaseId():
    _esmfid(ESMFId(-1,-1)), _method(-1), _phase(-1) {}

  ESMFPhaseId(ESMFId esmfid, int method, int phase):
    _esmfid(esmfid), _method(method), _phase(phase) {}

    bool operator==(const ESMFPhaseId &other) const {
      return _esmfid == other._esmfid && _method == other._method &&
             _phase == other._phase;
    }

    bool operator!=(const ESMFPhaseId &other) const {
      return !(*this == other);
    }

    unsigned long hashcode() const {
      return _esmfid.hashcode() * (13*(_method+1)) + _phase;
    }

    ESMFId getESMFId() const {
      return _esmfid;
    }

    string getMethodType() const {
      if (_method == 0) return "Init";
      else if (_method == 1) return "Run";
      else if (_method == 2) return "Final";
      else return "UNKNOWN";
    }

    int getPhase() const {
      return _phase;
    }
    
  private:
    ESMFId _esmfid;
    int _method;
    int _phase;
  };

  /*
    Here we store some basic component info.  This is stored
    separately from the rest of the ESMF Component structures
    since Components may come and go during the life of
    the application, so we cannot really rely on them being around
    at the end to query for this information.
  */
  class ComponentInfo {
    
  public:
        
  ComponentInfo(ESMFId id):
    _id(id), _name("UNKNOWN") {}

  ComponentInfo(ESMFId id, string name):
    _id(id), _name(name) {}
    
    string getName() const {
      return _name;
    }
    
    void setName(string name) {
      _name = name;
    }

    string getPhaseName(ESMFPhaseId phaseId) const {
      for (unsigned int i = 0; i < _phaseIds.size(); i++) {
        if (_phaseIds.at(i) == phaseId) {
          return "[" + _name + "] " + _phaseNames.at(i);
        }
      }
      stringstream ss;
      ss <<  "[" << _name << "] " << phaseId.getMethodType() << " " << phaseId.getPhase();
      return ss.str();
    }

    void setPhaseName(ESMFPhaseId phaseId, string name) {
      for (unsigned int i = 0; i < _phaseIds.size(); i++) {
        if (_phaseIds.at(i) == phaseId) {
          _phaseNames.at(i) = name;
          return;
        }
      }
      _phaseIds.push_back(phaseId);
      _phaseNames.push_back(name);
    }
    
  private:
    ESMFId _id;
    std::string _name;
    vector<ESMFPhaseId> _phaseIds;
    vector<string> _phaseNames;
  };

}

#endif


/**
 * Copyright 2017 HashMap Development Team
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#ifndef ESMCI_HASHMAP_H
#define ESMCI_HASHMAP_H

#include "ESMCI_HashNode.h"
#include "ESMCI_KeyHash.h"
#include <cstddef>
#include <vector>

using std::vector;

namespace ESMCI {
  // Hash map class template
  template <typename K, typename V, size_t tableSize, typename F = KeyHash<K, tableSize> >
    class HashMap
    {
    public:
    HashMap() :
    table(),
    hashFunc()
    {
    }
    
    ~HashMap()
    {
      // destroy all buckets one by one
      for (size_t i = 0; i < tableSize; ++i) {
        HashNode<K, V> *entry = table[i];

        while (entry != NULL) {
          HashNode<K, V> *prev = entry;
          entry = entry->getNext();
          delete prev;
        }

        table[i] = NULL;
      }
    }

    vector<HashNode<K, V> *> getEntries() const {
      vector<HashNode<K, V> *> entries;
      for (size_t i = 0; i < tableSize; ++i) {
        HashNode<K, V> *entry = table[i];
        while (entry != NULL) {
          entries.push_back(entry);
          entry = entry->getNext();
        }
      }
      return entries;
    }
    
    bool get(const K &key, V &value)
    {
      unsigned long hashValue = hashFunc(key);
      HashNode<K, V> *entry = table[hashValue];

      while (entry != NULL) {
        if (entry->getKey() == key) {
          value = entry->getValue();
          return true;
        }

        entry = entry->getNext();
      }

      return false;
    }

    void put(const K &key, const V &value)
    {
      unsigned long hashValue = hashFunc(key);
      HashNode<K, V> *prev = NULL;
      HashNode<K, V> *entry = table[hashValue];

      while (entry != NULL && entry->getKey() != key) {
        prev = entry;
        entry = entry->getNext();
      }

      if (entry == NULL) {
        entry = new HashNode<K, V>(key, value);

        if (prev == NULL) {
          // insert as first bucket
          table[hashValue] = entry;

        } else {
          prev->setNext(entry);
        }

      } else {
        // just update the value
        entry->setValue(value);
      }
    }

    bool reverse(const V &value, K &key) {
      for (unsigned long i=0; i < tableSize; i++) {
        HashNode<K, V> *entry = table[i];
        while (entry != NULL && entry->getValue() != value) {
          entry = entry->getNext();
        }
        if (entry != NULL) {
          key = entry->getKey();
          return true;
        }
      }
      return false;
    }
    
    void remove(const K &key)
    {
      unsigned long hashValue = hashFunc(key);
      HashNode<K, V> *prev = NULL;
      HashNode<K, V> *entry = table[hashValue];

      while (entry != NULL && entry->getKey() != key) {
        prev = entry;
        entry = entry->getNext();
      }

      if (entry == NULL) {
        // key not found
        return;

      } else {
        if (prev == NULL) {
          // remove first bucket of the list
          table[hashValue] = entry->getNext();

        } else {
          prev->setNext(entry->getNext());
        }

        delete entry;
      }
    }

    private:
    HashMap(const HashMap & other);
    const HashMap & operator=(const HashMap & other);
    // hash table
    HashNode<K, V> *table[tableSize];
    F hashFunc;
    };
}

#endif

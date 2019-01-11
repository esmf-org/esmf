
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

#ifndef ESMCI_HASHNODE_H
#define ESMCI_HASHNODE_H

#include <cstddef>

namespace ESMCI {
// Hash node class template
template <typename K, typename V>
class HashNode
{
public:
    HashNode(const K &key, const V &value) :
        _key(key), _value(value), _next(NULL)
    {
    }

    K getKey() const
    {
        return _key;
    }

    V getValue() const
    {
        return _value;
    }

    void setValue(V value)
    {
        _value = value;
    }

    HashNode *getNext() const
    {
        return _next;
    }

    void setNext(HashNode *next)
    {
        _next = next;
    }

private:
    // key-value pair
    K _key;
    V _value;
    // next bucket with the same key
    HashNode *_next;
    // disallow copy and assignment
    HashNode(const HashNode &);
    HashNode & operator=(const HashNode &);
};
}

#endif

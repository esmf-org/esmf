from collections import namedtuple
import itertools


def itr_row(key, sequence):
    for element in sequence:
        yield ({key: element})


def iter_product_keywords(keywords, as_namedtuple=True):
    if as_namedtuple:
        yld_tuple = namedtuple('ITesterKeywords', keywords.keys())

    iterators = [itr_row(ki, vi) for ki, vi in keywords.items()]
    for dictionaries in itertools.product(*iterators):
        yld = {}
        for dictionary in dictionaries:
            yld.update(dictionary)
        if as_namedtuple:
            yld = yld_tuple(**yld)
        yield yld

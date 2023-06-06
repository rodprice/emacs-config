# SPDX-License-Identifier: GPL-3.0-or-later
"""
Complementing Python code for pyinspect.el!

The entry point is `_pyinspect_json`, and is the only function used by the elisp
module. The rest are helper functions called by `_pyinspect_json`
"""

import json
from inspect import getmembers, isbuiltin, ismethod
from typing import Dict

_PYINSPECT_ITEM_CAP = 5
_PYINSPECT_STR_CAP = 80


def _pyinspect_inspect_object(obj):
    """
    Turns a **non-primitive** obj into a dictionary of its fields and their values.
    Filters out some built-in magic fields and pretty-prints dictionary values via `json.dumps`.
    Doesn't display methods.
    """

    def stringify_val(member):
        key, val = member
        if isinstance(val, str):
            return key, '"{}"'.format(val)
        if type(val) in (dict, tuple, list):
            return key, _pyinspect_trim(val, _PYINSPECT_ITEM_CAP, _PYINSPECT_STR_CAP)
        return key, f"{str(val)} {str(type(val))}"

    def is_trash(member):
        key, val = member
        return (
            key in ["__doc__", "__class__", "__hash__", "__dict__"]
            or ismethod(val)
            or isbuiltin(val)
            or type(val).__name__ == "method-wrapper"
        )

    return dict(stringify_val(m) for m in reversed(getmembers(obj)) if not is_trash(m))


def _pyinspect_add_quotes(key):
    """
    Surrounds string key with extra quotes because Emacs parses them as just symbols
    and makes it hard to distinguish between them and non-string symbols

    >>> _pyinspect_add_quotes("hello")
    '"hello"'

    >>> _pyinspect_add_quotes(1)
    1
    """
    return '"{}"'.format(key) if type(key) is str else key


def _pyinspect_trim(obj, elem_cap, str_cap):
    """
    If obj is a sequence (dict/list/tuple), takes its first elem_cap elements and drops the rest.
    Also adds a cute ellipsis before the closing bracket to signal that it has been trimmed.
    Returns a pretty-printed string of the sequence, formatted by json.dumps with indent=4.

    If it's a string or any other kind of object, coerce it into a string and take the first
    str_cap characters. AND add a cute ellipsis.

    >>> _pyinspect_trim("abcde", elem_cap=3, str_cap=3)
    'abc...'

    >>> print(_pyinspect_trim([1, 2, 3, 4], elem_cap=3, str_cap=3))
    [
        1,
        2,
        3
        ...
    ]

    >>> print(_pyinspect_trim({x: x + 1 for x in range(10)}, elem_cap=3, str_cap=3))
    {
        "0": 1,
        "1": 2,
        "2": 3
        ...
    }
    """

    def trim_seq(seq):
        if type(seq) is dict:
            return _pyinspect_take_dict(seq, elem_cap)
        elif type(seq) in (tuple, list):
            return seq[:elem_cap]

    if type(obj) in (dict, tuple, list):
        jsondump = json.dumps(trim_seq(obj), indent=4)
        if len(obj) > elem_cap:
            return f"{jsondump[:-1]}    ...\n{jsondump[-1]}"
        return jsondump

    s = str(obj)
    return f"{s[:str_cap]}..." if len(s) > str_cap else s


def _pyinspect_take_dict(d: Dict, n: int):
    "Returns a new dictionary with the first n pairs from d"

    def iterator():
        i = 0
        for item in d.items():
            if i == n:
                break
            yield item
            i += 1

    return dict(iterator())


def _pyinspect(obj):
    "Dispatches the appropriate inspection according to obj type"

    if type(obj) in (str, bool, int, float, complex):
        return {"type": "primitive", "value": obj}

    elif type(obj) in (tuple, list):
        return {
            "type": "collection",
            "value": [
                _pyinspect_trim(item, _PYINSPECT_ITEM_CAP, _PYINSPECT_STR_CAP)
                for item in obj
            ],
        }

    elif type(obj) is dict:
        return {
            "type": "dict",
            "value": {
                _pyinspect_add_quotes(k): _pyinspect_trim(
                    v, _PYINSPECT_ITEM_CAP, _PYINSPECT_STR_CAP
                )
                for (k, v) in obj.items()
            },
        }

    else:
        return {"type": "object", "value": _pyinspect_inspect_object(obj)}


def _pyinspect_json(obj):
    print(
        json.dumps(_pyinspect(obj), indent=4, default=lambda o: _pyinspect(o)["value"])
    )

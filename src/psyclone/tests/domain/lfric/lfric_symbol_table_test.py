# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology

''' This module tests the LFric-specific SymbolTable.'''

import pytest

from psyclone.domain.lfric import psyir, LFRicSymbolTable
from psyclone.psyir.symbols import (ArrayType, DataSymbol, REAL_DOUBLE_TYPE,
                                    RoutineSymbol)


def test_find_or_create_integer():
    '''Checks the find_or_create_integer convenience function. '''
    symbol_table = LFRicSymbolTable()
    sym_i = symbol_table.find_or_create_integer_symbol("i")
    assert isinstance(sym_i, DataSymbol)
    assert sym_i.name == "i"
    # pylint: disable=no-member
    assert sym_i.datatype == psyir.LfricIntegerScalarDataType()

    # Make sure the symbol exists without a tag:
    sym_i2 = symbol_table.lookup("i")
    assert sym_i2 is sym_i
    with pytest.raises(KeyError) as err:
        symbol_table.lookup_with_tag("i")
    assert "Could not find the tag 'i' in the Symbol Table" in str(err.value)

    # If we call the same function again, we must get the same symbol.
    sym_i2 = symbol_table.find_or_create_integer_symbol("i")
    assert sym_i2 is sym_i

    # Now create a variable with the same name, but a tag. In this case the
    # name of the created symbol must be different:
    sym_i_tag = symbol_table.find_or_create_integer_symbol("i", tag="i")
    assert sym_i_tag is not sym_i
    assert sym_i_tag.name != sym_i.name

    sym_i_tag2 = symbol_table.lookup_with_tag("i")
    assert sym_i_tag2 is sym_i_tag


def test_find_or_create_integer_errors():
    '''Tests various error conditions of find_or_create_integer_symbol.'''

    symbol_table = LFRicSymbolTable()
    routine = RoutineSymbol('routine')
    symbol_table.add(routine)

    with pytest.raises(TypeError) as err:
        symbol_table.find_or_create_integer_symbol("routine")
    assert ("Symbol 'routine' already exists, but is not a DataSymbol"
            in str(err.value))

    symbol_table.new_symbol("real", symbol_type=DataSymbol,
                            datatype=REAL_DOUBLE_TYPE)
    with pytest.raises(TypeError) as err:
        symbol_table.find_or_create_integer_symbol("real")
    assert ("Symbol 'real' already exists, but is not an integer"
            in str(err.value))


@pytest.mark.parametrize("intrinsic", ["real", "integer", "logical"])
def test_find_or_create_array_intrinsic_types(intrinsic):
    '''Checks that the find_or_create_array convenience function
    handles all intrinsic types. '''
    symbol_table = LFRicSymbolTable()
    arr = symbol_table.find_or_create_array("arr", 2, intrinsic)

    assert arr.name == "arr"
    assert isinstance(arr, DataSymbol)
    assert isinstance(arr.datatype, ArrayType)
    # pylint: disable=no-member
    if intrinsic == "real":
        assert arr.datatype._datatype == psyir.LfricRealScalarDataType()
    elif intrinsic == "integer":
        assert arr.datatype._datatype == psyir.LfricIntegerScalarDataType()
    elif intrinsic == "logical":
        assert arr.datatype._datatype == psyir.LfricLogicalScalarDataType()

    arr_queried = symbol_table.find_or_create_array("arr", 2, intrinsic)
    assert arr_queried is arr
    assert len(arr_queried.shape) == 2


def test_find_or_create_array_tags():
    '''Checks that the find_or_create_array convenience function
    handles tags as expected.'''

    symbol_table = LFRicSymbolTable()
    arr = symbol_table.find_or_create_array("arr", 2, "real")
    assert arr.name == "arr"

    # Using a tag with the same name should give a different name:
    arr2 = symbol_table.find_or_create_array("arr", 2, "real", tag="arr")
    assert arr2.name != "arr"


def test_find_or_create_array_errors():
    '''Checks that the find_or_create_array convenience function
    handles incorrect parameters as expected.'''
    symbol_table = LFRicSymbolTable()
    with pytest.raises(TypeError) as err:
        symbol_table.find_or_create_array("arr", 2, "invalid")
    assert ("Unsupported data type 'invalid' in find_or_create_array"
            in str(err.value))

    symbol_table.add(RoutineSymbol('routine'))
    with pytest.raises(TypeError) as err:
        symbol_table.find_or_create_array("routine", 2, "real")
    assert ("Symbol 'routine' already exists, but is not a DataSymbol"
            in str(err.value))

    # Test clash with an existing integer symbol:
    symbol_table.find_or_create_integer_symbol("a_2d")
    with pytest.raises(TypeError) as err:
        symbol_table.find_or_create_array("a_2d", 2, "real")
    assert ("Symbol 'a_2d' already exists, but is not an ArraySymbol" in
            str(err.value))

    symbol_table.find_or_create_array("int_2d", 2, "integer")
    with pytest.raises(TypeError) as err:
        symbol_table.find_or_create_array("int_2d", 2, "real")
    assert ("Symbol 'int_2d' already exists, but is not of type real, but"
            in str(err.value))

    symbol_table.find_or_create_array("a_3d", 3, "real")
    with pytest.raises(TypeError) as err:
        symbol_table.find_or_create_array("a_3d", 2, "real")
    assert ("Array 'a_3d' already exists, but has 3 dimensions, not 2."
            in str(err.value))

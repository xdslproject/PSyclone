# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the StructureReference class. '''

import pytest
from psyclone.errors import GenerationError
from psyclone.psyir import symbols, nodes
from psyclone.tests.utilities import check_links
from psyclone.core.access_info import VariablesAccessInfo


def test_struc_ref_create():
    ''' Tests for the create method. '''
    region_type = symbols.StructureType.create([
        ("startx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    region_type_symbol = symbols.TypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC),
        ("region", region_type_symbol, symbols.Symbol.Visibility.PRIVATE),
        ("sub_grids", symbols.ArrayType(region_type_symbol, [3]),
         symbols.Symbol.Visibility.PUBLIC),
        ("data", symbols.ArrayType(symbols.REAL_TYPE, [10, 10]),
         symbols.Symbol.Visibility.PUBLIC)])
    grid_type_symbol = symbols.TypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    assert sref.symbol is ssym
    assert len(sref.children) == 1
    assert sref.children[0].name == "nx"
    check_links(sref, sref.children)
    # Reference to scalar member of structure member of structure
    rref = nodes.StructureReference.create(ssym, ["region", "startx"])
    assert rref.children[0].name == "region"
    assert rref.children[0].children[0].name == "startx"
    check_links(rref.children[0], rref.children[0].children)
    # Reference to an element of an array member of the structure
    aref = nodes.StructureReference.create(
        ssym,
        [("data", [nodes.Literal("1", symbols.INTEGER_TYPE),
                   nodes.Literal("5", symbols.INTEGER_TYPE)])])
    assert isinstance(aref.children[0], nodes.ArrayMember)
    assert aref.children[0].name == "data"
    assert isinstance(aref.children[0].children[1], nodes.Literal)
    assert aref.children[0].children[1].value == "5"
    check_links(aref, aref.children)
    check_links(aref.children[0], aref.children[0].children)
    # Reference to a scalar member of an element of an array of structures
    # contained in a structure
    dref = nodes.StructureReference.create(
        ssym,
        [("sub_grids", [nodes.Literal("2", symbols.INTEGER_TYPE)]), "startx"])
    assert isinstance(dref.children[0], nodes.ArrayOfStructuresMember)
    assert isinstance(dref.children[0].children[0], nodes.Member)
    assert isinstance(dref.children[0].children[1], nodes.Literal)
    check_links(dref, dref.children)
    check_links(dref.children[0], dref.children[0].children)


def test_struc_ref_create_errors():
    ''' Tests for the validation checks in the create method. '''
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(None, [])
    assert ("'symbol' argument to StructureReference.create() should be a "
            "DataSymbol but found 'NoneType'" in str(err.value))
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("fake", symbols.INTEGER_TYPE), [])
    assert ("symbol that is (or could be) a structure, however symbol "
            "'fake' has type 'Scalar" in str(err.value))
    with pytest.raises(NotImplementedError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("fake", symbols.DeferredType()), [])
    assert ("referenced must have a TypeSymbol as its type but 'fake' has "
            "type 'DeferredType'" in str(err.value))
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", symbols.DeferredType()), 1)
    assert ("'members' argument to StructureReference._create() should be a "
            "list but found 'int'" in str(err.value))
    tsymbol_unknown = symbols.TypeSymbol("grid_type", symbols.DeferredType())
    with pytest.raises(NotImplementedError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", tsymbol_unknown), ["missing"])
    assert ("TypeSymbol 'grid_type' for symbol 'grid' must have a defined "
            "StructureType" in str(err.value))
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    tsymbol_known = symbols.TypeSymbol("grid_type", grid_type)
    with pytest.raises(TypeError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", tsymbol_known), [1])
    assert ("'members' passed to StructureType._create() must consist of "
            "either 'str' or 2-tuple entries but found 'int' while "
            "attempting to create reference to symbol 'grid'" in
            str(err.value))
    with pytest.raises(GenerationError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", tsymbol_known), ["missing"])
    assert ("The type definition for symbol 'grid' does not contain a "
            "member named 'missing'" in str(err.value))
    # Reference to a member of a struct that does not have a supported type
    grid_type = symbols.StructureType.create([
        ("nx", symbols.DeferredType(), symbols.Symbol.Visibility.PUBLIC)])
    tsymbol_known = symbols.TypeSymbol("grid_type", grid_type)
    with pytest.raises(NotImplementedError) as err:
        _ = nodes.StructureReference.create(
            symbols.DataSymbol("grid", tsymbol_known), ["nx"])
    assert "member 'nx' of 'grid' is of type 'DeferredType'" in str(err.value)


def test_struc_ref_validate_child():
    ''' Tests for the _validate_child method. '''
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    grid_type_symbol = symbols.TypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    # a StructureReference is only allowed (at most) one child.
    with pytest.raises(GenerationError) as err:
        sref.addchild("wrong")
    assert ("Item 'str' can't be child 1 of 'StructureReference'" in
            str(err.value))
    # If present, the first child has to be a MemberReference
    with pytest.raises(GenerationError) as err:
        sref.children[0] = "wrong"
    assert ("Item 'str' can't be child 0 of 'StructureReference'" in
            str(err.value))


def test_struc_ref_str():
    ''' Test the __str__ method of StructureReference. '''
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    grid_type_symbol = symbols.TypeSymbol("grid_type", grid_type)
    ssym = symbols.DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    sref = nodes.StructureReference.create(ssym, ["nx"])
    assert (str(sref) == "StructureReference[name:'grid']\n"
            "Member[name:'nx']\n")


def test_reference_accesses():
    ''' Test that the reference_accesses method raises a
    NotImplementedError. This will be addressed by #1028. '''
    var_access_info = VariablesAccessInfo()
    dref = nodes.StructureReference(
        symbols.DataSymbol(
            "grid",
            symbols.TypeSymbol("grid_type", symbols.DeferredType())))
    with pytest.raises(NotImplementedError) as err:
        dref.reference_accesses(var_access_info)
    assert ("Dependency analysis has not yet been implemented for "
            "Structures." in str(err.value))

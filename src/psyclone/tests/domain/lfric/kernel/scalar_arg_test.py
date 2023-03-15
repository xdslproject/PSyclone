# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the ScalarArg class.

'''
import pytest

from psyclone.domain.lfric.kernel.scalar_arg import ScalarArg


def test_init_noargs():
    '''Test that a ScalarArg instance can be created successfully when no
    arguments are provided.

    '''
    field_arg = ScalarArg()
    assert isinstance(field_arg, ScalarArg)
    assert field_arg.form == "GH_SCALAR"
    assert field_arg._datatype is None
    assert field_arg._access is None


def test_init_invalid():
    '''Test that appropriate exceptions are raised if invalid initial
    values are provided when constructing an instance of the ScalarArg
    class.

    '''
    with pytest.raises(ValueError) as info:
        _ = ScalarArg(datatype="invalid")
    assert ("The datatype descriptor metadata for a scalar should be one of "
            "['gh_real', 'gh_integer', 'gh_logical'], but found 'invalid'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = ScalarArg(access="invalid")
    assert ("The access descriptor metadata for a scalar should be one of "
            "['gh_read'], but found 'invalid'." in str(info.value))


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of ScalarArg are stored as expected.

    '''
    field_arg = ScalarArg("GH_REAL", "GH_READ")
    assert field_arg.form == "GH_SCALAR"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(ValueError) as info:
        _ = ScalarArg.create_from_fortran_string("not valid")
    assert ("Expected kernel metadata to be a Fortran Part_Ref, "
            "with the form 'arg_type(...)' but found 'not valid'."
            in str(info.value))

    fortran_string = "arg_type(GH_SCALAR, GH_REAL, GH_READ)"
    field_arg = ScalarArg.create_from_fortran_string(fortran_string)
    assert field_arg.form == "GH_SCALAR"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(TypeError) as info:
        _ = ScalarArg.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = ScalarArg.create_fparser2("hello(x)")
    with pytest.raises(ValueError) as info:
        _ = ScalarArg.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have the name 'arg_type' "
            "and be in the form 'arg_type(...)', but found 'hello(x)'."
            in str(info.value))

    fparser2_tree = ScalarArg.create_fparser2("arg_type(x)")
    with pytest.raises(ValueError) as info:
        _ = ScalarArg.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have 3 arguments, but "
            "found 1 in 'arg_type(x)'." in str(info.value))

    fparser2_tree = ScalarArg.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ)")
    with pytest.raises(ValueError) as info:
        _ = ScalarArg.create_from_fparser2(fparser2_tree)
    assert ("Scalars should have GH_SCALAR as their first metadata argument, "
            "but found 'GH_FIELD'." in str(info.value))

    fparser2_tree = ScalarArg.create_fparser2(
        "arg_type(GH_SCALAR, GH_UNREAL, GH_READ)")
    with pytest.raises(ValueError) as info:
        _ = ScalarArg.create_from_fparser2(fparser2_tree)
    assert ("At argument index '1' for metadata 'arg_type(GH_SCALAR, "
            "GH_UNREAL, GH_READ)'. The datatype descriptor metadata for a "
            "scalar should be one of ['gh_real', 'gh_integer', "
            "'gh_logical'], but found 'GH_UNREAL'." in str(info.value))

    fparser2_tree = ScalarArg.create_fparser2(
        "arg_type(GH_SCALAR, GH_REAL, GH_ERROR)")
    with pytest.raises(ValueError) as info:
        _ = ScalarArg.create_from_fparser2(fparser2_tree)
    assert ("At argument index '2' for metadata 'arg_type(GH_SCALAR, "
            "GH_REAL, GH_ERROR)'. The access descriptor metadata for a "
            "scalar should be one of ['gh_read'], but found 'GH_ERROR'."
            in str(info.value))

    fparser2_tree = ScalarArg.create_fparser2(
        "arg_type(GH_SCALAR, GH_REAL, GH_READ)")
    field_arg = ScalarArg.create_from_fparser2(fparser2_tree)
    assert field_arg.form == "GH_SCALAR"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"


def test_fortran_string():
    '''Test that the fortran_string method works as expected, including
    raise an exception if all of the required properties have not been
    set '''
    fortran_string = "arg_type(GH_SCALAR, GH_REAL, GH_READ)"
    field_arg = ScalarArg.create_from_fortran_string(fortran_string)
    result = field_arg.fortran_string()
    assert result == fortran_string

    field_arg = ScalarArg()
    with pytest.raises(ValueError) as info:
        _ = field_arg.fortran_string()
    assert ("Values for datatype and access must be provided "
            "before calling the fortran_string method, but found 'None' "
            "and 'None', respectively." in str(info.value))


def test_setter_getter():
    '''Test that the setters and getters work as expected, including
    raising exceptions if values are invalid. '''
    field_arg = ScalarArg()
    assert field_arg.form == "GH_SCALAR"

    assert field_arg.datatype is None
    with pytest.raises(ValueError) as info:
        field_arg.datatype = "invalid"
    assert ("The datatype descriptor metadata for a scalar should be one of "
            "['gh_real', 'gh_integer', 'gh_logical'], but found 'invalid'."
            in str(info.value))

    field_arg.datatype = "gh_integer"
    assert field_arg.datatype == "gh_integer"
    field_arg.datatype = "GH_INTEGER"
    assert field_arg.datatype == "GH_INTEGER"

    assert field_arg.access is None
    with pytest.raises(ValueError) as info:
        field_arg.access = "invalid"
    assert ("The access descriptor metadata for a scalar should be one of "
            "['gh_read'], but found 'invalid'." in str(info.value))

    field_arg.access = "gh_read"
    assert field_arg.access == "gh_read"
    field_arg.access = "GH_READ"
    assert field_arg.access == "GH_READ"
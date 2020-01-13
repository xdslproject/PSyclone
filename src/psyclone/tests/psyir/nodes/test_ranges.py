# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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


''' pytest tests for the various Range sub-classes of Node. '''

import pytest
from psyclone.psyir.symbols import DataType
from psyclone.psyir.nodes.ranges import ExplicitRange
from psyclone.psyGen import InternalError, Literal

@pytest.mark.parametrize("prop", ["start", "stop", "step"])
def test_explicit_range_getter_errors(prop):
    ''' Test that attempting to get any of the start/stop/step properties for
    an invalid ExplicitRange object raises the expected errors. '''
    # pylint:disable=eval-used
    erange = ExplicitRange()
    # Manually break the list of children
    erange._children = []
    with pytest.raises(InternalError) as err:
        _ = eval("erange." + prop)
    assert ("Malformed ExplicitRange: should have three children but found 0"
            in str(err.value))

    # Correct number of children but wrong type (as initialised to None)
    erange = ExplicitRange()
    with pytest.raises(InternalError) as err:
        _ = eval("erange." + prop)
    assert ("Malformed ExplicitRange: all children must be sub-classes of "
            "Node" in str(err.value))


@pytest.mark.parametrize("prop", ["start", "stop", "step"])
def test_explicit_range_setter_errors(prop):
    ''' Check that the various setters reject values that are not sub-classes
    of Node. '''
    # pylint:disable=exec-used
    erange = ExplicitRange()
    with pytest.raises(TypeError) as err:
        exec("erange." + prop + " = 1")
    assert "must be a sub-class of Node but got" in str(err.value)
    val = Literal("1.0", DataType.REAL)
    with pytest.raises(TypeError) as err:
        exec("erange." + prop + " = val")
    assert ("value of an ExplicitRange is a Literal then it must be of type "
            "INTEGER but got DataType.REAL" in str(err.value))


def test_explicit_range_props():
    ''' Test that the properties of an ExplicitRange return what we expect. '''
    start = Literal("10", DataType.INTEGER)
    stop = Literal("20", DataType.INTEGER)
    erange = ExplicitRange.create(start, stop)
    assert erange.children[0] is start
    assert erange.children[1] is stop
    # We didn't supply an increment so check that one was created
    assert isinstance(erange.children[2], Literal)
    assert erange.children[2].datatype == DataType.INTEGER
    assert erange.children[2].value == "1"

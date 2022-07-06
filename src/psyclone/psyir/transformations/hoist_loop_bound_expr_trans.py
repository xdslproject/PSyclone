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
# Author: S. Siso, STFC Daresbury Lab

'''This module contains the HoistLoopBoundExprTrans transformation. This
transformation moves complex bounds expressions out of the loop construct and
places them in integer scalar assignments before the loop.

'''

from psyclone.psyir.nodes import Routine, Literal, Reference, \
    StructureReference, Assignment
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations.loop_trans import LoopTrans, \
    TransformationError


class HoistLoopBoundExprTrans(LoopTrans):
    '''This transformation moves complex bounds expressions out of the loop
    construct and places them in integer scalar assignments before the loop.

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import HoistTrans
    >>> code = ("program test\\n"
    ...         "  use mymod, only: mytype\\n"
    ...         "  integer :: i,j,n\\n"
    ...         "  real :: a(n)\\n"
    ...         "  do i=mytype%start, UBOUND(a,1)\\n"
    ...         "    a(i) = 1.0\\n"
    ...         "  end do\\n"
    ...         "end program\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> hoist = HoistLoopBoundExprTrans()
    >>> hoist.apply(psyir.walk(Loop)[0])
    >>> print(FortranWriter()(psyir))
    program test
      integer :: i
      integer :: j
      integer :: n
      real, dimension(n,n) :: a
      real :: value
    <BLANKLINE>
      value = 1.0
      do i = 1, n, 1
        do j = 1, n, 1
          a(i,j) = value
        enddo
      enddo
    <BLANKLINE>
    end program test
    <BLANKLINE>

    '''
    def apply(self, node, options=None):
        '''Move complex bounds expressions out of the given loop construct and
        place them in integer scalar assignments before the loop.

        :param node: target PSyIR loop.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node, options)

        parent = node.parent
        position = node.position

        for bound in node.children[0:2]:

            # Skip Literals and non-structure references. Maybe it could be
            # more selective by allowing arithmetic expressions
            if isinstance(bound, Literal):
                continue
            if isinstance(bound, Reference) and not \
                    isinstance(bound, StructureReference):
                continue

            # Create new symbol
            symbol = parent.scope.symbol_table.new_symbol(
                "loop_bound", symbol_type=DataSymbol, datatype=INTEGER_TYPE
            )
            # Move bound expression to an assignment preceding the loop
            bound.replace_with(Reference(symbol))
            parent.addchild(Assignment.create(Reference(symbol), bound),
                            position)

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for the
        transformation.

        :param node: target PSyIR loop.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node does not have an \
            ancestor Routine where to hoist the bound expressions to.

        '''
        super().validate(node)

        # The node should be an assignment
        if not node.ancestor(Routine):
            raise TransformationError(
                "The loop provided to HoistLoopBoundExprTrans must belong to"
                " a Routine where to place the hoisted expressions.")

    def __str__(self):
        return ("Hoist complex loop bound expressions outside the loop "
                "construct")


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistLoopBoundExprTrans"]

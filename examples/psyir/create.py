# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modifications: A. R. Porter, STFC Daresbury Lab

'''A simple Python script showing how to create a PSyIR tree using the
create methods. In order to use it you must first install
PSyclone. See README.md in the top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing:

>>> python create.py

This should output a Fortran representation of the PSyIR and part of a
C representation of the PSyIR.

'''
from __future__ import print_function
from psyclone.psyir.nodes import Reference, Literal, UnaryOperation, \
    BinaryOperation, NaryOperation, Assignment, IfBlock, Loop, \
    Container, Range, ArrayReference, Call, KernelSchedule
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, SymbolTable, \
    ContainerSymbol, ArgumentInterface, ScalarType, ArrayType, \
    GlobalInterface, REAL_TYPE, REAL4_TYPE, REAL_DOUBLE_TYPE, INTEGER_TYPE, \
    INTEGER_SINGLE_TYPE, INTEGER4_TYPE, INTEGER8_TYPE
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.c import CWriter


# pylint: disable=too-many-locals
def create_psyir_tree():
    ''' Create an example PSyIR Tree.

    :returns: an example PSyIR tree.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    '''
    # Symbol table, symbols and scalar datatypes
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        symbol_type=DataSymbol, datatype=REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    symbol_table.specify_argument_list([arg1])
    tmp_symbol = symbol_table.new_symbol(symbol_type=DataSymbol,
                                         datatype=REAL_DOUBLE_TYPE)
    index_symbol = symbol_table.new_symbol(root_name="i",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE)
    real_kind = symbol_table.new_symbol(root_name="RKIND",
                                        symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE,
                                        constant_value=8)
    routine_symbol = RoutineSymbol("my_sub")

    # Array using precision defined by another symbol
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, real_kind)
    array = symbol_table.new_symbol(root_name="a", symbol_type=DataSymbol,
                                    datatype=ArrayType(scalar_type, [10]))

    # Nodes which do not have Nodes as children and (some) predefined
    # scalar datatypes
    zero = Literal("0.0", REAL_TYPE)
    one = Literal("1.0", REAL4_TYPE)
    two = Literal("2.0", scalar_type)
    int_zero = Literal("0", INTEGER_SINGLE_TYPE)
    int_one = Literal("1", INTEGER8_TYPE)
    tmp1 = Reference(arg1)
    tmp2 = Reference(tmp_symbol)

    # Unary Operation
    oper = UnaryOperation.Operator.SIN
    unaryoperation = UnaryOperation.create(oper, tmp2)

    # Binary Operation
    oper = BinaryOperation.Operator.ADD
    binaryoperation = BinaryOperation.create(oper, one.copy(), unaryoperation)

    # Nary Operation
    oper = NaryOperation.Operator.MAX
    naryoperation = NaryOperation.create(oper, [tmp1, tmp2.copy(), one.copy()])

    # Array reference using a range
    lbound = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                    Reference(array), int_one)
    ubound = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                    Reference(array), int_one.copy())
    my_range = Range.create(lbound, ubound)
    tmparray = ArrayReference.create(array, [my_range])

    # Assignments
    assign1 = Assignment.create(tmp1.copy(), zero.copy())
    assign2 = Assignment.create(tmp2.copy(), zero.copy())
    assign3 = Assignment.create(tmp2.copy(), binaryoperation)
    assign4 = Assignment.create(tmp1.copy(), tmp2.copy())
    assign5 = Assignment.create(tmp1.copy(), naryoperation)
    assign6 = Assignment.create(tmparray, two.copy())

    # Call
    call = Call.create(routine_symbol, [tmp1.copy(), binaryoperation.copy()])

    # If statement
    if_condition = BinaryOperation.create(BinaryOperation.Operator.GT,
                                          tmp1.copy(), zero.copy())
    ifblock = IfBlock.create(if_condition, [assign3, assign4])

    # Loop
    loop = Loop.create(index_symbol, int_zero, int_one.copy(), int_one.copy(),
                       [ifblock])

    # KernelSchedule
    kernel_schedule = KernelSchedule.create(
        "work", symbol_table, [assign1, call, assign2, loop, assign5, assign6])

    # Container
    container_symbol_table = SymbolTable()
    container = Container.create("CONTAINER", container_symbol_table,
                                 [kernel_schedule])

    # Import data from another container
    external_container = ContainerSymbol("some_mod")
    container_symbol_table.add(external_container)
    external_var = DataSymbol("some_var", INTEGER_TYPE,
                              interface=GlobalInterface(external_container))
    container_symbol_table.add(external_var)
    routine_symbol.interface = GlobalInterface(external_container)
    container_symbol_table.add(routine_symbol)
    return container


if __name__ == "__main__":
    psyir_tree = create_psyir_tree()

    # Write out the code as Fortran
    writer = FortranWriter()
    result = writer(psyir_tree)
    print(result)

    # Write out the code as C. At the moment NaryOperator, KernelSchedule
    # and Container are not supported in the C backend so the full example
    # can't be output.
    writer = CWriter()
    result = writer(psyir_tree.children[0].children[3])
    print(result)
